# Assemble one modeling frame with HONEST source labels and HONEST exogenous
# column names (REFACTOR.md Exp 4), replacing the old nhsn/nssp two-target split
# + spoofing.
#
# Same slice/reshape ops as today; only the labels change and the exogenous
# left_join moves here (out of the forecaster). The forecaster is told which
# source is primary (primary_source) instead of hardcoding "nhsn", and reads its
# exogenous columns by their real names instead of a spoofed "nssp".
#
# Reveals the one real asymmetry the spoof was hiding: an exogenous nssp column
# is the RAW slice (cut at the forecast date), while an exogenous nhsn column is
# the primary nhsn series (time-shifted). Preserved verbatim; flagged, not fixed.

g_insufficient_data_geos_default <- c("as", "mp", "vi", "gu")

# nhsn training frame + joined extra data; the primary rows for the nhsn signal.
flu_build_full_data <- function(nhsn_archive, version_policy, generation_date, forecast_date,
                                flu_data_substitutions, joined_latest_extra_data,
                                insufficient_data_geos = g_insufficient_data_geos_default) {
  train_data <- flu_slice_archive(nhsn_archive, version_policy, generation_date)
  train_data <- train_data %>%
    add_season_info() %>%
    mutate(
      geo_value = ifelse(geo_value == "usa", "us", geo_value),
      time_value = time_value - 3,
      source = "nhsn"
    )
  if (version_policy != "latest") {
    train_data <- train_data %>%
      data_substitutions(flu_data_substitutions, as.Date(generation_date))
  }
  train_data <- train_data %>% filter(geo_value %nin% insufficient_data_geos)
  attributes(train_data)$metadata$as_of <- as.Date(forecast_date)
  full_data <- train_data %>% bind_rows(joined_latest_extra_data)
  attributes(full_data)$metadata$other_keys <- "source"
  attributes(full_data)$metadata$as_of <- as.Date(forecast_date)
  full_data
}

# Pull the archive inputs from a built targets store (default: flu_hosp_prod).
# Dev convenience for iterating on a forecaster in the console against the same
# inputs the pipeline uses: arch <- flu_load_archives(); flu_assemble(arch, ...).
flu_load_archives <- function(project = "flu_hosp_prod") {
  withr::with_envvar(c(TAR_PROJECT = project), {
    list(
      nhsn = targets::tar_read(nhsn_archive_data),
      nssp = targets::tar_read(nssp_archive_data),
      joined_latest_extra_data = targets::tar_read(joined_latest_extra_data),
      flu_data_substitutions = targets::tar_read(flu_data_substitutions)
    )
  })
}

# A signal as the primary outcome frame (honest source label; nhsn carries its
# augmentation rows, nssp does not -- matching current behavior).
flu_primary_frame <- function(archives, signal, version_policy, generation_date, forecast_date,
                              insufficient_data_geos = g_insufficient_data_geos_default) {
  if (signal == "nhsn") {
    flu_build_full_data(
      archives$nhsn, version_policy, generation_date, forecast_date,
      archives$flu_data_substitutions, archives$joined_latest_extra_data, insufficient_data_geos
    )
  } else if (signal == "nssp") {
    nssp <- flu_slice_archive(archives$nssp, version_policy, generation_date)
    nssp <- nssp %>%
      rename(value = nssp) %>%
      mutate(time_value = floor_date(time_value, "week", week_start = 7) + 3) %>%
      mutate(source = "nssp") %>% # honest (was spoofed "nhsn")
      add_season_info()
    attributes(nssp)$metadata$as_of <- as.Date(forecast_date)
    attributes(nssp)$metadata$other_keys <- "source"
    nssp
  } else {
    stop("unknown signal: ", signal)
  }
}

# A signal as an exogenous column, named by the signal (honest).
flu_exogenous_column <- function(archives, signal, version_policy, generation_date, forecast_date,
                                 insufficient_data_geos = g_insufficient_data_geos_default) {
  if (signal == "nssp") {
    flu_slice_archive(archives$nssp, version_policy, generation_date, latest_cutoff = forecast_date) %>%
      transmute(geo_value, time_value, nssp)
  } else if (signal == "nhsn") {
    flu_build_full_data(
      archives$nhsn, version_policy, generation_date, forecast_date,
      archives$flu_data_substitutions, archives$joined_latest_extra_data, insufficient_data_geos
    ) %>%
      filter(source == "nhsn") %>%
      transmute(geo_value, time_value, nhsn = value)
  } else {
    stop("unknown exogenous signal: ", signal)
  }
}

# The one call: resolve (outcome, exogenous) into a modeling frame.
flu_assemble <- function(archives, outcome_signal, exogenous = character(),
                         version_policy = "as_of", generation_date, forecast_date,
                         insufficient_data_geos = g_insufficient_data_geos_default) {
  frame <- flu_primary_frame(
    archives, outcome_signal, version_policy, generation_date, forecast_date, insufficient_data_geos
  )
  for (sig in exogenous) {
    exo <- flu_exogenous_column(
      archives, sig, version_policy, generation_date, forecast_date, insufficient_data_geos
    )
    frame <- frame %>% left_join(exo, by = join_by(geo_value, time_value))
  }
  frame
}

# Signal-agnostic seasonal forecaster: same as g_flu_windowed_seasonal_extra_sources
# but the exogenous join happened in flu_assemble, and primary_source / extra_sources
# are parameters instead of the hardcoded "nhsn" / "nssp".
flu_seasonal_extra <- function(epi_data, ahead, extra_sources, primary_source) {
  scaled_pop_seasonal(
    epi_data,
    outcome = "value",
    ahead = ahead * 7,
    extra_sources = extra_sources,
    seasonal_method = "window",
    trainer = epipredict::quantile_reg(),
    drop_non_seasons = TRUE,
    pop_scaling = FALSE,
    lags = list(c(0, 7), c(0, 7)),
    keys_to_ignore = g_very_latent_locations,
    primary_source = primary_source
  ) %>%
    select(-source) %>%
    mutate(target_end_date = target_end_date + 3) %>%
    filter(geo_value %nin% c("mo", "wy"))
}


# ---- prod2 forecaster adapters: uniform (epi_data, ahead, extra_sources, primary_source) ----
# Non-seasonal forecasters read a single `value` column and ignore
# extra_sources/primary_source; they're wrapped only to share one call signature.
flu2_cdc_baseline <- function(epi_data, ahead, extra_sources, primary_source) {
  g_baseline_forecaster(epi_data, ahead)
}
flu2_linear <- function(epi_data, ahead, extra_sources, primary_source) {
  g_flu_linear(epi_data, ahead)
}
flu2_linear_no_population_scale <- function(epi_data, ahead, extra_sources, primary_source) {
  g_flu_linear_no_population_scale(epi_data, ahead)
}
flu2_climate_base <- function(epi_data, ahead, extra_sources, primary_source) {
  g_flu_climate_base(epi_data, ahead)
}
flu2_climate_geo_agged <- function(epi_data, ahead, extra_sources, primary_source) {
  g_flu_climate_geo_agged(epi_data, ahead)
}
# Plain seasonal: primary_source replaces the hardcoded "nhsn"; no exogenous.
flu2_windowed_seasonal <- function(epi_data, ahead, extra_sources, primary_source) {
  scaled_pop_seasonal(
    epi_data,
    outcome = "value",
    ahead = ahead * 7,
    trainer = epipredict::quantile_reg(),
    seasonal_method = "window",
    pop_scaling = FALSE,
    lags = c(0, 7),
    keys_to_ignore = g_very_latent_locations,
    primary_source = primary_source
  ) %>%
    mutate(target_end_date = target_end_date + 3)
}
flu2_windowed_seasonal_extra_sources <- flu_seasonal_extra

# The signal-expanded grid: each forecaster x {nhsn, nssp} outcome. exogenous is
# the OTHER signal, but only for the extra-sources forecasters; primary_source is
# always the outcome signal. Replaces the two-target (forecast_nhsn/forecast_nssp)
# split -- signal is now a grid dimension, not a spoof.
flu_build_prod2_grid <- function() {
  spec <- tibble::tribble(
    ~id, ~forecaster, ~version_policy, ~uses_extra,
    "cdc_baseline", "flu2_cdc_baseline", "as_of", FALSE,
    "linear", "flu2_linear", "as_of", FALSE,
    "linear_no_population_scale", "flu2_linear_no_population_scale", "as_of", FALSE,
    "windowed_seasonal", "flu2_windowed_seasonal", "as_of", FALSE,
    "windowed_seasonal_extra_sources", "flu2_windowed_seasonal_extra_sources", "as_of", TRUE,
    "climate_base", "flu2_climate_base", "as_of", FALSE,
    "climate_geo_agged", "flu2_climate_geo_agged", "as_of", FALSE,
    "seasonal_nssp_latest", "flu2_windowed_seasonal_extra_sources", "latest", TRUE
  )
  tidyr::expand_grid(spec, outcome_signal = c("nhsn", "nssp")) %>%
    mutate(
      exogenous = purrr::pmap(list(.data$uses_extra, .data$outcome_signal), function(ue, sig) {
        if (!ue) character() else if (sig == "nhsn") "nssp" else "nhsn"
      }),
      primary_source = .data$outcome_signal,
      # Reporting transform: how to take model-unit output to SUBMISSION units and
      # what CDC target to report it under. Applied only at the reporting boundary
      # (submission / scoring / external comparison), NOT for plotting, and separate
      # from any internal pop/quantile normalization the forecaster does itself.
      scale = dplyr::if_else(.data$outcome_signal == "nssp", 0.01, 1),
      target_name = dplyr::if_else(
        .data$outcome_signal == "nssp", "wk inc flu prop ed visits", "wk inc flu hosp"
      )
    ) %>%
    select(id, forecaster, version_policy, outcome_signal, exogenous, primary_source, scale, target_name)
}

# Per-signal reporting transform, read at the submission/scoring/external boundary.
# Single source of truth = the grid. (Not used for plotting or inside forecasters.)
flu_report_scale <- function(signal) {
  g <- flu_build_prod2_grid()
  g$scale[match(signal, g$outcome_signal)]
}
flu_report_target <- function(signal) {
  g <- flu_build_prod2_grid()
  g$target_name[match(signal, g$outcome_signal)]
}
