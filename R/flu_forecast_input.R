# SPIKE (REFACTOR.md Exp 5): make a forecaster's input reproducible in one call.
#
# The batch runner and the console both build forecaster input through these
# functions, so `my_forecaster(inp$epi_data, ahead, extra_data = inp$extra_data)`
# in the REPL is byte-identical to what the pipeline feeds. No separate scratch
# pipeline, nothing to reintegrate.
#
# The builders below are the exact bodies previously inlined in the full_data /
# forecast_nssp / forecast_nhsn targets (_flu_prod_shared.R:171-245).

g_insufficient_data_geos_default <- c("as", "mp", "vi", "gu")

# nhsn training frame + joined extra data; the epi_data for the nhsn signal.
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

# reshaped nssp frame; the epi_data for the nssp signal.
flu_build_nssp_data <- function(nssp_archive, version_policy, generation_date, forecast_date) {
  nssp_data <- flu_slice_archive(nssp_archive, version_policy, generation_date)
  nssp_data <- nssp_data %>%
    rename(value = nssp) %>%
    mutate(time_value = floor_date(time_value, "week", week_start = 7) + 3) %>%
    mutate(source = "nhsn") %>%
    add_season_info()
  attributes(nssp_data)$metadata$as_of <- as.Date(forecast_date)
  attributes(nssp_data)$metadata$other_keys <- "source"
  nssp_data
}

# full_data with roles swapped (value -> nssp); the extra_data for the nssp signal.
flu_spoof_full_data <- function(full_data) {
  full_data %>%
    rename(nssp = value) %>%
    filter(source == "nhsn") %>%
    select(-c(source, epiweek, epiyear, season, season_week))
}

# Pull the archive inputs from a built targets store (default: flu_hosp_prod).
# Call once, reuse across iterations.
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

# The one call. Returns the (epi_data, extra_data) pair a forecaster is fed for
# a given signal -- exactly what the batch runner passes.
flu_forecast_input <- function(forecast_date,
                               signal = c("nhsn", "nssp"),
                               version_policy = "as_of",
                               generation_date = forecast_date,
                               archives = flu_load_archives(),
                               insufficient_data_geos = g_insufficient_data_geos_default) {
  signal <- match.arg(signal)
  full_data <- flu_build_full_data(
    archives$nhsn, version_policy, generation_date, forecast_date,
    archives$flu_data_substitutions, archives$joined_latest_extra_data, insufficient_data_geos
  )
  if (signal == "nhsn") {
    # nhsn: epi_data = full_data, extra_data = raw nssp slice (latest_cutoff = forecast_date)
    nssp_slice <- flu_slice_archive(archives$nssp, version_policy, generation_date, latest_cutoff = forecast_date)
    list(epi_data = full_data, extra_data = nssp_slice)
  } else {
    # nssp: epi_data = reshaped nssp, extra_data = spoofed full_data
    list(
      epi_data = flu_build_nssp_data(archives$nssp, version_policy, generation_date, forecast_date),
      extra_data = flu_spoof_full_data(full_data)
    )
  }
}
