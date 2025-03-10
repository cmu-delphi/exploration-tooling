source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# Globals
g_aheads = 0:4 * 7
g_hhs_signal = "confirmed_admissions_covid_1d"
# The date when the forecast was generated (this is effectively the AS OF date).
g_forecast_generation_dates = seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L)[1:4]
# The reference date for the forecast.
g_forecast_dates = seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L)[1:4]
# This moves the week marker from Saturday to Wednesday
g_time_value_adjust = 3
# Directory for reports.
g_reports_dir = "reports"
# Fetch arguments for epidatr.
g_fetch_args = epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
# Debug mode will replace all forecaster functions with a fast dummy forecaster. Helps
# with prototyping the pipeline.
g_dummy_mode = as.logical(Sys.getenv("DUMMY_MODE", FALSE))
# Geos that have insufficient data for forecasting.
g_insufficient_data_geos = c("as", "pr", "vi", "gu", "mp")
# These are locations we shouldn't take into account when deciding on latency,
# since e.g. flusurv stopped updating, and the various geos stopped updating for
# ILI+
# note that this has a vector of names first, and then the list of things to exclude, because targets hates named lists and strips their names
g_very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))

# Human-readable object to be used for inspecting the forecasters in the pipeline.
g_forecaster_parameter_combinations <- rlang::list2(
  # just the data, possibly population scaled; likely to run into troubles
  # because of the scales of the different sources
  scaled_pop_main = bind_rows(
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      lags = list2(
        c(0, 7),
        c(0, 7, 14, 21),
      ),
      pop_scaling = FALSE,
      filter_source = "nhsn",
      filter_agg_level = "state",
      scale_method = "none",
      center_method = "median",
      nonlin_method = c("quart_root", "none"),
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = g_very_latent_locations
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      lags = list2(
        c(0, 7),
        c(0, 7, 14, 21),
      ),
      pop_scaling = FALSE,
      filter_source = "nhsn",
      filter_agg_level = "state",
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = g_very_latent_locations
    )
  ),
  scaled_pop_data_augmented = tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = "quantreg",
    lags = list(
      c(0, 7, 14, 21),
      c(0, 7)
    ),
    pop_scaling = FALSE,
    scale_method = "quantile",
    center_method = "median",
    nonlin_method = c("quart_root", "none"),
    filter_source = "",
    filter_agg_level = "",
    n_training = Inf,
    drop_non_seasons = TRUE,
    keys_to_ignore = g_very_latent_locations
  ),
  ## # The covid forecaster, ported over to flu. Also likely to struggle with the
  ## # extra data
  # the thing to beat (a simplistic baseline forecast)
  flatline = tidyr::expand_grid(
    forecaster = "flatline_fc",
    filter_source = "nhsn",
    filter_agg_level = "state"
  ),
  # using exogenous variables
  scaled_pop_exogenous = bind_rows(
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7) # exogenous feature
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "nhsn",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = g_very_latent_locations,
    ),
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms"),
        c("nssp", "nwss"),
        c("nssp", "nwss_region"),
        c("google_symptoms", "nwss"),
        c("google_symptoms", "nwss_region"),
        c("nwss", "nwss_region")
      ),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7), # first feature
          c(0, 7) # second feature
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "nhsn",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = g_very_latent_locations,
    ),
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms", "nwss", "nwss_region"),
      ),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7), # nssp
          c(0, 7), # google symptoms
          c(0, 7), # nwss
          c(0, 7), # nwss_region
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "nhsn",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = g_very_latent_locations,
    )
  ),
  ## # another kind of baseline forecaster
  ## no_recent_quant = tidyr::expand_grid(
  ##   forecaster = "no_recent_outcome",
  ##   trainer = "quantreg",
  ##   scale_method = "quantile",
  ##   nonlin_method = "quart_root",
  ##   filter_source = "",
  ##   use_population = c(TRUE, FALSE),
  ##   use_density = c(TRUE, FALSE),
  ##   week_method = "sine",
  ##   keys_to_ignore = g_very_latent_locations
  ## ),
  no_recent_but_exogenous = bind_rows(
    expand_grid(
      forecaster = "no_recent_outcome",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
      lags = list2(
        list2(
          # no hhs
          c(0, 7) # exogenous feature
        )
      ),
      scale_method = "quantile",
      nonlin_method = "quart_root",
      filter_source = c("", "nhsn"),
      use_population = TRUE,
      use_density = FALSE,
      week_method = "sine",
      n_training = Inf,
      keys_to_ignore = g_very_latent_locations
    ),
    expand_grid(
      forecaster = "no_recent_outcome",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms"),
        c("nssp", "nwss"),
        c("nssp", "nwss_region"),
        c("google_symptoms", "nwss"),
        c("google_symptoms", "nwss_region"),
        c("nwss", "nwss_region")
      ),
      lags = list2(
        list2(
          # no hhs
          c(0, 7), # first feature
          c(0, 7) # second feature
        )
      ),
      scale_method = "quantile",
      nonlin_method = "quart_root",
      filter_source = c("", "nhsn"),
      use_population = TRUE,
      use_density = FALSE,
      week_method = "sine",
      n_training = Inf,
      keys_to_ignore = g_very_latent_locations
    ),
    expand_grid(
      forecaster = "no_recent_outcome",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms", "nwss", "nwss_region"),
      ),
      lags = list2(
        list2(
          # no hhs
          c(0, 7), # nssp
          c(0, 7), # google symptoms
          c(0, 7), # nwss
          c(0, 7) # nwss_region
        )
      ),
      scale_method = "quantile",
      nonlin_method = "quart_root",
      filter_source = c("", "nhsn"),
      use_population = TRUE,
      use_density = FALSE,
      week_method = "sine",
      n_training = Inf,
      keys_to_ignore = g_very_latent_locations
    )
  ),
  scaled_pop_season = bind_rows(
    tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7, 14, 21),
        c(0, 7)
      ),
      seasonal_method = list("flu", "indicator", "climatological"),
      pop_scaling = FALSE,
      train_residual = c(TRUE, FALSE),
      filter_source = "nhsn",
      filter_agg_level = "state",
      drop_non_seasons = c(TRUE, FALSE),
      n_training = Inf,
      seasonal_backward_window = 5,
      keys_to_ignore = g_very_latent_locations
    ),
    # Window-based seasonal method shouldn't drop non-seasons
    tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7)
      ),
      seasonal_method = list("window", c("window", "flu"), c("window", "climatological")),
      pop_scaling = FALSE,
      train_residual = c(FALSE, TRUE),
      filter_source = c("", "nhsn"),
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      seasonal_backward_window = 5,
      keys_to_ignore = g_very_latent_locations
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7, 14, 21)
      ),
      seasonal_method = list("window", c("window", "flu"), c("window", "climatological")),
      pop_scaling = FALSE,
      train_residual = c(FALSE, TRUE),
      filter_source = c("", "nhsn"),
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      seasonal_backward_window = 8,
      keys_to_ignore = g_very_latent_locations
    )
    # trying various window sizes
  ),
  scaled_pop_season_exogenous = bind_rows(
    expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "nwss", "nwss_region"), # removing google_symptoms for lack of data for now
      lags = list2(
        list2(
          c(0, 7), # hhs
          c(0, 7) # exogenous feature
        )
      ),
      seasonal_method = list("window"),
      pop_scaling = FALSE,
      filter_source = "",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = FALSE,
      keys_to_ignore = g_very_latent_locations
    )
  ),
  season_window_sizes = tidyr::expand_grid(
    forecaster = "scaled_pop_seasonal",
    trainer = "quantreg",
    lags = list(
      c(0, 7)
    ),
    seasonal_method = list("window"),
    pop_scaling = FALSE,
    train_residual = FALSE,
    filter_source = "",
    filter_agg_level = "state",
    drop_non_seasons = FALSE,
    n_training = Inf,
    seasonal_backward_window = c(3, 5, 7, 9, 52),
    seasonal_forward_window = c(3, 5, 7),
    keys_to_ignore = g_very_latent_locations
  ),
  climate_linear = expand_grid(
    forecaster = "climate_linear_ensembled",
    scale_method = c("quantile", "none"),
    center_method = "median",
    nonlin_method = "quart_root",
    filter_source = c("", "nhsn"),
    filter_agg_level = "state",
    drop_non_seasons = c(FALSE),
    aheads = list(c(0, 7, 14, 21))
  )
) %>%
  map(function(x) {
    if (g_dummy_mode) {
      x$forecaster <- "dummy_forecaster"
    }
    x
  }) %>%
  map(add_id) %>%
  # Add the outcome to each forecaster.
  map(function(x) {
    x$outcome <- "hhs"
    x
  })
g_forecaster_params_grid <- g_forecaster_parameter_combinations %>%
  imap(
    \(x, i) make_forecaster_grid(x, i)
  ) %>%
  bind_rows()
stopifnot(length(g_forecaster_params_grid$id %>% unique()) == length(g_forecaster_params_grid$id))

parameter_targets <- list2(
  tar_target(name = aheads, command = g_aheads),
  tar_target(name = forecast_dates, command = g_forecast_dates),
  # This is for forecaster_lookup.
  tar_target(name = forecaster_params_grid, command = g_forecaster_params_grid),
  # This is for notebook generation.
  tar_target(name = forecaster_parameter_combinations, command = g_forecaster_parameter_combinations)
)
data_targets <- rlang::list2(
  tar_target(
    name = hhs_archive_data_asof,
    command = {
      get_health_data(as.Date(forecast_dates), disease = "flu") %>%
        mutate(version = as.Date(forecast_dates)) %>%
        relocate(geo_value, time_value, version, hhs)
    },
    pattern = map(forecast_dates)
  ),
  # TODO: Share code with covid?
  tar_target(
    name = hhs_archive,
    command = {
      hhs_archive <- hhs_archive_data_asof %>%
        as_epi_archive(compactify = TRUE) %>%
        daily_to_weekly_archive(agg_columns = "hhs")
      hhs_archive$DT %>%
        add_season_info() %>%
        mutate(
          agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse("us" == geo_value, "nation", "state"))
        ) %>%
        add_pop_and_density() %>%
        mutate(hhs = hhs / population * 10L^5) %>%
        mutate(source = "nhsn") %>%
        mutate(agg_level = ifelse(geo_value == "us", "nation", "state")) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE) %>%
        `$`("DT") %>%
        select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
    }
  ),
  tar_target(
    name = flusurv,
    command = {
      flusurv_adjusted <- generate_flusurv_adjusted()
      flusurv_adjusted$DT %>%
        mutate(
          time_value = time_value + g_time_value_adjust,
          version = version + g_time_value_adjust
        ) %>%
        mutate(source = "flusurv") %>%
        select(geo_value, time_value, version, hhs = adj_hosp_rate, source, agg_level, season, season_week)
    }
  ),
  tar_target(
    name = ili_plus,
    command = {
      ili_plus <- gen_ili_data()
      ili_plus <- ili_plus$DT %>%
        drop_na() %>%
        filter(hhs > 0.0001) %>%
        mutate(
          time_value = time_value + g_time_value_adjust,
          version = time_value
        ) %>%
        select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
      to_keep <-
        ili_plus %>%
        group_by(geo_value, season) %>%
        summarize(total_count = length(hhs), .groups = "drop") %>%
        filter(total_count >= 20) %>%
        select(geo_value, season)
      to_keep %>%
        left_join(
          ili_plus,
          by = join_by(geo_value, season),
          relationship = "many-to-many"
        ) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE) %>%
        `$`("DT")
    }
  ),
  tar_target(
    name = flusion_data_archive,
    command = {
      flusion_data_archive <-
        bind_rows(ili_plus, flusurv, hhs_archive) %>%
        add_pop_and_density() %>%
        as_epi_archive(compactify = TRUE, other_keys = "source")
      flusion_data_archive <- flusion_data_archive$DT %>%
        filter(
          # TODO: Shared exclusions list?
          !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
          !is.na(hhs),
          time_value <= max(forecast_dates)
        ) %>%
        relocate(
          source,
          geo_value,
          time_value,
          version,
          hhs,
          agg_level,
          season,
          season_week,
          year,
          population,
          density
        ) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      flusion_data_archive
    }
  ),
  # TODO: Share code with covid?
  tar_target(
    name = nssp_archive,
    command = {
      nssp_state <- retry_fn(
        max_attempts = 10,
        wait_seconds = 1,
        fn = pub_covidcast,
        source = "nssp",
        signal = "pct_ed_visits_influenza",
        time_type = "week",
        geo_type = "state",
        geo_values = "*",
        fetch_args = g_fetch_args
      )
      nssp_hhs <- retry_fn(
        max_attempts = 10,
        wait_seconds = 1,
        fn = pub_covidcast,
        source = "nssp",
        signal = "pct_ed_visits_influenza",
        time_type = "week",
        geo_type = "hhs",
        geo_values = "*",
        fetch_args = g_fetch_args
      )
      nssp_state %>%
        bind_rows(nssp_hhs) %>%
        select(geo_value, time_value, issue, nssp = value) %>%
        as_epi_archive(compactify = TRUE) %>%
        `$`("DT") %>%
        # weekly data is indexed from the start of the week
        mutate(time_value = time_value + 6 - g_time_value_adjust) %>%
        # Artifically add in a one-week latency.
        mutate(version = time_value + 7) %>%
        mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
        unnest(cols = "source") %>%
        # Always convert to data.frame after dplyr operations on data.table.
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  # TODO: Share code with covid?
  tar_target(
    name = google_symptoms_archive,
    command = {
      used_searches <- c(1, 3, 4)
      # not using actual versions here because the only revision behavior is the
      # source going down completely, which means we're actually just comparing
      # with the version without this source
      all_of_them <- lapply(used_searches, \(search_name) {
        google_symptoms_state_archive <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "google-symptoms",
          signals = glue::glue("s0{search_name}_smoothed_search"),
          time_type = "day",
          geo_type = "state",
          geo_values = "*",
          fetch_args = g_fetch_args
        )
        google_symptoms_hhs_archive <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "google-symptoms",
          signals = glue::glue("s0{search_name}_smoothed_search"),
          time_type = "day",
          geo_type = "hhs",
          geo_values = "*",
          fetch_args = g_fetch_args
        )
        google_symptoms_archive_min <- google_symptoms_state_archive %>%
          bind_rows(google_symptoms_hhs_archive) %>%
          select(geo_value, time_value, value) %>%
          daily_to_weekly() %>%
          mutate(version = time_value) %>%
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          filter(!is.na(value)) %>%
          relocate(source, geo_value, time_value, version, value) %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
      })
      all_of_them[[1]]$DT %<>% rename(google_symptoms_1_cough = value)
      all_of_them[[2]]$DT %<>% rename(google_symptoms_3_fever = value)
      all_of_them[[3]]$DT %<>% rename(google_symptoms_4_bronchitis = value)
      google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]]) %>% epix_merge(all_of_them[[3]])
      pre_pipeline <- google_symptoms_archive %>%
        epix_as_of(as.Date("2023-10-04"))
      colnames <- c("google_symptoms_1_cough", "google_symptoms_3_fever", "google_symptoms_4_bronchitis")
      for (colname in colnames) {
        learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
        google_symptoms_archive$DT %<>%
          data_whitening(colname = colname, learned_params, join_cols = c("geo_value", "source"))
      }
      google_symptoms_archive$DT %>%
        mutate(
          google_symptoms = ifelse(is.na(google_symptoms_1_cough), 0, google_symptoms_1_cough) +
            ifelse(is.na(google_symptoms_3_fever), 0, google_symptoms_3_fever) +
            ifelse(is.na(google_symptoms_4_bronchitis), 0, google_symptoms_4_bronchitis)
        ) %>%
        # Always convert to data.frame after dplyr operations on data.table
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  # TODO: Share code with covid?
  # TODO: Geo code share?
  tar_target(
    name = nwss_coarse,
    command = {
      nwss <- get_nwss_coarse_data("flu") %>%
        arrange(geo_value, time_value) %>%
        add_pop_and_density() %>%
        drop_na() %>%
        select(-agg_level, -year, -agg_level, -population, -density)
      pop_data <- gen_pop_and_density_data()
      cw <- readr::read_csv(
        "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv",
        show_col_types = FALSE,
        progress = FALSE
      ) %>%
        left_join(
          readr::read_csv(
            "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv",
            show_col_types = FALSE,
            progress = FALSE
          ),
          by = join_by(state_code == state_code)
        ) %>%
        mutate(hhs = as.character(hhs)) %>%
        select(geo_value = state_id, hhs_region = hhs)
      nwss_hhs_region <- nwss %>%
        left_join(cw, by = "geo_value") %>%
        mutate(year = year(time_value)) %>%
        left_join(pop_data, by = join_by(geo_value, year)) %>%
        select(-year, density) %>%
        group_by(time_value, hhs_region) %>%
        summarize(
          value = sum(value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          activity_level = sum(activity_level * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          region_value = mean(region_value * population) / sum(population, na.rm = TRUE),
          national_value = sum(national_value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(agg_level = "hhs_region", hhs_region = as.character(hhs_region)) %>%
        rename(geo_value = hhs_region)
      nwss %>%
        mutate(agg_level = "state") %>%
        bind_rows(nwss_hhs_region) %>%
        select(
          geo_value,
          time_value,
          nwss = value,
          nwss_region = region_value,
          nwss_national = national_value
        ) %>%
        mutate(time_value = time_value - g_time_value_adjust, version = time_value) %>%
        arrange(geo_value, time_value) %>%
        mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
        unnest(cols = "source") %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  tar_target(
    name = hhs_region,
    command = {
      hhs_region <- readr::read_csv(
        "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv"
      )
      state_id <- readr::read_csv(
        "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv"
      )
      hhs_region %>%
        left_join(state_id, by = "state_code") %>%
        select(hhs_region = hhs, geo_value = state_id) %>%
        mutate(hhs_region = as.character(hhs_region))
    }
  ),
  tar_target(
    name = hhs_evaluation_data,
    command = {
      new_flu_data <- flusion_data_archive$DT %>%
        filter(
          source == "nhsn",
          agg_level %in% c("state", "nation"),
          time_value >= as.Date("2023-10-04")
        ) %>%
        drop_na() %>%
        mutate(hhs = hhs * population / 10**5) %>%
        as_epi_archive(compactify = TRUE)
      new_flu_data %>%
        epix_as_of(new_flu_data$versions_end) %>%
        rename(
          true_value = hhs,
          target_end_date = time_value,
          signal = source
        ) %>%
        select(
          signal,
          geo_value,
          target_end_date,
          true_value,
          population
        ) %>%
        # Push Wednesday labels to Saturday.
        mutate(target_end_date = target_end_date + g_time_value_adjust)
    }
  ),
  tar_target(
  name = state_geo_values,
  command = {
    hhs_evaluation_data %>%
      pull(geo_value) %>%
      unique()
  }
),
  tar_target(
    name = joined_archive_data,
    command = {
      joined_archive_data <- flusion_data_archive %>%
        epix_merge(google_symptoms_archive, sync = "locf") %>%
        epix_merge(nwss_coarse, sync = "locf") %>%
        epix_merge(nssp_archive, sync = "locf") %>%
        `$`("DT") %>%
        drop_na(agg_level) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)

      # Jank adding of hhs_region column to the data
      joined_archive_data <- joined_archive_data$DT %>%
        add_hhs_region_sum(hhs_region) %>%
        as_epi_archive(other_keys = "source")
      # TODO: This is a hack to ensure the as_of data is cached. Maybe there's a better way.
      epix_slide_simple(joined_archive_data, dummy_forecaster, forecast_dates, cache_key = "joined_archive_data")
      joined_archive_data
    }
  )
)
get_partially_applied_forecaster <- function(forecaster, ahead, params, param_names) {
  function(epi_data) rlang::inject(forecaster(epi_data, ahead = ahead, !!!(set_names(params, param_names))))
}
forecasts_and_scores <- tar_map(
  values = g_forecaster_params_grid,
  names = id,
  unlist = FALSE,
  tar_target(
    name = forecast,
    command = {
      out <- epix_slide_simple(
        joined_archive_data,
        get_partially_applied_forecaster(forecaster, aheads, params, param_names),
        forecast_dates,
        cache_key = "joined_archive_data"
      ) %>%
        # TODO: Hack fix because whitening has edge cases. Remove when fixed.
        sort_by_quantile() %>%
        rename(prediction = value) %>%
        mutate(ahead = as.numeric(target_end_date - forecast_date)) %>%
        mutate(id = id)
      out
    },
    pattern = map(aheads)
  ),
  tar_target(
    name = score,
    command = {
      forecasts <- forecast %>%
        # Push the Wednesday markers to Saturday, to match targets with truth data.
        mutate(
          forecast_date = forecast_date + g_time_value_adjust,
          target_end_date = target_end_date + g_time_value_adjust
        ) %>%
        rename("model" = "id")
      # browser()
      evaluate_predictions(forecasts = forecasts, truth_data = hhs_evaluation_data) %>%
        rename("id" = "model")
    }
  )
)

combined_forecasts_and_scores <- rlang::list2(
  forecasts_and_scores,
  tar_combine(
    delphi_forecasts,
    forecasts_and_scores[["forecast"]],
    command = {
      dplyr::bind_rows(!!!.x) %>%
        rename(forecaster = id) %>%
        filter(geo_value %in% state_geo_values) %>%
        # Push the Wednesday markers to Saturday, to match targets with truth data.
        mutate(
          forecast_date = forecast_date + g_time_value_adjust,
          target_end_date = target_end_date + g_time_value_adjust
        )
    }
  ),
  tar_combine(
    delphi_scores,
    forecasts_and_scores[["score"]],
    command = {
      dplyr::bind_rows(!!!.x) %>%
        rename(forecaster = id) %>%
        filter(geo_value %in% state_geo_values)
    }
  )
)

external_targets <- list2(
  tar_target(
    outside_forecaster_subset,
    command = c("FluSight-baseline", "FluSight-ensemble", "UMass-flusion")
  ),
  tar_target(
    external_forecasts_file,
    command = {
      s3load("flusight_forecasts_2023.rds", bucket = "forecasting-team-data")
      flusight_forecasts_2023
    }
  ),
  tar_target(
    external_forecasts,
    command = {
      external_forecasts_file %>%
        filter(forecaster %in% outside_forecaster_subset)
    }
  ),
  tar_target(
    external_scores,
    command = {
      evaluate_predictions(
        forecasts = external_forecasts %>%
          filter(forecast_date %in% (forecast_dates + g_time_value_adjust)) %>%
          rename(model = forecaster),
        truth_data = hhs_evaluation_data
      ) %>%
        rename(forecaster = model)
    }
  )
)
joined_targets <- rlang::list2(
  tar_target(
    rescaled_delphi_forecasts,
    command = {
      delphi_forecasts %>%
        mutate(forecast_date = ceiling_date(forecast_date, "weeks", week_start = 6)) %>%
        left_join(hhs_evaluation_data %>% distinct(geo_value, population), by = "geo_value") %>%
        mutate(prediction = prediction * population / 10L**5) %>%
        select(-population)
    }
  ),
  tar_target(joined_forecasts, command = rescaled_delphi_forecasts %>% bind_rows(external_forecasts)),
  tar_target(joined_scores, command = delphi_scores %>% bind_rows(external_scores)),
  tar_map(
    values = list(forecaster_family = unique(g_forecaster_params_grid$family)),
    tar_target(
      name = notebook,
      command = {
        params_subset <- forecaster_parameter_combinations[[forecaster_family]]
        filtered_forecasts <- joined_forecasts %>%
          filter(forecaster %in% c(params_subset$id, outside_forecaster_subset))
        filtered_scores <- joined_scores %>%
          filter(forecaster %in% c(params_subset$id, outside_forecaster_subset))

        rmarkdown::render(
          "scripts/reports/comparison-notebook.Rmd",
          params = list(
            forecaster_parameters = params_subset,
            forecaster_family = forecaster_family,
            forecasts = filtered_forecasts,
            scores = filtered_scores,
            truth_data = hhs_evaluation_data,
            disease = "flu"
          ),
          output_file = here::here(g_reports_dir, paste0("flu-notebook-", forecaster_family, ".html"))
        )
      }
    )
  ),
  # TODO: Fix notebook, this is coming in a follow-up PR.
  # tar_target(
  #   overall_notebook,
  #   command = {
  #     rmarkdown::render(
  #       "scripts/reports/overall-comparison-notebook.Rmd",
  #       params = list(
  #         forecaster_parameters = g_forecaster_parameter_combinations,
  #         forecasts = joined_forecasts,
  #         scores = joined_scores,
  #         truth_data = hhs_evaluation_data,
  #         disease = g_disease
  #       ),
  #       output_file = here::here(g_reports_dir, paste0(g_disease, "-overall-notebook.html"))
  #     )
  #   }
  # ),
  # TODO: Fix notebook, it's missing process_nhsn_data() function.
  # tar_target(
  #   new_data_notebook,
  #   command = {
  #     rmarkdown::render("scripts/reports/new_data.Rmd", output_file = here::here("reports", "new_data.html"))
  #   }
  # )
)


rlang::list2(
  parameter_targets,
  data_targets,
  combined_forecasts_and_scores,
  external_targets,
  joined_targets
)
