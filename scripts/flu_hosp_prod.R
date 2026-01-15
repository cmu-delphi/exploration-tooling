# The Flu Hospitalization Production Forecasting Pipeline.
suppressPackageStartupMessages(source("R/load_all.R"))


# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).

# Setup targets config.
set_targets_config()
g_aheads <- -1:3
g_submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
g_insufficient_data_geos <- c("as", "mp", "vi", "gu")
g_insufficient_data_geos_nssp <- c(g_insufficient_data_geos, "wy")
g_excluded_geos <- c("as", "gu", "mh")
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "flu"
g_s3_prefix <- "exploration"
g_external_object_name <- glue::glue("2024/2024-2025_{g_disease}_hosp_forecasts.parquet")
# needed for windowed_seasonal
g_very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))
# Date to cut the truth data off at, so we don't have too much of the past for
# plotting.
g_truth_data_date <- "2023-09-01"
# Whether we're running in backtest mode.
# If TRUE, we don't run the report notebook, which is (a) slow and (b) should be
# preserved as an ASOF snapshot of our production results for that week.
# If TRUE, we run a scoring notebook, which scores the historical forecasts
# against the truth data and compares them to the ensemble.
# If FALSE, we run the weekly report notebook.
g_backtest_mode <- as.logical(Sys.getenv("BACKTEST_MODE", FALSE))
if (!g_backtest_mode) {
  # This is the as_of for the forecast. If run on our typical schedule, it's
  # today, which is a Wednesday. Sometimes, if we're doing a delayed forecast,
  # it's a Thursday. It's used for stamping the data and for determining the
  # appropriate as_of when creating the forecast.
  g_forecast_generation_dates <- Sys.Date()
  # Usually, the forecast_date is the same as the generation date, but you can
  # override this. It should be a Wednesday.
  g_forecast_dates <- round_date(g_forecast_generation_dates, "weeks", week_start = 3)
  # the forecast is actually for the wednesday beforehand for these days
  if (Sys.Date() %in% as.Date(c("2025-12-29"))) {
    g_forecast_dates <- as.Date("2025-12-24")
  }
} else {
  g_forecast_generation_dates <- c(
    as.Date(c("2024-11-21", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
    seq.Date(as.Date("2025-01-08"), as.Date("2025-12-17"), by = 7L),
    as.Date(c("2025-12-29")),
    seq.Date(as.Date("2025-12-31"), Sys.Date(), by = 7L),
  )
  # Every Wednesday since mid-Nov 2024
  g_forecast_dates <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)
}

# Forecaster definitions
g_linear <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter((source == "nhsn") | (source == "nssp")) %>%
    forecaster_baseline_linear(
      ahead,
      ...,
      residual_tail = 0.99,
      residual_center = 0.35,
      no_intercept = TRUE
    )
}
g_linear_no_population_scale <- function(epi_data, ahead, extra_data, ...) {
  forecaster_baseline_linear(
    epi_data,
    ahead,
    ...,
    residual_tail = 0.99,
    residual_center = 0.35,
    no_intercept = TRUE,
    population_scale = FALSE
  )
}
g_climate_base <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter((source == "nhsn") | (source == "nssp")) %>%
    climatological_model(ahead, ...)
}
g_climate_geo_agged <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter((source == "nhsn") | (source == "nssp")) %>%
    climatological_model(
      ahead,
      ...,
      geo_agg = TRUE
    )
}
g_windowed_seasonal <- function(epi_data, ahead, extra_data, ...) {
  scaled_pop_seasonal(
    epi_data,
    outcome = "value",
    ahead = ahead * 7,
    ...,
    trainer = epipredict::quantile_reg(),
    seasonal_method = "window",
    pop_scaling = FALSE,
    lags = c(0, 7),
    keys_to_ignore = g_very_latent_locations
  ) %>%
    mutate(target_end_date = target_end_date + 3)
}
g_windowed_seasonal_extra_sources <- function(epi_data, ahead, extra_data, ...) {
  fcst <-
    epi_data %>%
    left_join(extra_data, by = join_by(geo_value, time_value)) %>%
    scaled_pop_seasonal(
      outcome = "value",
      ahead = ahead * 7,
      extra_sources = "nssp",
      ...,
      seasonal_method = "window",
      trainer = epipredict::quantile_reg(),
      drop_non_seasons = TRUE,
      pop_scaling = FALSE,
      lags = list(c(0, 7), c(0, 7)),
      keys_to_ignore = g_very_latent_locations
    ) %>%
    select(-source) %>%
    mutate(target_end_date = target_end_date + 3) %>%
    filter(geo_value %nin% c("mo", "wy"))
  fcst
}
g_baseline_forecaster <- function(epi_data, ahead, extra_data, ...) {
  # all of the forecasts are made in the last ahead
  if (ahead < 3) {
    return(tibble(geo_value = character(), forecast_date = Date(), target_end_date = Date(), quantile_value = numeric(), value = numeric()))
  }
  real_forecast_date <- attributes(epi_data)$metadata$as_of
  last_data <- epi_data$time_value %>% max()
  latency_weeks <- as.integer(real_forecast_date - last_data) / 7
  fcst <- epi_data %>%
    cdc_baseline_forecaster(
      "value",
      args_list = cdc_baseline_args_list(aheads = 1:(3 + latency_weeks))
    ) %>%
    `$`(predictions) %>%
    pivot_quantiles_longer(.pred_distn) %>%
    select(
      geo_value, forecast_date,
      target_end_date = target_date,
      value = .pred_distn_value,
      quantile = .pred_distn_quantile_level
    ) %>%
    mutate(
      forecast_date = floor_date(forecast_date, "weeks", week_start = 7) + 3,
      target_end_date = floor_date(target_end_date, "weeks", week_start = 7) + 3
    ) %>%
    mutate(
      ahead = as.integer(target_end_date - forecast_date),
      forecast_date = real_forecast_date
    )
  ## fcst %>%
  ##   group_by(geo_value, forecast_date, target_end_date, quantile) %>%
  ##   count() %>%
  ##   arrange(desc(n))
  fcst
}
ids <- c(
  "cdc_baseline",
  "linear",
  "linear_no_population_scale",
  "windowed_seasonal",
  "windowed_seasonal_extra_sources",
  "climate_base",
  "climate_geo_agged",
  "seasonal_nssp_latest"
)
list_of_empty_lists <- lapply(seq_along(ids), \(x) list())
g_forecaster_params_grid <- tibble(
  id = ids,
  forecaster = rlang::syms(c(
    "g_baseline_forecaster",
    "g_linear",
    "g_linear_no_population_scale",
    "g_windowed_seasonal",
    "g_windowed_seasonal_extra_sources",
    "g_climate_base",
    "g_climate_geo_agged",
    "g_windowed_seasonal_extra_sources"
  )),
  params = list_of_empty_lists,
  param_names = list_of_empty_lists
)


# ================================ PARAMETERS AND DATA TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  tar_target(aheads, command = g_aheads),
  # Needed by create_flu_data_targets()
  tar_target(forecast_dates, command = g_forecast_dates),
  tar_file(
    forecast_report_rmd,
    command = "scripts/reports/forecast_report.Rmd"
  ),
  tar_file(
    ongoing_score_report_rmd,
    command = "scripts/reports/ongoing_score_report.Rmd"
  ),
  tar_file(
    name = score_report_rmd,
    command = "scripts/reports/score_report.Rmd"
  ),
  tar_file(
    flu_geo_exclusions,
    command = "scripts/flu_geo_exclusions.csv"
  ),
  tar_file(
    flu_nssp_geo_exclusions,
    command = "scripts/flu_nssp_geo_exclusions.csv"
  ),
  tar_file(
    flu_data_substitutions,
    command = "scripts/flu_data_substitutions.csv"
  ),
  create_flu_data_targets(),
  tar_target(
    joined_latest_extra_data,
    command = {
      joined_archive_data %>%
        epix_as_of(joined_archive_data$versions_end) %>%
        mutate(epiweek = epiweek(time_value), epiyear = epiyear(time_value)) %>%
        filter((agg_level == "state") | (agg_level == "nation")) %>%
        select(geo_value, source, time_value, hhs, season, season_week, epiweek, epiyear) %>%
        rename(value = hhs) %>%
        filter(source != "nhsn")
    }
  ),
  tar_change(
    name = nhsn_archive_data,
    change = get_s3_object_last_modified("nhsn_data_archive.parquet", "forecasting-team-data"),
    command = {
      get_nhsn_data_archive("nhsn_flu")
    }
  ),
  tar_target(
    name = nhsn_latest_data,
    command = {
      nhsn_archive_data %>%
        epix_as_of(min(Sys.Date(), nhsn_archive_data$versions_end)) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
    }
  ),
  tar_change(
    name = nssp_archive_data,
    change = max(
      get_covidcast_signal_last_update("nssp", "pct_ed_visits_influenza", "state"),
      get_socrata_updated_at("https://data.cdc.gov/api/views/mpgq-jmmr", lubridate::now(tz = "UTC"))
    ),
    command = {
      up_to_date_nssp_state_archive("influenza")
    }
  ),
  # tar_change(
  #   name = nssp_archive_data2,
  #   change = get_cast_api_latest_update_date(source = "nssp"),
  #   command = {
  #     get_cast_api_data(
  #       source = "nssp",
  #       signal = "pct_ed_visits_influenza",
  #       geo_type = "state",
  #       columns = c("geo_value", "time_value", "value", "report_ts_nominal_start", "report_ts_nominal_end"),
  #       limit = -1
  #     )
  #   }
  # ),
  tar_target(
    name = nssp_latest_data,
    command = {
      nssp_archive_data %>%
        epix_as_of(min(Sys.Date(), nssp_archive_data$versions_end))
    }
  ),
  # tar_change(
  #   name = nssp_latest_data2,
  #   change = get_cast_api_latest_update_date(source = "nssp"),
  #   command = {
  #     df <- get_cast_api_data(
  #       source = "nssp",
  #       signal = "pct_ed_visits_influenza",
  #       geo_type = "state",
  #       versions_before = Sys.Date() + 1,
  #       columns = c("geo_value", "time_value", "value", "report_ts_nominal_start"),
  #       limit = -1
  #     ) %>%
  #       rename(nssp = value, issue = report_ts_nominal_start) %>%
  #       # Need to adjust time_value by 3 days.
  #       mutate(time_value = time_value - 3, geo_value = tolower(geo_value), issue = as.Date(issue))
  #     max_issue <- df %>% summarize(max(issue)) %>% pull()
  #     df %>% as_epi_df(as_of = max_issue)
  #   }
  # ),
)


# ================================ FORECAST TARGETS ================================
forecast_targets <- tar_map(
  values = tidyr::expand_grid(
    g_forecaster_params_grid,
    tibble(
      forecast_date_int = g_forecast_dates,
      forecast_generation_date_int = g_forecast_generation_dates,
      forecast_date_chr = as.character(g_forecast_dates)
    )
  ),
  names = c("id", "forecast_date_chr"),
  tar_target(
    name = full_data,
    command = {
      # Train data
      if (grepl("latest", id)) {
        train_data <- nhsn_archive_data %>%
          epix_as_of(nhsn_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_generation_date_int))
      } else {
        train_data <- nhsn_archive_data %>%
          epix_as_of(min(as.Date(forecast_generation_date_int), nhsn_archive_data$versions_end))
      }
      train_data %<>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value - 3,
          source = "nhsn"
        )
      if (!grepl("latest", id)) {
        train_data %<>%
          data_substitutions(
            flu_data_substitutions,
            as.Date(forecast_generation_date_int)
          )
      }
      train_data %<>%
        filter(geo_value %nin% g_insufficient_data_geos)
      attributes(train_data)$metadata$as_of <- as.Date(forecast_date_int)
      full_data <- train_data %>%
        bind_rows(joined_latest_extra_data)
      attributes(full_data)$metadata$other_keys <- "source"
      attributes(full_data)$metadata$as_of <- as.Date(forecast_date_int)
      full_data
    }
  ),
  tar_target(
    name = forecast_nssp,
    command = {
      if (grepl("latest", id)) {
        nssp_data <- nssp_archive_data %>%
          epix_as_of(nssp_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_generation_date_int))
      } else {
        nssp_data <- nssp_archive_data %>%
          epix_as_of(min(as.Date(forecast_generation_date_int), nssp_archive_data$versions_end))
      }

      forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads, params, param_names)

      nssp_data %<>%
        rename(value = nssp) %>%
        mutate(time_value = floor_date(time_value, "week", week_start = 7) + 3) %>%
        mutate(source = "nhsn") %>%
        add_season_info()
      attributes(nssp_data)$metadata$as_of <- as.Date(forecast_date_int)
      attributes(nssp_data)$metadata$other_keys <- "source"
      # spoofing the name to switch their roles
      full_data_modified <- full_data %>%
        rename(nssp = value) %>%
        filter(source == "nhsn") %>%
        select(-c(source, epiweek, epiyear, season, season_week))
      nssp_data %>%
        forecaster_fn(extra_data = full_data_modified) %>%
        mutate(
          forecaster = id,
          geo_value = as.factor(geo_value)
        )
    },
    pattern = map(aheads)
  ),
  tar_target(
    name = forecast_nhsn,
    command = {
      if (grepl("latest", id)) {
        nssp_data <- nssp_archive_data %>%
          epix_as_of(nssp_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_date_int))
      } else {
        nssp_data <- nssp_archive_data %>%
          epix_as_of(min(as.Date(forecast_generation_date_int), nssp_archive_data$versions_end))
      }

      forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads, params, param_names)

      full_data %>%
        forecaster_fn(extra_data = nssp_data) %>%
        mutate(
          forecaster = id,
          geo_value = as.factor(geo_value)
        )
    },
    pattern = map(aheads)
  )
)

combined_nhsn_forecasts <- tar_combine(
  name = forecast_nhsn_full,
  forecast_targets[["forecast_nhsn"]],
  command = {
    dplyr::bind_rows(!!!.x)
  }
)

combined_nssp_forecasts <- tar_combine(
  name = forecast_nssp_full,
  forecast_targets[["forecast_nssp"]],
  command = {
    dplyr::bind_rows(!!!.x)
  }
)

# ================================ ENSEMBLE TARGETS ================================
ensemble_targets <- tar_map(
  values = tibble(
    forecast_date_int = g_forecast_dates,
    forecast_generation_date_int = g_forecast_generation_dates,
    forecast_date_chr = as.character(g_forecast_dates)
  ),
  names = "forecast_date_chr",
  tar_target(
    name = forecast_nhsn_full_filtered,
    command = {
      forecast_nhsn_full %>%
        filter(forecast_date == as.Date(forecast_date_int)) %>%
        # Remove `linear_no_population_scale`
        filter(forecaster %nin% c("linear_no_population_scale"))
    }
  ),
  tar_target(
    name = forecast_nssp_full_filtered,
    command = {
      forecast_nssp_full %>%
        filter(forecast_date == as.Date(forecast_date_int)) %>%
        # Renove `linear`
        filter(forecaster %nin% c("linear"))
    }
  ),
  tar_target(
    name = geo_forecasters_weights,
    command = {
      geo_forecasters_weights <- parse_prod_weights(flu_geo_exclusions, forecast_date_int, g_forecaster_params_grid$id)
      if (nrow(geo_forecasters_weights %>% filter(forecast_date == as.Date(forecast_date_int))) == 0) {
        cli_abort("there are no weights for the forecast date {forecast_date}")
      }
      geo_forecasters_weights
    },
  ),
  tar_target(
    name = geo_nssp_forecasters_weights,
    command = {
      geo_nssp_forecasters_weights <-
        parse_prod_weights(flu_nssp_geo_exclusions, forecast_date_int, g_forecaster_params_grid$id)
      if (nrow(geo_nssp_forecasters_weights %>% filter(forecast_date == as.Date(forecast_date_int))) == 0) {
        cli_abort("there are no weights for the forecast date {forecast_date}")
      }
      geo_nssp_forecasters_weights
    },
  ),
  tar_target(
    name = geo_exclusions,
    command = {
      exclude_geos(geo_forecasters_weights)
    }
  ),
  tar_target(
    name = climate_linear,
    command = {
      forecast_nhsn_full_filtered %>%
        # Apply the ahead-by-quantile weighting scheme
        ensemble_climate_linear(aheads, other_weights = geo_forecasters_weights) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup() %>%
        sort_by_quantile() %>%
        mutate(forecaster = "climate_linear")
    },
  ),
  tar_target(
    name = ens_ar_only,
    command = {
      forecast_nhsn_full_filtered %>%
        filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
        group_by(geo_value, forecast_date, target_end_date, quantile) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        sort_by_quantile() %>%
        mutate(forecaster = "ens_ar_only")
    }
  ),
  tar_target(
    name = ensemble_mixture_res,
    command = {
      climate_linear %>%
        bind_rows(
          forecast_nhsn_full_filtered %>%
            filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))
        ) %>%
        ensemble_weighted(geo_forecasters_weights) %>%
        mutate(forecaster = "ensemble_mix")
    },
  ),
  tar_target(
    name = ensemble_nssp_clim_lin,
    command = {
      forecast_nssp_full_filtered %>%
        ensemble_climate_linear(
          aheads,
          other_weights = geo_nssp_forecasters_weights,
          max_climate_ahead_weight = 0.6,
          max_climate_quantile_weight = 0.6
        ) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup() %>%
        sort_by_quantile() %>%
        mutate(forecaster = "climate_linear")
    },
  ),
  tar_target(
    name = ensemble_nssp_mixture_res,
    command = {
      ensemble_nssp_clim_lin %>%
        bind_rows(
          forecast_nssp_full_filtered %>%
            filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
            filter(forecast_date < target_end_date) # don't use for neg aheads
        ) %>%
        ensemble_weighted(geo_nssp_forecasters_weights) %>%
        mutate(forecaster = "ensemble_mix")
    },
  ),
  tar_target(
    name = forecasts_and_ensembles,
    command = {
      bind_rows(
        forecast_nhsn_full_filtered,
        climate_linear,
        ensemble_mixture_res,
        ens_ar_only
      )
    }
  ),
  tar_target(
    name = forecasts_and_ensembles_nssp,
    command = {
      bind_rows(
        forecast_nssp_full_filtered,
        ensemble_nssp_clim_lin,
        ensemble_nssp_mixture_res,
      )
    }
  ),
  tar_target(
    name = make_submission_csv,
    command = {
      if (!g_backtest_mode && g_submission_directory != "cache") {
        forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
        nhsn_submission <- ensemble_mixture_res %>%
          format_flusight(disease = "flu")
        nssp_submission <- ensemble_nssp_mixture_res %>%
          format_flusight(disease = "flu") %>%
          mutate(
            target = "wk inc flu prop ed visits",
            value = value / 100
          )
        bind_rows(nhsn_submission, nssp_submission) %>%
          write_submission_file(
            forecast_reference_date,
            file.path(g_submission_directory, "model-output/CMU-TimeSeries")
          )
      } else {
        cli_alert_info("Not making submission csv because we're in backtest mode or submission directory is cache")
      }
    },
    cue = tar_cue("always")
  ),
  tar_target(
    name = make_climate_submission_csv,
    command = {
      if (!g_backtest_mode && g_submission_directory != "cache") {
        forecast_nhsn_full_filtered %>%
          filter(forecaster %in% c("climate_base", "climate_geo_agged")) %>%
          group_by(geo_value, target_end_date, quantile) %>%
          summarize(forecast_date = as.Date(forecast_date_int), value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          ungroup() %>%
          filter(!(geo_value %in% g_excluded_geos)) %>%
          format_flusight(disease = "flu") %>%
          filter(location %nin% c("60", "66", "78")) %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            file.path(g_submission_directory, "model-output/CMU-climate_baseline"),
            file_name = "CMU-climate_baseline"
          )
      } else {
        cli_alert_info(
          "Not making climate submission csv because we're in backtest mode or submission directory is cache"
        )
      }
    },
    cue = tar_cue("always")
  ),
  tar_target(
    name = validate_result,
    command = {
      make_submission_csv
      # only validate if we're saving the result to a hub
      if (!g_backtest_mode && g_submission_directory != "cache") {
        validation <- validate_submission(
          g_submission_directory,
          file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(forecast_date_int))
        )
      } else {
        validation <- "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
      validation
    },
  ),
  tar_target(
    name = validate_climate_result,
    command = {
      make_climate_submission_csv
      # only validate if we're saving the result to a hub
      if (!g_backtest_mode && g_submission_directory != "cache") {
        validation <- validate_submission(
          g_submission_directory,
          file_path = sprintf(
            "CMU-climate_baseline/%s-CMU-climate_baseline.csv",
            get_forecast_reference_date(forecast_date_int)
          )
        )
      } else {
        validation <- "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
      validation
    },
  ),
  tar_target(
    name = truth_data_pre_process,
    command = {
      # Plot both as_of and latest data to compare
      nhsn_data <- nhsn_archive_data %>%
        epix_as_of(min(as.Date(forecast_generation_date_int), nhsn_archive_data$versions_end)) %>%
        mutate(source = "nhsn as_of forecast") %>%
        bind_rows(nhsn_latest_data %>% mutate(source = "nhsn")) %>%
        select(geo_value, target_end_date = time_value, value, source) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos)
      nssp_data <- nssp_latest_data %>%
        select(geo_value, target_end_date = time_value, value = nssp) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos) %>%
        mutate(target_end_date = target_end_date + 3, source = "nssp")
      list(nhsn_data, nssp_data)
    }
  ),
  tar_target(
    name = truth_data_nhsn,
    command = {
      nhsn_data <- truth_data_pre_process[[1]]
      nssp_data <- truth_data_pre_process[[2]]
      nssp_max_state_value <- nssp_data %>%
        rename(nssp = value) %>%
        full_join(
          nhsn_data %>%
            select(geo_value, target_end_date, value),
          by = join_by(geo_value, target_end_date)
        ) %>%
        group_by(geo_value) %>%
        summarise(rel_max_value = max(value, na.rm = TRUE) / max(nssp, na.rm = TRUE))
      nssp_renormalized <- nssp_data %>%
        left_join(nssp_max_state_value, by = join_by(geo_value)) %>%
        mutate(value = value * rel_max_value) %>%
        select(-rel_max_value)
      nhsn_data %>% bind_rows(nssp_renormalized)
    }
  ),
  tar_target(
    name = truth_data_nssp,
    command = {
      nhsn_data <- truth_data_pre_process[[1]]
      nssp_data <- truth_data_pre_process[[2]]
      nhsn_max_state_value <- nhsn_data %>%
        rename(nssp = value) %>%
        full_join(
          nssp_data %>%
            select(geo_value, target_end_date, value),
          by = join_by(geo_value, target_end_date)
        ) %>%
        group_by(geo_value) %>%
        summarise(rel_max_value = max(value, na.rm = TRUE) / max(nssp, na.rm = TRUE))
      nhsn_renormalized <-
        nhsn_data %>%
        left_join(
          nhsn_max_state_value,
          by = join_by(geo_value)
        ) %>%
        mutate(value = value * rel_max_value) %>%
        select(-rel_max_value)
      nssp_data %>% bind_rows(nhsn_renormalized)
    }
  ),
  tar_target(
    notebook,
    command = {
      # Only render the report if there is only one forecast date
      # i.e. we're running this in prod on schedule
      if (!g_backtest_mode) {
        if (!dir.exists(here::here("reports"))) {
          dir.create(here::here("reports"))
        }
        rmarkdown::render(
          forecast_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_prod_on_%s.html", as.Date(forecast_date_int), as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            forecast_nhsn = forecasts_and_ensembles %>% ungroup() %>% filter(forecaster != "climate_geo_agged"),
            forecast_nssp = forecasts_and_ensembles_nssp %>% ungroup() %>% filter(forecaster != "climate_geo_agged"),
            forecast_date = as.Date(forecast_date_int),
            truth_data_nhsn = truth_data_nhsn,
            truth_data_nssp = truth_data_nssp
          )
        )
      }
    }
  )
)


# ================================ SCORE TARGETS ================================
external_forecast_targets <- tar_map(
  values = tibble(
    forecast_date_int = seq(as.Date("2024-11-23"), round_date(Sys.Date() - 3, "week", 6), by = "week")
  ) %>%
    mutate(
      forecast_date_chr = as.character(as.Date(forecast_date_int)),
      filename = paste0(g_s3_prefix, "/", forecast_date_chr, "/", g_disease, "_forecasts.parquet"),
    ),
  names = "forecast_date_chr",
  tar_change(
    name = external_forecasts,
    change = get_s3_object_last_modified(filename, "forecasting-team-data"),
    command = {
      get_external_forecasts(filename)
    }
  ),
  tar_target(
    name = score_external_nhsn_forecasts,
    command = {
      score_forecasts(nhsn_latest_data, external_forecasts, "wk inc flu hosp")
    }
  )
)

combined_targets <- list2(
  tar_combine(
    name = external_forecasts_full,
    external_forecast_targets[["external_forecasts"]],
    command = {
      dplyr::bind_rows(!!!.x)
    }
  ),
  tar_combine(
    name = external_scores_nhsn_full,
    external_forecast_targets[["score_external_nhsn_forecasts"]],
    command = {
      dplyr::bind_rows(!!!.x)
    }
  )
)

list2(
  tar_change(
    external_forecasts,
    change = get_s3_object_last_modified(g_external_object_name, "forecasting-team-data"),
    command = {
      get_external_forecasts(g_external_object_name)
    }
  ),
  tar_combine(
    name = joined_forecasts_and_ensembles_nhsn,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = {
      filter_shared_geo_dates(
        dplyr::bind_rows(!!!.x),
        external_forecasts %>%
          filter(target == "wk inc flu hosp") %>%
          select(-target),
        min_locations = 52,
        min_dates = 40
      )
    }
  ),
  tar_combine(
    name = joined_forecasts_and_ensembles_nssp,
    ensemble_targets[["forecasts_and_ensembles_nssp"]],
    command = {
      filter_shared_geo_dates(
        dplyr::bind_rows(!!!.x),
        external_forecasts %>%
          filter(target == "wk inc flu prop ed visits") %>%
          select(-target) %>%
          mutate(value = value * 100),
        min_locations = 50,
        min_dates = 14
      )
    }
  ),
  tar_target(
    name = scores_nhsn,
    command = {
      score_forecasts(nhsn_latest_data, joined_forecasts_and_ensembles_nhsn, "wk inc flu hosp")
    }
  ),
  tar_target(
    name = scores_nssp,
    command = {
      nssp_latest_data %>%
        rename(value = nssp) %>%
        mutate(time_value = ceiling_date(time_value, unit = "week") - 1) %>%
        score_forecasts(joined_forecasts_and_ensembles_nssp, "wk inc flu prop ed visits")
    }
  )
)
if (g_backtest_mode) {
  score_notebook <- tar_target(
    name = score_plot,
    command = {
      render_score_plot(score_report_rmd, scores_nhsn, g_forecast_dates, "flu")
    },
    cue = tar_cue("always")
  )
} else {
  # Only render the report if there is only one forecast date
  # i.e. we're running this in prod on schedule
  score_notebook <- list2(
    tar_target(
      ongoing_nhsn_score_notebook,
      command = {
        if (!dir.exists(here::here("reports"))) {
          dir.create(here::here("reports"))
        }
        # Don't run if there aren't forecasts in the past 4 weeks to evaluate
        if (external_forecasts_full %>%
          filter(
            forecast_date >= round_date(Sys.Date() - 3, "week", 6) - 4 * 7,
            target == "wk inc flu hosp"
          ) %>%
          distinct(forecast_date) %>%
          nrow() == 0) {
          return()
        }
        # Score notebook individual average (see ongoing_score_report_rmd for documentation)
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_nhsn_scoring_individual.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            target = "nhsn",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc flu hosp") %>% select(-target),
            archive = nhsn_archive_data,
            scores = external_scores_nhsn_full,
            averaging_method = "individual"
          )
        )
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_nhsn_scoring_common.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            target = "nhsn",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc flu hosp") %>% select(-target),
            archive = nhsn_archive_data,
            scores = external_scores_nhsn_full,
            averaging_method = "common"
          )
        )
      }
    )
  )
}

list2(
  parameters_and_date_targets,
  forecast_targets,
  ensemble_targets,
  combined_nhsn_forecasts,
  combined_nssp_forecasts,
  external_forecast_targets,
  combined_targets,
  score_notebook
)
