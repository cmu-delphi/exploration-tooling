# The COVID Hospitalization Production Forecasting Pipeline.
suppressPackageStartupMessages(source("R/load_all.R"))


# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).

# Setup targets config.
set_targets_config()
g_aheads <- -1:3
g_submission_directory <- Sys.getenv("COVID_SUBMISSION_DIRECTORY", "cache")
g_insufficient_data_geos <- c("as", "mp", "vi", "gu")
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "covid"
g_external_object_name <- glue::glue("exploration/2024-2025_{g_disease}_hosp_forecasts.parquet")
# date to cut the truth data off at, so we don't have too much of the past
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
} else {
  g_forecast_generation_dates <- c(as.Date(c("2024-11-20", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")), seq.Date(as.Date("2025-01-08"), Sys.Date(), by = 7L))
  g_forecast_dates <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)
}

# Forecaster definitions
g_linear <- function(epi_data, ahead, extra_data, ...) {
  forecaster_baseline_linear(epi_data, ahead, ..., residual_tail = 0.97, residual_center = 0.097, no_intercept = TRUE)
}
g_climate_base <- function(epi_data, ahead, extra_data, ...) {
  climatological_model(
    epi_data, ahead, ...,
  )
}
g_climate_geo_agged <- function(epi_data, ahead, extra_data, ...) {
  climatological_model(
    epi_data, ahead, ...,
    geo_agg = TRUE
  )
}
g_windowed_seasonal <- function(epi_data, ahead, extra_data, ...) {
  fcst <-
    epi_data %>%
    scaled_pop_seasonal(
      outcome = "value",
      ahead = ahead * 7,
      ...,
      seasonal_method = "none",
      trainer = epipredict::quantile_reg(),
      drop_non_seasons = TRUE,
      pop_scaling = FALSE,
      lags = list(c(0, 7))
    ) %>%
    mutate(target_end_date = target_end_date + 3)
  fcst
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
      lags = list(c(0, 7), c(0, 7))
    ) %>%
    mutate(target_end_date = target_end_date + 3) %>%
    # Wyoming has no data for NSSP since July 2024
    filter(geo_value %nin% c("mo", "usa", "wy"))
  fcst
}
g_forecaster_params_grid <- tibble(
  id = c("linear", "windowed_seasonal", "windowed_seasonal_extra_sources", "climate_base", "climate_geo_agged"),
  forecaster = rlang::syms(c("g_linear", "g_windowed_seasonal", "g_windowed_seasonal_extra_sources", "g_climate_base", "g_climate_geo_agged")),
  params = list(
    list(),
    list(),
    list(),
    list(),
    list()
  ),
  param_names = list(
    list(),
    list(),
    list(),
    list(),
    list()
  )
)


# ================================ PARAMETERS AND DATA TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  tar_target(aheads, command = g_aheads),
  tar_file(
    forecast_report_rmd,
    command = "scripts/reports/forecast_report.Rmd"
  ),
  tar_file(
    score_report_rmd,
    command = "scripts/reports/score_report.Rmd"
  ),
  tar_file(
    covid_geo_exclusions,
    command = "covid_geo_exclusions.csv"
  ),
  tar_file(
    covid_data_substitutions,
    command = "covid_data_substitutions.csv"
  ),
  tar_change(
    name = nhsn_archive_data,
    change = get_s3_object_last_modified("nhsn_data_archive.parquet", "forecasting-team-data"),
    command = {
      get_nhsn_data_archive("nhsn_covid")
    }
  ),
  tar_target(
    nhsn_latest_data,
    command = {
      nhsn_archive_data %>%
        epix_as_of(min(Sys.Date(), nhsn_archive_data$versions_end)) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
    }
  ),
  tar_change(
    nssp_archive_data,
    change = get_covidcast_signal_last_update("nssp", "pct_ed_visits_covid", "state"),
    command = {
      up_to_date_nssp_state_archive("covid")
    }
  ),
  tar_target(
    nssp_latest_data,
    command = {
      nssp_archive_data %>%
        epix_as_of(min(Sys.Date(), nssp_archive_data$versions_end))
    }
  )
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
    name = forecast_res,
    command = {
      nhsn_data <- nhsn_archive_data %>%
        epix_as_of(min(as.Date(forecast_date_int), nhsn_archive_data$versions_end)) %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value - 3
        ) %>%
        data_substitutions(covid_data_substitutions, as.Date(forecast_generation_date_int)) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
      attributes(nhsn_data)$metadata$as_of <- as.Date(forecast_date_int)

      nssp_data <- nssp_archive_data %>%
        epix_as_of(min(as.Date(forecast_date_int), nssp_archive_data$versions_end))

      forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads, params, param_names)

      forecaster_fn(nhsn_data, extra_data = nssp_data) %>%
        mutate(
          forecaster = id,
          geo_value = as.factor(geo_value)
        )
    },
    pattern = map(aheads)
  )
)
combined_forecasts <- tar_combine(
  name = forecast_full,
  forecast_targets[["forecast_res"]],
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
    name = forecast_full_filtered,
    command = {
      forecast_full %>%
        filter(forecast_date == as.Date(forecast_date_int))
    }
  ),
  tar_target(
    name = geo_forecasters_weights,
    command = {
      geo_forecasters_weights <- parse_prod_weights(covid_geo_exclusions, forecast_date_int, g_forecaster_params_grid$id)
      if (nrow(geo_forecasters_weights %>% filter(forecast_date == as.Date(forecast_date_int))) == 0) {
        cli_abort("there are no weights for the forecast date {forecast_date}")
      }
      geo_forecasters_weights
    },
  ),
  tar_target(
    name = geo_exclusions,
    command = {
      exclude_geos(geo_forecasters_weights)
    }
  ),
  tar_target(
    name = ensemble_clim_lin,
    command = {
      forecast_full_filtered %>%
        ensemble_climate_linear(
          aheads,
          other_weights = geo_forecasters_weights,
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
    name = ens_ar_only,
    command = {
      forecast_full_filtered %>%
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
      ensemble_clim_lin %>%
        bind_rows(
          forecast_full_filtered %>%
            filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
            filter(forecast_date < target_end_date) # don't use for neg aheads
        ) %>%
        ensemble_weighted(geo_forecasters_weights) %>%
        mutate(forecaster = "ensemble_mix")
    },
  ),
  tar_target(
    name = forecasts_and_ensembles,
    command = {
      bind_rows(
        forecast_full_filtered,
        ensemble_clim_lin,
        ensemble_mixture_res,
        ens_ar_only
      )
    }
  ),
  tar_target(
    name = make_submission_csv,
    command = {
      if (!g_backtest_mode && g_submission_directory != "cache") {
        forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
        ensemble_mixture_res %>%
          format_flusight(disease = "covid") %>%
          write_submission_file(forecast_reference_date, file.path(g_submission_directory, "model-output/CMU-TimeSeries"))
      } else {
        cli_alert_info("Not making submission csv because we're in backtest mode or submission directory is cache")
      }
    },
  ),
  tar_target(
    name = make_climate_submission_csv,
    command = {
      if (!g_backtest_mode && g_submission_directory != "cache") {
        forecast_full_filtered %>%
          filter(forecaster %in% c("climate_base", "climate_geo_agged")) %>%
          group_by(geo_value, target_end_date, quantile) %>%
          summarize(forecast_date = as.Date(forecast_date_int), value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          ungroup() %>%
          format_flusight(disease = "covid") %>%
          filter(location %nin% c("60", "66", "78")) %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            file.path(g_submission_directory, "model-output/CMU-climate_baseline"),
            file_name = "CMU-climate_baseline"
          )
      } else {
        cli_alert_info("Not making climate submission csv because we're in backtest mode or submission directory is cache")
      }
    },
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
          file_path = sprintf("CMU-climate_baseline/%s-CMU-climate_baseline.csv", get_forecast_reference_date(forecast_date_int))
        )
      } else {
        validation <- "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
      validation
    },
  ),
  tar_target(
    name = truth_data,
    command = {
      # Plot both as_of and latest data to compare
      nhsn_data <- nhsn_archive_data %>%
        epix_as_of(min(as.Date(forecast_generation_date_int), nhsn_archive_data$versions_end)) %>%
        mutate(source = "nhsn as_of forecast") %>%
        bind_rows(nhsn_latest_data %>% mutate(source = "nhsn")) %>%
        select(geo_value, target_end_date = time_value, value) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos)
      nssp_data <- nssp_latest_data %>%
        select(geo_value, target_end_date = time_value, value = nssp) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos) %>%
        mutate(target_end_date = target_end_date + 3, source = "nssp")
      nssp_renormalized <-
        nssp_data %>%
        left_join(
          nssp_data %>%
            rename(nssp = value) %>%
            full_join(
              nhsn_data %>%
                select(geo_value, target_end_date, value),
              by = join_by(geo_value, target_end_date)
            ) %>%
            group_by(geo_value) %>%
            summarise(rel_max_value = max(value, na.rm = TRUE) / max(nssp, na.rm = TRUE)),
          by = join_by(geo_value)
        ) %>%
        mutate(value = value * rel_max_value) %>%
        select(-rel_max_value)
      nhsn_data %>% bind_rows(nssp_renormalized)
    },
  ),
  tar_target(
    notebook,
    command = {
      # Only render the report if there is only one forecast date
      # i.e. we're running this in prod on schedule
      if (!g_backtest_mode) {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          forecast_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_covid_prod_on_%s.html", as.Date(forecast_date_int), as.Date(Sys.Date()))
          ),
          params = list(
            disease = "covid",
            forecast_res = forecasts_and_ensembles,
            forecast_date = as.Date(forecast_date_int),
            truth_data = truth_data
          )
        )
      }
    }
  )
)


# ================================ SCORE TARGETS ================================
if (g_backtest_mode) {
  score_targets <- list2(
    tar_change(
      external_forecasts,
      change = get_s3_object_last_modified(g_external_object_name, "forecasting-team-data"),
      command = {
        get_external_forecasts(g_external_object_name)
      }
    ),
    tar_combine(
      name = joined_forecasts_and_ensembles,
      ensemble_targets[["forecasts_and_ensembles"]],
      command = {
        local_together <- dplyr::bind_rows(!!!.x)
        # only seek to score on dates that are present both locally and in the
        # remote
        viable_dates <- inner_join(
          local_together %>%
            select(geo_value, forecast_date) %>%
            distinct() %>%
            arrange(forecast_date) %>%
            mutate(
              forecast_date = ceiling_date(forecast_date, unit = "week", week_start = 6)
            ),
          external_forecasts %>%
            select(geo_value, forecast_date) %>%
            distinct() %>% filter(forecast_date > "2024-10-01"),
          by = c("geo_value", "forecast_date")
        )
        dplyr::bind_rows(
          local_together %>%
            mutate(
              forecast_date = ceiling_date(forecast_date, unit = "week", week_start = 6)
            ) %>%
            inner_join(viable_dates, by = c("geo_value", "forecast_date")),
          external_forecasts %>%
            inner_join(viable_dates, by = c("geo_value", "forecast_date"))
        )
      }
    ),
    tar_target(
      name = scores,
      command = {
        score_forecasts(nhsn_latest_data, joined_forecasts_and_ensembles, "covid")
      }
    ),
    tar_target(
      name = score_plot,
      command = {
        render_score_plot(score_report_rmd, scores, g_forecast_dates, "covid")
      },
      cue = tar_cue("always")
    )
  )
} else {
  score_targets <- list()
}

list2(
  parameters_and_date_targets,
  forecast_targets,
  ensemble_targets,
  combined_forecasts,
  score_targets
)
