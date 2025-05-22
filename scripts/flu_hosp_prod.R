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
g_excluded_geos <- c("as", "gu", "mh")
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "flu"
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
} else {
  g_forecast_generation_dates <- c(as.Date(c("2024-11-21", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")), seq.Date(as.Date("2025-01-08"), Sys.Date(), by = 7L))
  g_forecast_dates <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)
}

# Forecaster definitions
g_linear <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter(source == "nhsn") %>%
    forecaster_baseline_linear(
      ahead, ...,
      residual_tail = 0.99,
      residual_center = 0.35,
      no_intercept = TRUE
    )
}
g_climate_base <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter(source == "nhsn") %>%
    climatological_model(ahead, ...)
}
g_climate_geo_agged <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter(source == "nhsn") %>%
    climatological_model(ahead, ..., geo_agg = TRUE)
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
    filter(geo_value %nin% c("mo", "us", "wy"))
  fcst
}
g_forecaster_params_grid <- tibble(
  id = c("linear", "windowed_seasonal", "windowed_seasonal_extra_sources", "climate_base", "climate_geo_agged", "seasonal_nssp_latest"),
  forecaster = rlang::syms(c("g_linear", "g_windowed_seasonal", "g_windowed_seasonal_extra_sources", "g_climate_base", "g_climate_geo_agged", "g_windowed_seasonal_extra_sources")),
  params = list(
    list(),
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
    list(),
    list()
  )
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
    score_report_rmd,
    command = "scripts/reports/score_report.Rmd"
  ),
  tar_file(
    flu_geo_exclusions,
    command = "flu_geo_exclusions.csv"
  ),
  tar_file(
    flu_data_substitutions,
    command = "flu_data_substitutions.csv"
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
    nhsn_latest_data,
    command = {
      nhsn_archive_data %>%
        epix_as_of(min(Sys.Date(), nhsn_archive_data$versions_end)) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
    }
  ),
  tar_change(
    nssp_archive_data,
    change = get_covidcast_signal_last_update("nssp", "pct_ed_visits_influenza", "state"),
    command = {
      up_to_date_nssp_state_archive("influenza")
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
    full_data,
    command = {
      # Train data
      if (grepl("latest", id)) {
        train_data <- nhsn_archive_data %>%
          epix_as_of(nhsn_archive_data$versions_end) %>% filter(time_value < as.Date(forecast_date_int))
      } else {
      train_data <- nhsn_archive_data %>%
        epix_as_of(min(as.Date(forecast_date_int), nhsn_archive_data$versions_end))
      }
    train_data %<>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value - 3,
          source = "nhsn"
        )
    if (!grepl("latest", id)) {
    train_data %<>% data_substitutions(
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
    name = forecast_res,
    command = {
      if (grepl("latest", id)) {
        nssp_data <- nssp_archive_data %>%
          epix_as_of(nssp_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_date_int))
      } else {
        nssp_data <- nssp_archive_data %>%
          epix_as_of(min(as.Date(forecast_date_int), nssp_archive_data$versions_end))
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
      geo_forecasters_weights <- parse_prod_weights(flu_geo_exclusions, forecast_date_int, g_forecaster_params_grid$id)
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
    name = climate_linear,
    command = {
      forecast_full_filtered %>%
        # Apply the ahead-by-quantile weighting scheme
        ensemble_climate_linear(aheads, other_weights = geo_forecasters_weights) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup() %>%
        sort_by_quantile() %>%
        mutate(forecaster = "climate_linear")
    }
  ),
  tar_target(
    name = ens_climate_linear_window_season,
    command = {
      climate_linear %>%
        # Ensemble with windowed_seasonal and windowed_seasonal_extra_sources
        bind_rows(
          forecast_full_filtered %>%
            filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))
        ) %>%
        ensemble_weighted(geo_forecasters_weights) %>%
        mutate(forecaster = "ensemble_linclim_windowed_seasonal")
    }
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
    name = forecasts_and_ensembles,
    command = {
      bind_rows(
        forecast_full_filtered,
        climate_linear,
        ens_ar_only,
        ens_climate_linear_window_season
      )
    }
  ),
  tar_target(
    name = make_submission_csv,
    command = {
      if (!g_backtest_mode && g_submission_directory != "cache") {
        ens_climate_linear_window_season %>%
          format_flusight(disease = "flu") %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            file.path(g_submission_directory, "model-output/CMU-TimeSeries")
          )
      } else {
        cli_alert_info("Not making submission csv because we're in backtest mode or submission directory is cache")
      }
    }
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
          filter(!(geo_value %in% g_excluded_geos)) %>%
          format_flusight(disease = "flu") %>%
          filter(location %nin% c("60", "66", "78")) %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            file.path(g_submission_directory, "model-output/CMU-climate_baseline"),
            file_name = "CMU-climate_baseline"
          )
      } else {
        cli_alert_info("Not making climate submission csv because we're in backtest mode or submission directory is cache")
      }
    }
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
    # This target is time dependent for plotting.
    name = truth_data,
    command = {
      # Ths might be just to make the target depend on the forecast_generation_date_int
      date <- forecast_generation_date_int
      nssp_data <- nssp_latest_data %>%
        select(geo_value, target_end_date = time_value, value = nssp) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos) %>%
        mutate(target_end_date = target_end_date + 3, source = "nssp")
      nhsn_data <- nhsn_latest_data %>%
        select(geo_value, target_end_date = time_value, value) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos) %>%
        mutate(source = "nhsn")
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
      if (!g_backtest_mode) {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          forecast_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_prod_on_%s.html", as.Date(forecast_date_int), Sys.Date())
          ),
          params = list(
            disease = "flu",
            forecast_res = forecasts_and_ensembles%>% ungroup() %>% filter(forecaster != "climate_geo_agged"),
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
        filter_shared_geo_dates(
          dplyr::bind_rows(!!!.x),
          external_forecasts
        )
      }
    ),
    tar_target(
      name = scores,
      command = {
        browser()
        nhsn_latest_end_of_week <-
          nhsn_latest_data %>%
          mutate(
            time_value = round_date(time_value, unit = "week", week_start = 6)
          )
        score_forecasts(nhsn_latest_end_of_week, joined_forecasts_and_ensembles, "flu")
      }
    ),
    tar_target(
      name = score_plot,
      command = {
        render_score_plot(score_report_rmd, scores, g_forecast_dates, "flu")
      }
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
