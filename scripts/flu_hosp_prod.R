# The Flu Hospitalization Production Forecasting Pipeline.
source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
insufficient_data_geos <- c("as", "mp", "vi", "gu")
excluded_geos <- c("as", "gu", "mh")
# date to cut the truth data off at, so we don't have too much of the past
truth_data_date <- "2023-09-01"
# needed to create the aux data targets
end_date <- Sys.Date()

# This is the as_of for the forecast. If run on our typical schedule, it's
# today, which is a Wednesday. Sometimes, if we're doing a delayed forecast,
# it's a Thursday. It's used for stamping the data and for determining the
# appropriate as_of when creating the forecast.
forecast_generation_dates <- Sys.Date()
# Usually, the forecast_date is the same as the generation date, but you can
# override this. It should be a Wednesday.
forecast_dates <- round_date(forecast_generation_dates, "weeks", week_start = 3)
# If doing backfill, you can set the forecast_date to a sequence of dates.
# forecast_dates <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)
# forecast_generation_date needs to follow suit, but it's more complicated
# because sometimes we forecast on Thursday.
# forecast_generation_dates <- c(as.Date(c("2024-11-22", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")), seq.Date(as.Date("2025-01-08"), Sys.Date(), by = 7L))

# Whether we're running in backtest mode.
# If TRUE, we don't run the report notebook, which is (a) slow and (b) should be
# preserved as an ASOF snapshot of our production results for that week.
# If TRUE, we run a scoring notebook, which scores the historical forecasts
# against the truth data and compares them to the ensemble.
# If FALSE, we run the weekly report notebook.
backtest_mode <- length(forecast_dates) > 1

# Select first two for debugging
## forecast_generation_dates <- forecast_generation_dates[1:2]
## forecast_dates <- forecast_dates[1:2]

# needed for windowed_seasonal
very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))

forecaster_fns <- list2(
  linear = function(epi_data, ahead, extra_data, ...) {
    epi_data %>%
      filter(source == "nhsn") %>%
      forecaster_baseline_linear(
        ahead, ...,
        residual_tail = 0.99,
        residual_center = 0.35,
        no_intercept = TRUE
      )
  },
  # linearlog = function(...) {
  #   forecaster_baseline_linear(..., log = TRUE)
  # },
  climate_base = function(epi_data, ahead, extra_data, ...) {
    epi_data %>%
      filter(source == "nhsn") %>%
      climatological_model(ahead, ...)
  },
  climate_geo_agged = function(epi_data, ahead, extra_data, ...) {
    epi_data %>%
      filter(source == "nhsn") %>%
      climatological_model(ahead, ..., geo_agg = TRUE)
  },
  windowed_seasonal = function(epi_data, ahead, extra_data, ...) {
    scaled_pop_seasonal(
      epi_data,
      outcome = "value",
      ahead = ahead * 7,
      ...,
      trainer = epipredict::quantile_reg(),
      seasonal_method = "window",
      pop_scaling = FALSE,
      lags = c(0, 7),
      keys_to_ignore = very_latent_locations
    ) %>%
      mutate(target_end_date = target_end_date + 3)
  },
  windowed_seasonal_extra_sources = function(epi_data, ahead, extra_data, ...) {
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
        keys_to_ignore = very_latent_locations
      ) %>%
      select(-source) %>%
      mutate(target_end_date = target_end_date + 3) %>%
      filter(geo_value != c("mo", "us", "wy"))
    fcst
  }
)
forecaster_fn_names_ <- names(forecaster_fns)

# This is needed to build the data archive
ref_time_values_ <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7L)


# ================================ PARAMETERS AND DATE TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  rlang::list2(
    tar_target(aheads, command = -1:3),
    tar_target(forecasters, command = indices),
    tar_target(name = ref_time_values, command = ref_time_values_),
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
    )
  ),
  make_historical_flu_data_targets(),
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
        filter(geo_value %nin% insufficient_data_geos)
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
    tibble(forecaster_fn_names = forecaster_fn_names_),
    tibble(
      forecast_date_int = forecast_dates,
      forecast_generation_date_int = forecast_generation_dates,
      forecast_date_chr = as.character(forecast_dates)
    )
  ),
  names = c("forecaster_fn_names", "forecast_date_chr"),
  tar_target(
    full_data,
    command = {
      # Train data
      train_data <- nhsn_archive_data %>%
        epix_as_of(min(as.Date(forecast_date_int), nhsn_archive_data$versions_end)) %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value - 3,
          source = "nhsn"
        ) %>%
        data_substitutions(flu_data_substitutions, as.Date(forecast_generation_date_int)) %>%
        filter(geo_value %nin% insufficient_data_geos)
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
      nssp <- nssp_archive_data %>%
        epix_as_of(min(as.Date(forecast_date_int), nssp_archive_data$versions_end))

      full_data %>%
        forecaster_fns[[forecaster_fn_names]](ahead = aheads, extra_data = nssp) %>%
        mutate(
          forecaster = forecaster_fn_names,
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
    forecast_date_int = forecast_dates,
    forecast_generation_date_int = forecast_generation_dates,
    forecast_date_chr = as.character(forecast_dates)
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
      geo_forecasters_weights <- parse_prod_weights(flu_geo_exclusions, forecast_date_int, forecaster_fn_names_)
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
        ensemble_linear_climate(aheads, other_weights = geo_forecasters_weights) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup() %>%
        sort_by_quantile()
    }
  ),
  tar_target(
    name = ens_climate_linear_window_season,
    command = {
      climate_linear %>%
        mutate(forecaster = "climate_linear") %>%
        # Ensemble with windowed_seasonal and windowed_seasonal_extra_sources
        bind_rows(
          forecast_full_filtered %>%
            filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))
        ) %>%
        ensemble_weighted(geo_forecasters_weights)
    }
  ),
  tar_target(
    name = ens_ar_only,
    command = {
      forecast_full_filtered %>%
        filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
        group_by(geo_value, forecast_date, target_end_date, quantile) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        sort_by_quantile()
    }
  ),
  tar_target(
    name = forecasts_and_ensembles,
    command = {
      bind_rows(
        forecast_full_filtered,
        climate_linear %>% mutate(forecaster = "climate_linear"),
        ens_ar_only %>% mutate(forecaster = "ens_ar_only"),
        ens_climate_linear_window_season %>% mutate(forecaster = "ensemble_linclim_windowed_seasonal")
      )
    }
  ),
  tar_target(
    name = make_submission_csv,
    command = {
      if (!backtest_mode && submission_directory != "cache") {
        ens_climate_linear_window_season %>%
          format_flusight(disease = "flu") %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            file.path(submission_directory, "model-output/CMU-TimeSeries")
          )
      } else {
        cli_alert_info("Not making submission csv because we're in backtest mode or submission directory is cache")
      }
    }
  ),
  tar_target(
    name = make_climate_submission_csv,
    command = {
      if (!backtest_mode && submission_directory != "cache") {
        forecast_full_filtered %>%
          filter(forecaster %in% c("climate_base", "climate_geo_agged")) %>%
          group_by(geo_value, target_end_date, quantile) %>%
          summarize(forecast_date = as.Date(forecast_date_int), value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          ungroup() %>%
          filter(!(geo_value %in% excluded_geos)) %>%
          format_flusight(disease = "flu") %>%
          filter(location %nin% c("60", "66", "78")) %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            submission_directory = file.path(submission_directory, "model-output/CMU-climate_baseline"),
            file_name = "CMU-climate_baseline"
          )
      } else {
        cli_alert_info("Not making climate submission csv because we're in backtest mode or submission directory is cache")
      }
    },
    priority = 0.99
  ),
  tar_target(
    name = validate_result,
    command = {
      make_submission_csv
      # only validate if we're saving the result to a hub
      if (!backtest_mode && submission_directory != "cache") {
        validation <- validate_submission(
          submission_directory,
          file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(forecast_date_int))
        )
      } else {
        validation <- "not validating when there is no hub (set submission_directory)"
      }
      validation
    },
  ),
  tar_target(
    name = validate_climate_result,
    command = {
      make_climate_submission_csv
      # only validate if we're saving the result to a hub
      if (!backtest_mode && submission_directory != "cache") {
        validation <- validate_submission(
          submission_directory,
          file_path = sprintf("CMU-climate_baseline/%s-CMU-climate_baseline.csv", get_forecast_reference_date(forecast_date_int))
        )
      } else {
        validation <- "not validating when there is no hub (set submission_directory)"
      }
      validation
    },
  ),
  tar_target(
    name = truth_data,
    command = {
      date <- forecast_generation_date_int
      nssp_data <- nssp_latest_data %>%
        select(geo_value, target_end_date = time_value, value = nssp) %>%
        filter(target_end_date > truth_data_date, geo_value %nin% insufficient_data_geos) %>%
        mutate(target_end_date = target_end_date + 3, source = "nssp")
      nhsn_data <- nhsn_latest_data %>%
        select(geo_value, target_end_date = time_value, value) %>%
        filter(target_end_date > truth_data_date, geo_value %nin% insufficient_data_geos) %>%
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
      if (!backtest_mode) {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          forecast_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_prod_on_%s.html", as.Date(forecast_date_int), Sys.Date())
          ),
          params = list(
            disease = "flu",
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
if (backtest_mode) {
  score_targets <- list2(
    tar_target(
      external_forecasts,
      command = {
        get_external_forecasts("flu")
      }
    ),
    tar_combine(
      name = joined_forecasts_and_ensembles,
      ensemble_targets[["forecasts_and_ensembles"]],
      command = {
        dplyr::bind_rows(!!!.x, external_forecasts)
      }
    ),
    tar_target(
      name = scores,
      command = {
        nhsn_latest_end_of_week <-
          nhsn_latest_data %>%
          mutate(
            time_value = ceiling_date(time_value, unit = "week", week_start = 6)
          )
        score_forecasts(nhsn_latest_end_of_week, joined_forecasts_and_ensembles)
      }
    ),
    tar_target(
      name = score_plot,
      command = {
        render_score_plot(score_report_rmd, scores, forecast_dates, "flu")
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
