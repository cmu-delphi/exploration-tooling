# The COVID Hospitalization Production Forecasting Pipeline.
source("scripts/targets-common.R")

submission_directory <- Sys.getenv("COVID_SUBMISSION_DIRECTORY", "cache")
insufficient_data_geos <- c("as", "mp", "vi", "gu")
# date to cut the truth data off at, so we don't have too much of the past
truth_data_date <- "2023-09-01"

# This is the as_of for the forecast. If run on our typical schedule, it's
# today, which is a Wednesday. Sometimes, if we're doing a delayed forecast,
# it's a Thursday. It's used for stamping the data and for determining the
# appropriate as_of when creating the forecast.
forecast_generation_dates <- Sys.Date()
# Usually, the forecast_date is the same as the generation date, but you can
# override this. It should be a Wednesday.
forecast_dates <- round_date(forecast_generation_dates, "weeks", week_start = 3)
# forecast_generation_date needs to follow suit, but it's more complicated
# because sometimes we forecast on Thursday.
# forecast_generation_dates <- c(as.Date(c("2024-11-20", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")), seq.Date(as.Date("2025-01-08"), Sys.Date(), by = 7L))
# If doing backfill, you can set the forecast_date to a sequence of dates.
# forecast_dates <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)

# Whether we're running in backtest mode.
# If TRUE, we don't run the report notebook, which is (a) slow and (b) should be
# preserved as an ASOF snapshot of our production results for that week.
# If TRUE, we run a scoring notebook, which scores the historical forecasts
# against the truth data and compares them to the ensemble.
# If FALSE, we run the weekly report notebook.
backtest_mode <- length(forecast_dates) > 1

# Select first two for debugging
# forecast_generation_dates <- forecast_generation_dates[1:2]
# forecast_dates <- forecast_dates[1:2]


forecaster_fns <- list2(
  linear = function(epi_data, ahead, extra_data, ...) {
    forecaster_baseline_linear(epi_data, ahead, ..., residual_tail = 0.97, residual_center = 0.097, no_intercept = TRUE)
  },
  # linearlog = function(epi_data, ahead, extra_data, ...) {
  #   forecaster_baseline_linear(..., log = TRUE)
  # },
  climate_base = function(epi_data, ahead, extra_data, ...) {
    climatological_model(
      epi_data, ahead, ...,
    )
  },
  climate_geo_agged = function(epi_data, ahead, extra_data, ...) {
    climatological_model(
      epi_data, ahead, ...,
      geo_agg = TRUE
    )
  },
  windowed_seasonal = function(epi_data, ahead, extra_data, ...) {
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
        lags = list(c(0, 7), c(0, 7))
      ) %>%
      mutate(target_end_date = target_end_date + 3) %>%
      # Wyoming has no data for NSSP since July 2024
      filter(geo_value != "wy")
    fcst
  }
)
forecaster_fn_names_ <- names(forecaster_fns)


# ================================ PARAMETERS AND DATE TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  tar_target(aheads, command = -1:3),
  tar_change(
    nhsn_latest_data,
    change = get_socrata_updated_at("https://data.cdc.gov/api/views/mpgq-jmmr"),
    command = {
      if (wday(Sys.Date()) < 6 & wday(Sys.Date()) > 3) {
        # download from the preliminary data source from Wednesday to Friday
        most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
      } else {
        most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
      }
      most_recent_result <-
        most_recent_result %>%
        process_nhsn_data() %>%
        filter(disease == "nhsn_covid") %>%
        select(-disease) %>%
        filter(geo_value %nin% insufficient_data_geos)
      most_recent_result
    }
  ),
  tar_change(
    name = nhsn_archive_data,
    change = get_s3_object_last_modified("forecasting-team-data", "archive_timestamped.parquet"),
    command = {
      create_nhsn_data_archive(disease = "nhsn_covid")
    }
  ),
  # TODO: tar_change maybe?
  tar_target(
    current_nssp_archive,
    command = {
      up_to_date_nssp_state_archive("covid")
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
    name = forecast_res,
    command = {
      if (as.Date(forecast_generation_date_int) < Sys.Date()) {
        train_data <- nhsn_archive_data %>%
          epix_as_of(min(as.Date(forecast_date_int), current_nssp_archive$versions_end)) %>%
          add_season_info() %>%
          mutate(
            geo_value = ifelse(geo_value == "usa", "us", geo_value),
            time_value = time_value - 3
          )
      } else {
        train_data <-
          nhsn_latest_data %>%
          data_substitutions(disease = "covid", last(forecast_generation_dates)) %>%
          as_epi_df(as_of = as.Date(forecast_date_int)) %>%
          mutate(time_value = time_value - 3)
      }
      nssp <- current_nssp_archive %>%
        epix_as_of(min(as.Date(forecast_date_int), current_nssp_archive$versions_end)) %>%
        mutate(time_value = time_value)
      attributes(train_data)$metadata$as_of <- as.Date(forecast_date_int)
      train_data %>%
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
      geo_forecasters_weights <- parse_prod_weights(here::here("covid_geo_exclusions.csv"), forecast_date_int, forecaster_fn_names_)
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
    name = ensemble_res,
    command = {
      forecast_full_filtered %>%
        ensemble_linear_climate(
          aheads,
          other_weights = geo_forecasters_weights,
          max_climate_ahead_weight = 0.6,
          max_climate_quantile_weight = 0.6
        ) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup() %>%
        sort_by_quantile()
    },
  ),
  tar_target(
    name = ensemble_mixture_res,
    command = {
      forecast_full_filtered %>%
        ensemble_linear_climate(
          aheads,
          other_weights = geo_forecasters_weights,
          max_climate_ahead_weight = 0.6,
          max_climate_quantile_weight = 0.6
        ) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup() %>%
        bind_rows(forecast_full_filtered %>%
          filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
          filter(forecast_date < target_end_date)) %>% # don't use for neg aheads
        group_by(geo_value, forecast_date, target_end_date, quantile) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        sort_by_quantile()
    },
  ),
  tar_target(
    name = forecasts_and_ensembles,
    command = {
      bind_rows(
        forecast_full_filtered,
        ensemble_res %>% mutate(forecaster = "ensemble"),
        ensemble_mixture_res %>% mutate(forecaster = "ensemble_mix"),
        # TODO: Maybe later, match with flu_hosp_prod
        # ensemble_mixture_res_2 %>% mutate(forecaster = "ensemble_mix_2"),
        # combo_ensemble_mixture_res %>% mutate(forecaster = "combo_ensemble_mix")
      )
    }
  ),
  tar_target(
    name = make_submission_csv,
    command = {
      if (!backtest_mode && submission_directory != "cache") {
        forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
        ensemble_mixture_res %>%
          format_flusight(disease = "covid") %>%
          write_submission_file(forecast_reference_date, file.path(submission_directory, "model-output/CMU-TimeSeries"))
      } else {
        cli_alert_info("Not making submission csv because we're in backtest mode or submission directory is cache")
      }
    },
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
          format_flusight(disease = "covid") %>%
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
      nssp_state <- retry_fn(
        max_attempts = 20,
        wait_seconds = 2,
        fn = pub_covidcast,
        source = "nssp",
        signal = "pct_ed_visits_covid",
        time_type = "week",
        geo_type = "state",
        geo_values = "*",
        fetch_args = epidatr::fetch_args_list(timeout_seconds = 400)
      ) %>%
        select(geo_value, source, target_end_date = time_value, value) %>%
        filter(target_end_date > truth_data_date, geo_value %nin% insufficient_data_geos) %>%
        mutate(target_end_date = target_end_date + 6)
      truth_data <- nhsn_latest_data %>%
        mutate(target_end_date = time_value) %>%
        filter(time_value > truth_data_date) %>%
        mutate(source = "nhsn") %>%
        select(geo_value, target_end_date, source, value)
      nssp_renormalized <-
        nssp_state %>%
        left_join(
          nssp_state %>%
            rename(nssp = value) %>%
            full_join(
              truth_data %>%
                select(geo_value, target_end_date, value),
              by = join_by(geo_value, target_end_date)
            ) %>%
            group_by(geo_value) %>%
            summarise(rel_max_value = max(value, na.rm = TRUE) / max(nssp, na.rm = TRUE)),
          by = join_by(geo_value)
        ) %>%
        mutate(value = value * rel_max_value) %>%
        select(-rel_max_value)
      truth_data %>% bind_rows(nssp_renormalized)
    },
  ),
  tar_target(
    notebook,
    command = {
      # Only render the report if there is only one forecast date
      # i.e. we're running this in prod on schedule
      if (!backtest_mode) {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          "scripts/reports/forecast_report.Rmd",
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
if (backtest_mode) {
  score_targets <- list2(
    tar_target(
      external_forecasts,
      command = {
        locations_crosswalk <- get_population_data() %>%
          select(state_id, state_code) %>%
          filter(state_id != "usa")
        arrow::read_parquet("data/forecasts/covid_hosp_forecasts.parquet") %>%
          filter(output_type == "quantile") %>%
          select(forecaster, geo_value = location, forecast_date, target_end_date, quantile = output_type_id, value) %>%
          inner_join(locations_crosswalk, by = c("geo_value" = "state_code")) %>%
          mutate(geo_value = state_id) %>%
          select(forecaster, geo_value, forecast_date, target_end_date, quantile, value)
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
        truth_data <- nhsn_latest_data %>%
          select(geo_value, target_end_date = time_value, true_value = value)
        joined_forecasts_and_ensembles %>%
          rename("model" = "forecaster", "prediction" = "value") %>%
          evaluate_predictions(forecasts = ., truth_data = truth_data) %>%
          rename("forecaster" = "model")
      }
    ),
    tar_target(
      name = score_plot,
      command = {
        rmarkdown::render(
          "scripts/reports/score_report.Rmd",
          params = list(
            scores = scores,
            forecast_dates = forecast_dates,
            disease = "covid"
          ),
          output_file = here::here(
            "reports",
            sprintf("covid_backtesting_2024_2025_on_%s.html", as.Date(Sys.Date()))
          )
        )
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
