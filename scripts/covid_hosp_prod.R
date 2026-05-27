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
g_insufficient_data_geos_nssp <- c(g_insufficient_data_geos, "wy")
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "covid"
g_s3_prefix <- "exploration"
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
  # the forecast is actually for the wednesday beforehand for these days
  if (Sys.Date() %in% as.Date(c("2025-12-29"))) {
    g_forecast_dates <- as.Date("2025-12-24")
  }
} else {
  g_forecast_generation_dates <- c(
    as.Date(c("2024-11-20", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
    seq.Date(as.Date("2025-01-08"), as.Date("2025-12-17"), by = 7L),
    as.Date(c("2025-12-29")),
    seq.Date(as.Date("2025-12-31"), Sys.Date(), by = 7L)
  )
  # Every Wednesday since mid-Nov 2024
  g_forecast_dates <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)
}

# Forecaster grid — function definitions live in R/covid_prod_forecasters.R.
ids <- c(
  "cdc_baseline",
  "linear",
  "linear_no_population_scale",
  "windowed_seasonal",
  "windowed_seasonal_extra_sources",
  "climate_base",
  "climate_geo_agged",
  "windowed_seasonal_latest",
  "seasonal_nssp_latest"
)
g_forecaster_params_grid <- tibble(
  id = ids,
  forecaster = rlang::syms(c(
    "g_baseline_forecaster",
    "g_covid_linear",
    "g_covid_linear_no_population_scale",
    "g_covid_windowed_seasonal",
    "g_covid_windowed_seasonal_extra_sources",
    "g_covid_climate_base",
    "g_covid_climate_geo_agged",
    "g_covid_windowed_seasonal",
    "g_covid_windowed_seasonal_extra_sources"
  )),
  params = vector("list", length(ids)),
  param_names = vector("list", length(ids))
)


# ================================ PARAMETERS AND DATA TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  tar_target(aheads, command = g_aheads),
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
    name = covid_geo_exclusions,
    command = "scripts/covid_geo_exclusions.csv"
  ),
  tar_file(
    name = covid_nssp_geo_exclusions,
    command = "scripts/covid_nssp_geo_exclusions.csv"
  ),
  tar_file(
    name = covid_data_substitutions,
    command = "scripts/covid_data_substitutions.csv"
  ),
  tar_change(
    name = nhsn_archive_data,
    change = get_cast_api_latest_update_date(source = "nhsn"),
    command = {
      get_nhsn_data_archive("covid")
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
    change = get_cast_api_latest_update_date(source = "nssp"),
    command = {
      up_to_date_nssp_state_archive("covid")
    }
  ),
  tar_target(
    name = nssp_latest_data,
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
    name = forecast_nhsn,
    command = {
      # if the forecaster is named latest, it should use the most up to date
      # version of the data
      if (grepl("latest", id)) {
        nhsn_data <- nhsn_archive_data %>%
          epix_as_of(nhsn_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_date_int))
        nssp_data <- nssp_archive_data %>%
          epix_as_of(nssp_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_date_int))
      } else {
        nhsn_data <- nhsn_archive_data %>%
          epix_as_of(min(as.Date(forecast_generation_date_int), nhsn_archive_data$versions_end))
        nssp_data <- nssp_archive_data %>%
          epix_as_of(min(as.Date(forecast_generation_date_int), nssp_archive_data$versions_end))
      }
      nhsn_data <- nhsn_data %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = floor_date(time_value, "week", week_start = 7) + 3
        ) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
      if (!grepl("latest", id)) {
        nhsn_data %<>%
          data_substitutions(covid_data_substitutions, as.Date(forecast_generation_date_int))
      }
      attributes(nhsn_data)$metadata$as_of <- as.Date(forecast_date_int)

      # trim the cdc_baseline data to avoid larger quantile data
      if (id == "cdc_baseline") {
        nhsn_data <- nhsn_data %>% filter(time_value >= as.Date("2024-11-09"))
      }

      forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads, params, param_names)

      forecaster_fn(nhsn_data, extra_data = nssp_data) %>%
        mutate(
          forecaster = id,
          geo_value = as.factor(geo_value)
        )
    },
    pattern = map(aheads)
  ),
  tar_target(
    name = forecast_nssp,
    command = {
      # if the forecaster is named latest, it should use the most up to date
      # version of the data
      if (grepl("latest", id)) {
        nhsn_data <- nhsn_archive_data %>%
          epix_as_of(nhsn_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_date_int))
        nssp_data <- nssp_archive_data %>%
          epix_as_of(nssp_archive_data$versions_end) %>%
          filter(time_value < as.Date(forecast_date_int))
      } else {
        nhsn_data <- nhsn_archive_data %>%
          epix_as_of(min(as.Date(forecast_date_int), nhsn_archive_data$versions_end))
        nssp_data <- nssp_archive_data %>%
          epix_as_of(min(as.Date(forecast_date_int), nssp_archive_data$versions_end))
      }
      nssp_data <- nssp_data %>%
        rename(value = nssp) %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = floor_date(time_value, "week", week_start = 7) + 3
        ) %>%
        filter(geo_value %nin% g_insufficient_data_geos_nssp)

      if (!grepl("latest", id)) {
        nhsn_data %<>%
          data_substitutions(covid_data_substitutions, as.Date(forecast_generation_date_int))
      }

      # jank renaming to avoid hard-coded variable name problems
      nhsn_data %<>%
        rename(nssp = value) %>%
        mutate(
          time_value = floor_date(time_value, "week", week_start = 7) + 3
        )
      attributes(nssp_data)$metadata$as_of <- as.Date(forecast_date_int)

      # trim the cdc_baseline data to avoid larger quantile data
      if (id == "cdc_baseline") {
        nssp_data <- nssp_data %>% filter(time_value >= as.Date("2024-11-09"))
      }

      forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads, params, param_names)

      forecaster_fn(nssp_data, extra_data = nhsn_data) %>%
        mutate(
          forecaster = id,
          geo_value = as.factor(geo_value)
        )
    },
    pattern = map(aheads)
  )
)

combined_forecast_targets <- build_combined_forecast_targets(forecast_targets)


# ================================ ENSEMBLE TARGETS ================================
ensemble_targets <- tar_map(
  values = tibble(
    forecast_date_int = g_forecast_dates,
    forecast_generation_date_int = g_forecast_generation_dates,
    forecast_date_chr = as.character(g_forecast_dates)
  ),
  names = "forecast_date_chr",
  tar_target(
    name = forecast_filtered,
    command = list(
      nhsn = forecast_nhsn_full %>%
        filter(forecast_date == as.Date(forecast_date_int)) %>%
        filter(forecaster %nin% c("linear_no_population_scale")),
      nssp = forecast_nssp_full %>%
        filter(forecast_date == as.Date(forecast_date_int)) %>%
        filter(forecaster %nin% c("linear"))
    )
  ),
  tar_target(
    name = geo_weights,
    command = {
      make_weights <- function(excl_file) {
        w <- parse_prod_weights(excl_file, forecast_date_int, g_forecaster_params_grid$id)
        if (nrow(w %>% filter(forecast_date == as.Date(forecast_date_int))) == 0) {
          cli_abort("there are no weights for the forecast date {forecast_date}")
        }
        w
      }
      list(
        nhsn = make_weights(covid_geo_exclusions),
        nssp = make_weights(covid_nssp_geo_exclusions)
      )
    }
  ),
  tar_target(
    name = geo_exclusions,
    command = exclude_geos(geo_weights$nhsn)
  ),
  tar_target(
    name = ensemble_clim_lin,
    command = {
      make_clim_lin <- function(forecasts, weights) {
        forecasts %>%
          ensemble_climate_linear(
            aheads,
            other_weights = weights,
            max_climate_ahead_weight = 0.6,
            max_climate_quantile_weight = 0.6
          ) %>%
          filter(geo_value %nin% geo_exclusions) %>%
          ungroup() %>%
          sort_by_quantile() %>%
          mutate(forecaster = "climate_linear")
      }
      list(
        nhsn = make_clim_lin(forecast_filtered$nhsn, geo_weights$nhsn),
        nssp = make_clim_lin(forecast_filtered$nssp, geo_weights$nssp)
      )
    }
  ),
  tar_target(
    name = ens_ar_only,
    command = {
      forecast_filtered$nhsn %>%
        filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
        group_by(geo_value, forecast_date, target_end_date, quantile) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        sort_by_quantile() %>%
        mutate(forecaster = "ens_ar_only")
    }
  ),
  tar_target(
    name = ensemble_mixture,
    command = {
      ar_nhsn <- forecast_filtered$nhsn %>%
        filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
        filter(forecast_date < target_end_date) # covid nhsn: drop neg aheads from AR
      ar_nssp <- forecast_filtered$nssp %>%
        filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))
      list(
        nhsn = ensemble_clim_lin$nhsn %>%
          bind_rows(ar_nhsn) %>%
          ensemble_weighted(geo_weights$nhsn) %>%
          mutate(forecaster = "ensemble_mix"),
        nssp = ensemble_clim_lin$nssp %>%
          bind_rows(ar_nssp) %>%
          ensemble_weighted(geo_weights$nssp) %>%
          mutate(forecaster = "ensemble_mix")
      )
    }
  ),
  tar_target(
    name = forecasts_and_ensembles,
    command = list(
      nhsn = bind_rows(forecast_filtered$nhsn, ensemble_clim_lin$nhsn, ensemble_mixture$nhsn, ens_ar_only),
      nssp = bind_rows(forecast_filtered$nssp, ensemble_clim_lin$nssp, ensemble_mixture$nssp)
    )
  ),
  tar_target(
    name = make_submission_csv,
    command = {
      if (g_submission_directory != "cache" && (!g_backtest_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
        forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
        nhsn_submission <- ensemble_mixture$nhsn %>%
          format_flusight(disease = "covid")
        nssp_submission <- ensemble_mixture$nssp %>%
          format_flusight(disease = "covid") %>%
          mutate(
            target = "wk inc covid prop ed visits",
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
      if (g_submission_directory != "cache" && (!g_backtest_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
        forecast_filtered$nhsn %>%
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
      if (g_submission_directory != "cache" && (!g_backtest_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
        validate_submission(
          g_submission_directory,
          file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(forecast_date_int))
        )
      } else {
        "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
    }
  ),
  tar_target(
    name = validate_climate_result,
    command = {
      make_climate_submission_csv
      if (g_submission_directory != "cache" && (!g_backtest_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
        validate_submission(
          g_submission_directory,
          file_path = sprintf(
            "CMU-climate_baseline/%s-CMU-climate_baseline.csv",
            get_forecast_reference_date(forecast_date_int)
          )
        )
      } else {
        "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
    }
  ),
  tar_target(
    name = truth_data,
    command = {
      nhsn_raw <- nhsn_archive_data %>%
        epix_as_of(min(as.Date(forecast_generation_date_int), nhsn_archive_data$versions_end)) %>%
        mutate(source = "nhsn as_of forecast") %>%
        bind_rows(nhsn_latest_data %>% mutate(source = "nhsn")) %>%
        select(geo_value, target_end_date = time_value, value, source) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos)
      nssp_raw <- nssp_latest_data %>%
        select(geo_value, target_end_date = time_value, value = nssp) %>%
        filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos) %>%
        mutate(target_end_date = target_end_date + 3, source = "nssp")
      normalize_to_primary <- function(primary, secondary) {
        rel_max <- secondary %>%
          rename(sec = value) %>%
          full_join(primary %>% select(geo_value, target_end_date, value), by = join_by(geo_value, target_end_date)) %>%
          group_by(geo_value) %>%
          summarise(scale = max(value, na.rm = TRUE) / max(sec, na.rm = TRUE))
        secondary %>%
          left_join(rel_max, by = join_by(geo_value)) %>%
          mutate(value = value * scale) %>%
          select(-scale) %>%
          bind_rows(primary, .)
      }
      list(
        nhsn = normalize_to_primary(nhsn_raw, nssp_raw),
        nssp = normalize_to_primary(nssp_raw, nhsn_raw)
      )
    }
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
            sprintf("%s_covid_prod_on_%s.html", as.Date(forecast_date_int), as.Date(Sys.Date()))
          ),
          params = list(
            disease = "covid",
            forecast_nhsn = forecasts_and_ensembles$nhsn %>% ungroup() %>% filter(forecaster %in% c("cdc_baseline", "climate_linear", "ensemble_mix", "windowed_seasonal", "windowed_seasonal_extra_sources")),
            forecast_nssp = forecasts_and_ensembles$nssp %>% ungroup() %>% filter(forecaster %in% c("cdc_baseline", "climate_linear", "ensemble_mix", "windowed_seasonal", "windowed_seasonal_extra_sources")),
            forecast_date = as.Date(forecast_date_int),
            truth_data_nhsn = truth_data$nhsn,
            truth_data_nssp = truth_data$nssp
          )
        )
      }
    }
  )
)


# ================================ SCORE TARGETS ================================
external_forecast_targets <- build_external_forecast_targets()

joined_targets <- list2(
  tar_combine(
    name = local_forecasts_and_ensembles_nhsn,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = purrr::map(list(!!!.x), "nhsn") %>% dplyr::bind_rows()
  ),
  tar_combine(
    name = local_forecasts_and_ensembles_nssp,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = purrr::map(list(!!!.x), "nssp") %>% dplyr::bind_rows()
  ),
  tar_target(
    name = local_scores_nhsn,
    command = {
      score_forecasts(nhsn_latest_data, local_forecasts_and_ensembles_nhsn, "wk inc covid hosp")
    }
  ),
  tar_target(
    name = local_scores_nssp,
    command = {
      nssp_latest_data %>%
        rename(value = nssp) %>%
        mutate(time_value = ceiling_date(time_value, unit = "week") - 1) %>%
        score_forecasts(local_forecasts_and_ensembles_nssp %>% mutate(value = value / 100), "wk inc covid prop ed visits")
    }
  )
)

combined_targets <- build_combined_targets(external_forecast_targets)

if (g_backtest_mode) {
  score_notebook <- build_backtest_score_targets()
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
            target == "wk inc covid hosp"
          ) %>% distinct(forecast_date) %>% nrow() == 0) {
          return()
        }
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_covid_nhsn_scoring.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "covid",
            target = "nhsn",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc covid hosp") %>% select(-target),
            archive = nhsn_archive_data,
            scores = external_scores_nhsn_full
          )
        )
      }
    ),
    tar_target(
      ongoing_nssp_score_notebook,
      command = {
        if (!dir.exists(here::here("reports"))) {
          dir.create(here::here("reports"))
        }
        # Don't run if there aren't forecasts in the past 4 weeks to evaluate
        if (external_forecasts_full %>%
          filter(
            forecast_date >= round_date(Sys.Date() - 3, "week", 6) - 4 * 7,
            target == "wk inc covid prop ed visits"
          ) %>% distinct(forecast_date) %>% nrow() == 0) {
          return()
        }
        # Score notebook individual average (see ongoing_score_report_rmd for documentation)
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_covid_nssp_scoring_individual.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "covid",
            target = "nssp",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc covid prop ed visits") %>% select(-target),
            archive = nssp_archive_data,
            scores = external_scores_nssp_full,
            averaging_method = "individual"
          )
        )
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_covid_nssp_scoring_common.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "covid",
            target = "nssp",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc covid prop ed visits") %>% select(-target),
            archive = nssp_archive_data,
            scores = external_scores_nssp_full,
            averaging_method = "common"
          )
        )
      }
    ),
  )
}

list2(
  parameters_and_date_targets,
  forecast_targets,
  ensemble_targets,
  combined_forecast_targets,
  external_forecast_targets,
  combined_targets,
  joined_targets,
  score_notebook
)
