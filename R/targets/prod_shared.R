# Shared helpers for the production forecasting pipelines (covid_hosp_prod, flu_hosp_prod).
# Functions prefixed build_* return target lists and depend on g_* globals defined in
# the calling script.

g_baseline_forecaster <- function(epi_data, ahead, extra_data, ...) {
  if (ahead < 3) {
    # Same schema as the populated path below: the old `quantile_value` name
    # here leaked a spurious all-NA column into forecast_*_full via bind_rows.
    return(tibble(
      geo_value = character(), forecast_date = Date(),
      target_end_date = Date(), quantile = numeric(), value = numeric()
    ))
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
  fcst
}

# Returns a tar_map covering all forecast dates, fetching and scoring external
# forecasts from S3. Depends on g_s3_prefix, g_disease, nhsn_latest_data,
# nssp_latest_data targets.
build_external_forecast_targets <- function() {
  tar_map(
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
      command = get_external_forecasts(filename)
    ),
    tar_target(
      name = score_external_nhsn_forecasts,
      command = score_forecasts(
        nhsn_latest_data, external_forecasts,
        paste0("wk inc ", g_disease, " hosp")
      )
    ),
    tar_target(
      name = score_external_nssp_forecasts,
      command = score_forecasts(
        nssp_latest_data %>% mutate(value = nssp),
        external_forecasts,
        paste0("wk inc ", g_disease, " prop ed visits")
      )
    )
  )
}

# Returns the per-date ensemble/submission/report tar_map shared by the flu and
# covid prod pipelines. The per-disease asymmetries are named arguments here
# rather than inline literals buried in two hand-copied tar_maps, so drift
# between the pipelines is visible at the call sites. Everything else in the
# commands is shared verbatim. Depends on targets and g_* globals defined in
# the calling script: aheads, forecast_nhsn_full / forecast_nssp_full,
# g_forecaster_params_grid, g_submission_directory, g_evaluation_mode,
# g_forecast_dates, g_truth_data_date, g_insufficient_data_geos,
# nhsn_archive_data, nhsn_latest_data, nssp_latest_data, forecast_report_rmd,
# and the geo-exclusion file targets named by *_geo_exclusions_file.
#
# Arguments (all per-disease config; substituted into the commands as literals
# via tar_map values, so no run-time global carries them):
# - geo_exclusions_file / nssp_geo_exclusions_file: bare target names (strings)
#   of the hand-edited weights csv file targets.
# - ensemble_spec: named list of the three ensemble rows (climate_linear,
#   ens_ar_only, ensemble_mix; see g_ensemble_specs in scripts/*_hosp_prod.R
#   and run_ensemble() in R/targets/ensemble_runner.R for the field meanings).
#   Spliced whole as a list-column, same as the other per-disease values here:
#   it is disease-constant (not per forecast-date), so this embeds it as a
#   literal in every branch's frozen command rather than a run-time global.
# - climate_submission_excluded_geos: geos dropped from the standalone
#   CMU-climate_baseline submission (flu only; historically g_excluded_geos).
build_prod_ensemble_targets <- function(
  forecast_schedule,
  disease,
  geo_exclusions_file,
  nssp_geo_exclusions_file,
  ensemble_spec,
  climate_submission_excluded_geos = character(0)
) {
  values <- forecast_schedule %>%
    mutate(
      disease = .env$disease,
      geo_exclusions_file = rlang::syms(.env$geo_exclusions_file),
      nssp_geo_exclusions_file = rlang::syms(.env$nssp_geo_exclusions_file),
      ensemble_spec = list(.env$ensemble_spec),
      climate_excluded_geos = list(.env$climate_submission_excluded_geos)
    )
  tar_map(
    values = values,
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
          nhsn = make_weights(geo_exclusions_file),
          nssp = make_weights(nssp_geo_exclusions_file)
        )
      }
    ),
    # Derived from the nhsn weights file but applied to both signals' ensembles
    # (pre-existing behavior in both pipelines, possibly a copy-paste artifact;
    # kept verbatim -- adjudicate when the ensemble layer gets its spec).
    tar_target(
      name = geo_exclusions,
      command = exclude_geos(geo_weights$nhsn)
    ),
    tar_target(
      name = ensemble_clim_lin,
      command = {
        spec <- ensemble_spec$climate_linear
        run_clim_lin <- function(forecasts, signal, weights) {
          run_ensemble(
            method = spec$method,
            id = spec$id,
            forecasts = forecasts,
            components = spec$components[[signal]],
            aheads = aheads,
            weights = weights,
            climate_caps = spec$climate_caps[[signal]],
            geo_exclusions = if (spec$apply_geo_exclusions) geo_exclusions else NULL,
            sort_quantiles = spec$sort_quantiles
          )
        }
        list(
          nhsn = run_clim_lin(forecast_filtered$nhsn, "nhsn", geo_weights$nhsn),
          nssp = run_clim_lin(forecast_filtered$nssp, "nssp", geo_weights$nssp)
        )
      }
    ),
    tar_target(
      name = ens_ar_only,
      command = {
        spec <- ensemble_spec$ar_only
        run_ensemble(
          method = spec$method,
          id = spec$id,
          forecasts = forecast_filtered$nhsn,
          components = spec$components$nhsn,
          geo_exclusions = if (spec$apply_geo_exclusions) geo_exclusions else NULL,
          sort_quantiles = spec$sort_quantiles
        )
      }
    ),
    tar_target(
      name = ensemble_mixture,
      command = {
        spec <- ensemble_spec$ensemble_mix
        run_mix <- function(forecasts, signal, weights, clim_lin) {
          run_ensemble(
            method = spec$method,
            id = spec$id,
            forecasts = forecasts,
            components = spec$components[[signal]],
            weights = weights,
            geo_exclusions = if (spec$apply_geo_exclusions) geo_exclusions else NULL,
            drop_negative_aheads = spec$drop_negative_aheads[[signal]],
            extra_forecasts = clim_lin,
            sort_quantiles = spec$sort_quantiles
          )
        }
        list(
          nhsn = run_mix(forecast_filtered$nhsn, "nhsn", geo_weights$nhsn, ensemble_clim_lin$nhsn),
          nssp = run_mix(forecast_filtered$nssp, "nssp", geo_weights$nssp, ensemble_clim_lin$nssp)
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
        if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
          forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
          nhsn_submission <- ensemble_mixture$nhsn %>%
            format_flusight(disease = disease)
          nssp_submission <- ensemble_mixture$nssp %>%
            format_flusight(disease = disease) %>%
            mutate(
              target = paste0("wk inc ", disease, " prop ed visits"),
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
        if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
          forecast_filtered$nhsn %>%
            filter(forecaster %in% c("climate_base", "climate_geo_agged")) %>%
            group_by(geo_value, target_end_date, quantile) %>%
            summarize(forecast_date = as.Date(forecast_date_int), value = mean(value, na.rm = TRUE), .groups = "drop") %>%
            ungroup() %>%
            filter(!(geo_value %in% climate_excluded_geos)) %>%
            format_flusight(disease = disease) %>%
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
        if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
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
        if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
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
        if (!g_evaluation_mode) {
          if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
          rmarkdown::render(
            forecast_report_rmd,
            output_file = here::here(
              "reports",
              sprintf("%s_%s_prod_on_%s.html", as.Date(forecast_date_int), disease, as.Date(Sys.Date()))
            ),
            params = list(
              disease = disease,
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
}

# Combines per-forecaster forecast_nhsn and forecast_nssp tar_map outputs into
# single forecast_nhsn_full / forecast_nssp_full targets.
build_combined_forecast_targets <- function(forecast_targets) {
  list(
    tar_combine(
      name = forecast_nhsn_full,
      forecast_targets[["forecast_nhsn"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_combine(
      name = forecast_nssp_full,
      forecast_targets[["forecast_nssp"]],
      command = dplyr::bind_rows(!!!.x)
    )
  )
}

# Combines per-date external forecast / score tar_map outputs and assembles
# final scores_nhsn / scores_nssp by joining with local scores.
build_combined_targets <- function(external_forecast_targets) {
  list2(
    tar_combine(
      name = external_forecasts_full,
      external_forecast_targets[["external_forecasts"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_combine(
      name = external_scores_nhsn_full,
      external_forecast_targets[["score_external_nhsn_forecasts"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_combine(
      name = external_scores_nssp_full,
      external_forecast_targets[["score_external_nssp_forecasts"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_target(
      name = scores_nhsn,
      command = bind_rows(external_scores_nhsn_full, local_scores_nhsn)
    ),
    tar_target(
      name = scores_nssp,
      command = bind_rows(external_scores_nssp_full, local_scores_nssp)
    )
  )
}

# Returns score plot targets for backtest mode. Depends on g_disease, g_forecast_dates,
# scores_nhsn, scores_nssp, and score_report_rmd targets.
build_backtest_score_targets <- function() {
  list2(
    tar_target(
      name = score_nhsn_plot,
      command = render_score_plot(score_report_rmd, scores_nhsn, g_forecast_dates, g_disease, "nhsn"),
      cue = tar_cue("always")
    ),
    tar_target(
      name = score_nssp_plot,
      command = render_score_plot(score_report_rmd, scores_nssp, g_forecast_dates, g_disease, "nssp"),
      cue = tar_cue("always")
    )
  )
}
