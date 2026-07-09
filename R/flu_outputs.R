# Submission, validation, and report-notebook outputs for the flu pipelines.
#
# The write/validate/render bodies are extracted verbatim from the per-date
# ensemble targets. flu_output_targets() assembles the mode-specific tail of the
# ensemble tar_map with the g_backtest_mode gating already resolved per pipeline
# (REFACTOR.md: per-pipeline constant propagation):
#   - production writes/validates every run and renders the weekly notebook;
#   - backfill writes/validates only on the final forecast date and renders no
#     notebook (that render was dead in backtest mode).
# NOTE: in cache mode (g_submission_directory == "cache") these all no-op, so the
# output oracle cannot exercise the gates; their correctness is by construction
# (boolean constant fold of the original `!g_backtest_mode || <final date>`).

flu_write_submission <- function(ensemble_mixture, forecast_date_int) {
  forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
  nhsn_submission <- ensemble_mixture$nhsn %>%
    format_flusight(disease = "flu")
  nssp_submission <- ensemble_mixture$nssp %>%
    format_flusight(disease = "flu") %>%
    mutate(
      target = flu_report_target("nssp"),
      value = value * flu_report_scale("nssp")
    )
  bind_rows(nhsn_submission, nssp_submission) %>%
    write_submission_file(
      forecast_reference_date,
      file.path(g_submission_directory, "model-output/CMU-TimeSeries")
    )
}

flu_write_climate_submission <- function(forecast_filtered, forecast_date_int) {
  forecast_filtered$nhsn %>%
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
}

flu_validate_submission <- function(forecast_date_int) {
  validate_submission(
    g_submission_directory,
    file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(forecast_date_int))
  )
}

flu_validate_climate_submission <- function(forecast_date_int) {
  validate_submission(
    g_submission_directory,
    file_path = sprintf(
      "CMU-climate_baseline/%s-CMU-climate_baseline.csv",
      get_forecast_reference_date(forecast_date_int)
    )
  )
}

flu_render_forecast_notebook <- function(forecasts_and_ensembles, truth_data, forecast_date_int, forecast_report_rmd) {
  if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
  rmarkdown::render(
    forecast_report_rmd,
    output_file = here::here(
      "reports",
      sprintf("%s_flu_prod_on_%s.html", as.Date(forecast_date_int), as.Date(Sys.Date()))
    ),
    params = list(
      disease = "flu",
      forecast_nhsn = forecasts_and_ensembles$nhsn %>% ungroup() %>% filter(forecaster %in% c("cdc_baseline", "climate_linear", "ensemble_mix", "windowed_seasonal", "windowed_seasonal_extra_sources")),
      forecast_nssp = forecasts_and_ensembles$nssp %>% ungroup() %>% filter(forecaster %in% c("cdc_baseline", "climate_linear", "ensemble_mix", "windowed_seasonal", "windowed_seasonal_extra_sources")),
      forecast_date = as.Date(forecast_date_int),
      truth_data_nhsn = truth_data$nhsn,
      truth_data_nssp = truth_data$nssp
    )
  )
}

# Mode-specific tail of the ensemble tar_map. Returns a list of tar_target
# objects; spliced into the tar_map by build_flu_prod_pipeline(), so their
# per-date branch references (forecast_date_int, ensemble_mixture, ...) are
# substituted by tar_map exactly as the inline definitions were.
flu_output_targets <- function(backtest_mode) {
  if (backtest_mode) {
    # Backfill: write/validate only on the final forecast date; no notebook.
    list(
      tar_target(
        name = make_submission_csv,
        command = if (g_submission_directory != "cache" && as.Date(forecast_date_int) == max(g_forecast_dates)) {
          flu_write_submission(ensemble_mixture, forecast_date_int)
        } else {
          cli_alert_info("Not writing submission (cache dir or non-final backfill date)")
        },
        cue = tar_cue("always")
      ),
      tar_target(
        name = make_climate_submission_csv,
        command = if (g_submission_directory != "cache" && as.Date(forecast_date_int) == max(g_forecast_dates)) {
          flu_write_climate_submission(forecast_filtered, forecast_date_int)
        } else {
          cli_alert_info("Not writing climate submission (cache dir or non-final backfill date)")
        },
        cue = tar_cue("always")
      ),
      tar_target(
        name = validate_result,
        command = {
          make_submission_csv
          if (g_submission_directory != "cache" && as.Date(forecast_date_int) == max(g_forecast_dates)) {
            flu_validate_submission(forecast_date_int)
          } else {
            "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
          }
        }
      ),
      tar_target(
        name = validate_climate_result,
        command = {
          make_climate_submission_csv
          if (g_submission_directory != "cache" && as.Date(forecast_date_int) == max(g_forecast_dates)) {
            flu_validate_climate_submission(forecast_date_int)
          } else {
            "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
          }
        }
      )
    )
  } else {
    # Production: write/validate every run, render the weekly report notebook.
    list(
      tar_target(
        name = make_submission_csv,
        command = if (g_submission_directory != "cache") {
          flu_write_submission(ensemble_mixture, forecast_date_int)
        } else {
          cli_alert_info("Not writing submission (cache dir)")
        },
        cue = tar_cue("always")
      ),
      tar_target(
        name = make_climate_submission_csv,
        command = if (g_submission_directory != "cache") {
          flu_write_climate_submission(forecast_filtered, forecast_date_int)
        } else {
          cli_alert_info("Not writing climate submission (cache dir)")
        },
        cue = tar_cue("always")
      ),
      tar_target(
        name = validate_result,
        command = {
          make_submission_csv
          if (g_submission_directory != "cache") {
            flu_validate_submission(forecast_date_int)
          } else {
            "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
          }
        }
      ),
      tar_target(
        name = validate_climate_result,
        command = {
          make_climate_submission_csv
          if (g_submission_directory != "cache") {
            flu_validate_climate_submission(forecast_date_int)
          } else {
            "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
          }
        }
      ),
      tar_target(
        name = notebook,
        command = flu_render_forecast_notebook(forecasts_and_ensembles, truth_data, forecast_date_int, forecast_report_rmd)
      )
    )
  }
}
