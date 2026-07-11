# Submission, validation, and report-notebook outputs for the flu pipelines.
#
# The write/validate/render bodies are extracted verbatim from the per-date
# ensemble targets in scripts/flu_hosp_prod.R, which gates them by mode
# (production: every run; evaluation: only the final forecast date, no notebook).
# NOTE: in cache mode (g_submission_directory == "cache") the callers no-op, so
# the output oracle cannot exercise the mode gates.

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

