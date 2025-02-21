get_external_forecasts <- function(disease) {
  locations_crosswalk <- get_population_data() %>%
    select(state_id, state_code) %>%
    filter(state_id != "usa")
  arrow::read_parquet(glue::glue("data/forecasts/{disease}_hosp_forecasts.parquet")) %>%
    filter(output_type == "quantile") %>%
    select(forecaster, geo_value = location, forecast_date, target_end_date, quantile = output_type_id, value) %>%
    inner_join(locations_crosswalk, by = c("geo_value" = "state_code")) %>%
    mutate(geo_value = state_id) %>%
    select(forecaster, geo_value, forecast_date, target_end_date, quantile, value)
}

score_forecasts <- function(nhsn_latest_data, joined_forecasts_and_ensembles) {
  truth_data <-
    nhsn_latest_data %>%
    select(geo_value, target_end_date = time_value, oracle_value = value) %>%
    left_join(
      get_population_data() %>%
        select(state_id, state_code),
      by = c("geo_value" = "state_id")
    ) %>%
    drop_na() %>%
    rename(location = state_code) %>%
    select(-geo_value)
  forecasts_formatted <-
    joined_forecasts_and_ensembles %>%
    format_scoring_utils(disease = "covid")
  scores <- forecasts_formatted %>%
    filter(location != "US") %>%
    hubEvals::score_model_out(
      truth_data,
      metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_90"),
      summarize = FALSE,
      by = c("model_id")
    )
  scores %>%
    left_join(
      get_population_data() %>%
        select(state_id, state_code),
      by = c("location" = "state_code")
    ) %>%
    rename(
      forecaster = model_id,
      forecast_date = reference_date,
      ahead = horizon,
      geo_value = state_id
    ) %>%
    select(-location)
}


render_score_plot <- function(score_report_rmd, scores, forecast_dates, disease) {
  rmarkdown::render(
    score_report_rmd,
    params = list(
      scores = scores,
      forecast_dates = forecast_dates,
      disease = disease
    ),
    output_file = here::here(
      "reports",
      glue::glue("{disease}_backtesting_2024_2025_on_{as.Date(Sys.Date())}")
    )
  )
}
