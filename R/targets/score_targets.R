get_external_forecasts <- function(external_object_name) {
  locations_crosswalk <- get_population_data() %>%
    select(state_id, state_code) %>%
    filter(state_id != "usa")
  s3read_using(
    nanoparquet::read_parquet,
    object = external_object_name,
    bucket = "forecasting-team-data"
  ) %>%
    filter(output_type == "quantile") %>%
    select(target, forecaster, geo_value = location, forecast_date, target_end_date, quantile = output_type_id, value) %>%
    inner_join(locations_crosswalk, by = c("geo_value" = "state_code")) %>%
    mutate(geo_value = state_id) %>%
    select(target, forecaster, geo_value, forecast_date, target_end_date, quantile, value)
}

score_forecasts <- function(latest_data, joined_forecasts_and_ensembles, target) {
  truth_data <-
    latest_data %>%
    select(geo_value, target_end_date = time_value, oracle_value = value) %>%
    left_join(
      get_population_data() %>%
        select(state_id, state_code),
      by = c("geo_value" = "state_id")
    ) %>%
    drop_na() %>%
    rename(location = state_code) %>%
    select(-geo_value)
  # limit the forecasts to the same set of forecasting times
  max_forecast_date <-
    joined_forecasts_and_ensembles %>%
    group_by(forecaster) %>%
    summarize(max_forecast = max(forecast_date)) %>%
    pull(max_forecast) %>%
    min()
  forecasts_formatted <-
    joined_forecasts_and_ensembles[joined_forecasts_and_ensembles$forecast_date <= max_forecast_date, ] %>%
    format_scoring_utils(target)
  scores <-
    forecasts_formatted %>%
    filter(output_type == "quantile") %>%
    filter(location %nin% c("US", "60", "66", "78")) %>%
    hubEvals::score_model_out(
      truth_data,
      metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_90"),
      summarize = TRUE,
      by = c("model_id", "target", "reference_date", "location", "horizon")
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
