# Scoring and Evaluation Functions

evaluate_predictions <- function(forecasts, truth_data) {
  checkmate::assert_data_frame(forecasts)
  checkmate::assert_data_frame(truth_data)
  checkmate::assert_names(
    names(forecasts),
    must.include = c("model", "geo_value", "forecast_date", "target_end_date", "quantile", "prediction")
  )
  checkmate::assert_names(
    names(truth_data),
    must.include = c("geo_value", "target_end_date", "true_value")
  )

  forecast_obj <- left_join(forecasts, truth_data, by = c("geo_value", "target_end_date")) %>%
    scoringutils::as_forecast_quantile(
      quantile_level = "quantile",
      observed = "true_value",
      predicted = "prediction",
      forecast_unit = c("model", "geo_value", "forecast_date", "target_end_date")
    )
  # browser()
  # # Find where the forecasts are not monotonically increasing as a function of quantile_level
  # df <- forecast_obj %>%
  #   group_by(model, geo_value, forecast_date, target_end_date) %>%
  #   mutate(monotonic = all(diff(predicted) >= 0)) %>%
  #   ungroup() %>%
  #   filter(!monotonic)
  # df %>% print(n=100)

  scores <- forecast_obj %>%
    scoringutils::score(metrics = get_metrics(.)) %>%
    as_tibble()
  if (
    length(setdiff(
      c("model", "geo_value", "forecast_date", "target_end_date", "wis", "ae", "coverage_50", "coverage_90"),
      names(scores)
    )) >
      0
  ) {
    stop("scoring error, quantile monotonicity likely violated")
  }
  scores %>%
    select(
      model,
      geo_value,
      forecast_date,
      target_end_date,
      wis,
      ae = ae_median,
      coverage_50 = interval_coverage_50,
      coverage_90 = interval_coverage_90
    ) %>%
    mutate(ahead = as.numeric(target_end_date - forecast_date))
}


#' import prediction cards generated elsewhere
#' @description
#' load an externally generated RDS to be evaluated
#' @param predictions_filename the filename to be read as an RDS
#' @param forecaster_name the name to assign the forecaster
#' @export
read_external_predictions_data <- function(predictions_filename, forecaster_name = NULL) {
  prediction_cards <- readRDS(predictions_filename)
  if (is.null(forecaster_name)) {
    return(prediction_cards)
  }
  prediction_cards %>%
    filter(forecast_date >= "2023-06-01") %>%
    filter(forecaster == forecaster_name)
}
