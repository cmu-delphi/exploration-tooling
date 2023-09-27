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

#' @export
run_evaluation_measure <- function(data, evaluation_data, measures) {
  data %>%
    evaluate_predictions(
      evaluation_data,
      err_measures = measures,
      grp_vars = c(
        "forecaster",
        "data_source",
        "signal",
        "geo_value",
        "forecast_date",
        "target_end_date",
        "ahead"
      )
    )
}
