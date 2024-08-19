# Scoring and Evaluation Functions

evaluate_predictions <- function(predictions_cards, truth_data) {
  checkmate::assert_data_frame(predictions_cards)
  checkmate::assert_data_frame(truth_data)
  checkmate::assert_names(
    names(predictions_cards),
    must.include = c("geo_value", "forecast_date", "target_end_date", "quantile", "prediction")
  )
  checkmate::assert_names(
    names(truth_data),
    must.include = c("geo_value", "target_end_date", "true_value")
  )

  left_join(predictions_cards, truth_data, by = c("geo_value", "target_end_date")) %>%
    scoringutils::score(metrics = c("interval_score", "ae_median")) %>%
    scoringutils::summarize_scores(by = c("forecast_date")) %>%
    rename(wis = interval_score, ae = ae_median)
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
