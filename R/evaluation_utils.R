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

#' evaluate_predictions wrapper
#' @description
#' run the measures on `data`, with truth data `evaluation_data`
#' @param data a prediction card to be scored
#' @param evaluation_data the true values
#' @param measures a set of scores to be used
#' @export
run_evaluation_measure <- function(data, evaluation_data, measures) {
  data %>%
    evaluate_predictions(
      evaluation_data,
      err_measures = measures,
      grp_vars = c(
        "signal",
        "geo_value",
        "forecast_date",
        "target_end_date"
      )
    )
}
