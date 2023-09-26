#' @export
read_evaluation_data <- function(evaluation_filename) {
  evaluation_data <- readRDS(evaluation_filename)
  evaluation_data %<>%
    filter(data_source == "hhs" & signal == "confirmed_admissions_covid_1d") %>%
    {
      assert_that(nrow(.) == 1L)
      .
    } %>%
    {
      .[["signal.df"]][[1L]]
    } %>%
    rename(
      actual = value,
      target_end_date = time_value
    ) %>%
    as_tibble()
}

#' @export
read_prediction_data <- function(predictions_filename, forecaster_name = NULL) {
  prediction_cards <- readRDS(predictions_filename)
  if (is.null(forecaster_name)) {
    return(prediction_cards)
  }
  prediction_cards %>%
    filter(forecast_date >= "2023-06-01") %>%
    filter(forecaster == forecaster_name)
}

#' @export
run_evaluation_measure <- function(data, evaluation_data, measure) {
  data %>%
    evaluate_predictions(
      evaluation_data,
      err_measures = list(measure),
      grp_vars = c("forecaster", "data_source", "signal", "geo_value", "forecast_date", "target_end_date", "ahead")
    )
}

#' @export
download_data <- function(src, signal, start_date, end_date) {
  data <- pub_covidcast(
    src,
    signals = signal,
    geo_type = "state",
    time_type = "day",
    time_values = epirange(from = start_date, to = end_date),
    geo_values = "*",
    issues = "*",
    fetch_args = fetch_args_list(return_empty = TRUE, timeout_seconds = 100)
  )
}
