source("R/scoring_functions2.R")
library(assertthat)
library(magrittr)
library(tidyverse)


err_measures <- list(
  wis = weighted_interval_score,
  ae = absolute_error,
  coverage_80 = interval_coverage(coverage = 0.8)
  # value_50 = function(quantile, value, actual_value) {
  #   value[[match(0.5, quantile)]]
  # }
)

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

read_prediction_data <- function(predictions_filename, forecaster_name = NULL) {
  prediction_cards <- readRDS(predictions_filename)
  if (is.null(forecaster_name)) {
    return(prediction_cards)
  }
  prediction_cards %>%
    filter(forecast_date >= "2023-06-01") %>%
    filter(forecaster == forecaster_name)
}

run_evaluation_measure <- function(data, evaluation_data, measure) {
  data %>%
    evaluate_predictions(
      evaluation_data,
      err_measures = list(measure),
      grp_vars = c("forecaster", "data_source", "signal", "geo_value", "forecast_date", "target_end_date", "ahead")
    )
}
