#' Storage format
#'
#' The columns for any stored prediction are `(geo_value, forecast_date,
#' target_end_date, quantile, value)` in that particular order. It does not
#' include the point estimate as a separate row.
#'
#' @param pred the output as produced by epipredict
#' @param true_forecast_date the actual date from which the model is
#'   making the forecast, rather than the last day of available data
#' @param target_end_date the date of the prediction
#'
#' @importFrom epipredict nested_quantiles
#' @importFrom tidyr unnest
#' @export
format_storage <- function(pred, true_forecast_date, target_end_date) {
  pred %>%
    mutate(
      forecast_date = true_forecast_date,
      .dstn = nested_quantiles(.pred_distn)
    ) %>%
    unnest(.dstn) %>%
    select(-any_of(c(".pred_distn", ".pred", "time_value"))) %>%
    rename(quantile = quantile_levels, value = values, target_end_date = target_date) %>%
    relocate(geo_value, forecast_date, target_end_date, quantile, value)
}

#' Format for the COVID-19 Forecast Hub
#'
#' Expects the pred to have the columns geo_value, .pred, .pred_distn,
#' forecast_date, and target_date. It does not assume that the forecast_date is
#' accurate.
#'
#' @param pred the output as produced by epipredict
#' @param true_forecast_date the actual date from which the model is
#'   making the forecast, rather than the last day of available data
#' @param target_end_date the date of the prediction
#' @param quantile_levels the quantile levels
format_covidhub <- function(pred, true_forecast_date, target_end_date, quantile_levels) {
  pred %<>%
    group_by(forecast_date, geo_value, target_date) %>%
    rename(target_end_date = target_date) %>%
    reframe(quantile = quantile_levels, value = quantile(.pred_distn, quantile_levels)[[1]])
  forecasts$ahead <- ahead
  forecasts %<>%
    group_by(forecast_date, geo_value, target_date) %>%
    mutate(forecast_date = target_date - ahead) %>%
    rename(target_end_date = target_date) %>%
    reframe(
      quantile = quantiles,
      value = quantile(.pred_distn, quantiles)[[1]],
      ahead = ahead
    )
  forecasts$forecaster <- "epipredict_forecast"
  forecasts$incidence_period <- "day"
  forecasts$data_source <- source
  forecasts$signal <- signal
  epipredict_forecast <- rbind(epipredict_forecast, forecasts)
}
