#' storage format
#' @description
#' the columns for any stored prediction are
#' `(geo_value, forecast_date, target_end_date, quantile, value)`
#' in that particular order. It does not include the point estimate as a
#'   separate row.
#' @import magrittr dplyr epipredict
#' @export
format_storage <- function(pred, true_forecast_date, target_end_date) {
  pred %>%
    mutate(
      forecast_date = true_forecast_date,
      .dstn = nested_quantiles(.pred_distn)
    ) %>%
    unnest(.dstn) %>%
    select(-.pred_distn, -.pred, -time_value) %>%
    rename(quantile = tau, value = q, target_end_date = target_date) %>%
    relocate(geo_value, forecast_date, target_end_date, quantile, value)
}

#' format for the COVID-19 Forecast Hub
#' @description
#' expects the pred to have the columns geo_value, .pred, .pred_distn,
#'   forecast_date, and target_date
#' It does not assume that the forecast_date is accurate
#' The end result
format_covidhub <- function(pred, true_forecast_date, levels) {
  pred %<>%
    group_by(forecast_date, geo_value, target_date) %>%
    rename(target_end_date = target_date) %>%
    reframe(quantile = levels, value = quantile(.pred_distn, levels)[[1]])
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
