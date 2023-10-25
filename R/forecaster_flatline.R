#' flatline forecaster (aka baseline)
#' @description
#' a minimal forecaster whose median is just the last value
#' does not support `lags` as a parameter, but otherwise has the same parameters as `arx_forecaster`
#' @inheritParams scaled_pop
#' @importFrom rlang sym
#' @export
flatline_fc <- function(epi_data,
                        outcome,
                        extra_sources = "",
                        ahead = 1,
                        trainer = parsnip::linear_reg(),
                        quantile_levels = covidhub_probs(),
                        ...) {
  # perform any preprocessing not supported by epipredict
  # one that every forecaster will need to handle: how to manage max(time_value)
  # that's older than the `as_of` date
  epidataAhead <- extend_ahead(epi_data, ahead)
  # see latency_adjusting for other examples
  # this next part is basically unavoidable boilerplate you'll want to copy
  epi_data <- epidataAhead[[1]]
  effective_ahead <- epidataAhead[[2]]
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (confirm_insufficient_data(epi_data, effective_ahead, args_input)) {
    null_result <- tibble(
      geo_value = character(),
      forecast_date = Date(),
      target_end_date = Date(),
      quantile = numeric(),
      value = numeric()
    )
    return(null_result)
  }
  args_input[["ahead"]] <- effective_ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(flatline_args_list, args_input)
  # if you want to ignore extra_sources, setting predictors is the way to do it
  predictors <- c(outcome, extra_sources)
  argsPredictorsTrainer <- perform_sanity_checks(epi_data, outcome, predictors, NULL, args_list)
  args_list <- argsPredictorsTrainer[[1]]
  predictors <- argsPredictorsTrainer[[2]]
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict

  # since this is just the flatline, we don't need much of anything
  res <- flatline_forecaster(epi_data, outcome = outcome, args_list = args_list)
  true_forecast_date <- attributes(epi_data)$metadata$as_of
  pred <- format_storage(res$predictions, true_forecast_date)
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict e.g. calibration
  return(pred)
}
