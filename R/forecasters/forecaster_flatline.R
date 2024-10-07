#' Flatline forecaster (aka baseline)
#'
#' A minimal forecaster whose median is just the last value does not support
#' `lags` as a parameter, but otherwise has the same parameters as
#' `arx_forecaster`.
#'
#' @inheritParams scaled_pop
#' @importFrom rlang sym
#' @importFrom epipredict flatline_forecaster flatline_args_list
#' @export
flatline_fc <- function(epi_data,
                        outcome,
                        extra_sources = "",
                        ahead = 1,
                        trainer = parsnip::linear_reg(),
                        quantile_levels = covidhub_probs(),
                        ...) {
  # perform any preprocessing not supported by epipredict
  # this is a temp fix until a real fix gets put into epipredict
  epi_data <- clear_lastminute_nas(epi_data, outcome, extra_sources)
  # this next part is basically unavoidable boilerplate you'll want to copy
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, ahead, args_input, outcome, extra_sources)) {
    null_result <- tibble(
      geo_value = character(),
      forecast_date = lubridate::Date(),
      target_end_date = lubridate::Date(),
      quantile = numeric(),
      value = numeric()
    )
    return(null_result)
  }
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(flatline_args_list, args_input)
  # since this is a flatline forecaster, it can't use other predictors, so we remove them
  epi_data <- epi_data %>% select(all_of(c("geo_value", "time_value", outcome, attributes(epi_data)$metadata$other_keys)))
  # fixing any weirdness in the args_list and trainer
  predictors <- c(outcome)
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, NULL, args_list)
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict

  # since this is just the flatline, we don't need much of anything
  res <- flatline_forecaster(epi_data, outcome = outcome, args_list = args_list)
  true_forecast_date <- attributes(epi_data)$metadata$as_of
  if (is.null(true_forecast_date)) {
    true_forecast_date <- max(epi_data$time_value)
  }
  pred <- format_storage(res$predictions, true_forecast_date)
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict e.g. calibration
  return(pred)
}
