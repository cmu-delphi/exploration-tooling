#' predict on smoothed data and the standard deviation
#' @description
#' This is a variant of `scaled_pop`, which predicts on a smoothed version of
#'   the data. Even if the target is smoothed when used as a /predictor/, as a
#'   /target/ it still uses the raw value (this captures some of the noise).  It
#'   also uses a rolling standard deviation as an auxillary signal, window of
#'   withd `sd_width`, which by default is 28 days.
#' @param epi_data the actual data used
#' @param outcome the name of the target variable
#' @param extra_sources the name of any extra columns to use. This list could be
#'   empty
#' @param ahead (this is relative to the `as_of` field of the `epi_df`, which is
#'   likely *not* the same as the `ahead` used by epipredict, which is relative
#'   to the max time value of the `epi_df`. how to handle this is a modelling
#'   question left up to each forecaster; see latency_adjusting.R for the
#'   existing examples)
#' @param pop_scaling an example extra parameter unique to this forecaster
#' @param trainer an example extra parameter that is fairly common
#' @param smooth_width the number of days over which to do smoothing. If `NULL`,
#'   then no smoothing is applied.
#' @param smooth_cols the names of the columns to smooth. If `NULL` it smooths
#'   everything
#' @param sd_width the number of days over which to take a moving average of the
#'   standard deviation. If `NULL`, the sd_width isn't included.
#' @param sd_mean_width to calculate the sd, we need a window size for the mean
#'   used.
#' @param sd_cols the names of the columns to smooth. If `NULL` its includes
#'   the sd of everything
#' @param quantile_levels The quantile levels to predict. Defaults to those
#' @param ... any additional arguments as used by [arx_args_list]
#'   required by covidhub.
#' @seealso some utilities for making forecasters: [format_storage],
#'   [perform_sanity_checks]
#' @importFrom epipredict epi_recipe step_population_scaling frosting arx_args_list layer_population_scaling
#' @importFrom tibble tibble
#' @importFrom recipes all_numeric
#' @export
smoothed_scaled <- function(epi_data,
                          outcome,
                          extra_sources = "",
                          ahead = 1,
                          pop_scaling = TRUE,
                          trainer = parsnip::linear_reg(),
                          quantile_levels = covidhub_probs(),
                          smooth_width = 7,
                          smooth_cols = NULL,
                          sd_width = 28,
                          sd_mean_width = 14,
                          sd_cols = NULL,
                          ...) {
  # perform any preprocessing not supported by epipredict
  # this is a temp fix until a real fix gets put into epipredict
  epi_data <- clear_lastminute_nas(epi_data)
  # one that every forecaster will need to handle: how to manage max(time_value)
  # that's older than the `as_of` date
  epidataAhead <- extend_ahead(epi_data, ahead)
  # see latency_adjusting for other examples
  # this next part is basically unavoidable boilerplate you'll want to copy
  epi_data <- epidataAhead[[1]]
  effective_ahead <- epidataAhead[[2]]
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, effective_ahead, args_input)) {
    null_result <- tibble(
      geo_value = character(),
      forecast_date = lubridate::Date(),
      target_end_date = lubridate::Date(),
      quantile = numeric(),
      value = numeric()
    )
    return(null_result)
  }
  args_input[["ahead"]] <- effective_ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(arx_args_list, args_input)
  # if you want to ignore extra_sources, setting predictors is the way to do it
  predictors <- c(outcome, extra_sources)
  # TODO: Partial match quantile_level coming from here (on Dmitry's machine)
  argsPredictorsTrainer <- perform_sanity_checks(epi_data, outcome, predictors, trainer, args_list)
  args_list <- argsPredictorsTrainer[[1]]
  predictors <- argsPredictorsTrainer[[2]]
  trainer <- argsPredictorsTrainer[[3]]
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  # smoothing
  keep_mean <- (smooth_width == sd_mean_width) # do we need to do the mean separately?
  if (!is.null(smooth_width) && !keep_mean) {
    epi_data %<>% rolling_mean(
      width = smooth_width,
      cols_to_mean = smooth_cols
    )
  }

  # measuring standard deviation
  if (!is.null(sd_width)) {
    epi_data %<>% rolling_sd(
      sd_width = sd_width,
      mean_width = sd_mean_width,
      cols_to_sd = sd_cols,
      keep_mean = keep_mean
    )
  }
  # and need to make sure we exclude the original varialbes as predictors
  predictors <- update_predictors(epi_data, c(smooth_cols, sd_cols), predictors)
  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  if (pop_scaling) {
    preproc %<>% step_population_scaling(
      all_numeric(),
      df = epipredict::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  preproc %<>% arx_preprocess(outcome, predictors, args_list)

  # postprocessing supported by epipredict
  postproc <- frosting()
  postproc %<>% arx_postprocess(trainer, args_list)
  if (pop_scaling) {
    postproc %<>% layer_population_scaling(
      .pred, .pred_distn,
      df = epipredict::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  # with all the setup done, we execute and format
  pred <- run_workflow_and_format(preproc, postproc, trainer, epi_data)
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict e.g. calibration
  return(pred)
}
