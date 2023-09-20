library(epipredict)

#' helper function for those writing forecasters
#' @description
perform_sanity_checks <- function(epi_data,
                                  outcome,
                                  predictors,
                                  trainer,
                                  args_list) {
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `arx_args_list().")
  }
  if (!(is.null(trainer) || epipredict:::is_regression(trainer))) {
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  }
  args_list$lags <- epipredict:::arx_lags_validator(predictors, args_list$lags)
  # only needed an empty string to prevent the tibble from going crazy
  if (extra_sources == c("")) {
    extra_sources <- c()
  }
  return(args_list)
}

# TODO replace with `step_arx_forecaster`
arx_preprocess <- function(r, outcome, predictors, args_list) {
  # input already validated
  args_list <- perform_sanity_checks(trainer, args_list)
  lags <- args_list$lags
  for (l in seq_along(lags)) {
    p <- predictors[l]
    r %<>% step_epi_lag(!!p, lag = lags[[l]])
  }
  r %<>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead) %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training)
  return(r)
}

# TODO replace with `layer_arx_forecaster`
arx_postprocess <- function(f,
                            trainer,
                            args_list,
                            forecast_date,
                            target_date) {
  f %<>% layer_predict()
  if (inherits(trainer, "quantile_reg")) {
    # add all levels to the forecaster and update postprocessor
    tau <- sort(epipredict:::compare_quantile_args(
      args_list$levels,
      rlang::eval_tidy(trainer$args$tau)
    ))
    args_list$levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
    f %<>% layer_quantile_distn(levels = tau) %>% layer_point_from_distn()
  } else {
    f %<>% layer_residual_quantiles(
      probs = args_list$levels, symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key
    )
  }
  f %<>% layer_add_forecast_date(forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  if (args_list$nonneg) f %<>% layer_threshold(dplyr::starts_with(".pred"))
  return(f)
}

#' helper function to run a epipredict model and reformat to hub format
run_workflow_and_format <- function(preproc, postproc, trainer, epi_data) {
  workflow <- epi_workflow(preproc, trainer) %>%
    fit(epi_data) %>%
    add_frosting(postproc)
  latest <- get_test_data(recipe = preproc, x = epi_data)
  pred <- predict(workflow, latest)
  # the forecast_date may currently be the max time_value
  true_forecast_date <- attributes(epi_data)$metadata$as_of
  return(format_storage(pred, true_forecast_date))
}

#' generate forecaster predictions on a particular dataset
#' @description
#' a wrapper that turns a forecaster, parameters, data
#' combination into an actual experiment that outputs a
#' prediction.
#' as far as batchtools is concerned, the scoring function is a particular
#'   parameter of the forecaster (or Algorithm, as they call it).
#' @param data as per batchtools
#' @param job as per batchtools
#' @param instance as per batchtools
#' @param scoring_function TODO detailed spec
#' @param forecaster a function that does the actual forecasting for a given
#'   day. See `exampleSpec.R` for an example function and its documentation for
#'   the general parameter requirements.
#' @param slide_training a required parameter that governs the window size that
#'   epix_slide hands off to epipredict.
#' @param slide_training_pad a required parameter that determines padding
#' @param trainer should be given as a string, which will be converted to a
#'   function.
#' @param ahead a necessary parameter to specify an experiment
#' @param ... any extra parameters the user has defined for forecaster.
forecaster_pred <- function(data,
                            job,
                            instance,
                            forecaster=scaled_pop,
                            slide_training = Inf,
                            slide_training_pad = 20L,
                            trainer = "linear_reg",
                            ahead = 1,
                            ...) {
  archive <- instance$archive
  outcome <- instance$outcome
  extra_sources <- instance$extra_sources
  output_format <- instance$output_formatter
  # restrict the dataset to areas where training is possible
  start_date <- min(archive$DT$time_value) + slide_training + slide_training_pad
  end_date <- max(archive$DT$time_value) - ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = 1)
  # TODO maybe allow for other params that are actually functions
  trainer <- match.fun(trainer)
  # first generate the forecasts
  # TODO forecaster probably needs a do.call
  res <- epix_slide(
    archive,
    ~ forecaster(
      ahead,
      .x,
      trainer,
      ... # TODO update to fit the spec
    ),
    before = n_training + n_training_pad - 1,
    ref_time_values = valid_predict_dates,
    new_col_name = ".pred_distn",
  )
  # TODO append the truth data
  return(res)
}
