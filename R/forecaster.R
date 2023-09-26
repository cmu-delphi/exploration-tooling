#' helper function for those writing forecasters
#' @description
#' a smorgasbord of checks that any epipredict-based forecaster should do:
#' 1. check that the args list is created correctly,
#' 2. validate the outcome and predictors as present,
#' 3. make sure the trainer is a `regression` model from `parsnip`
#' 4. remake the lags to match the numebr of predictors
#' 5. rewrite an empty extra sources list from an empty string
#' @inheritParams scaled_pop
#' @param args_list the args list created by [`epipredict::arx_args_list`]
#' @export
perform_sanity_checks <- function(epi_data,
                                  outcome,
                                  predictors,
                                  trainer,
                                  args_list) {
  # if "" has made its way into the list, throw it out, as it represents no
  # extra predictors
  predictors <- predictors[predictors != ""]
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `arx_args_list().")
  }
  # if (!(is.null(trainer) || epipredict:::is_regression(trainer))) {
  #   cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  # }
  args_list$lags <- epipredict:::arx_lags_validator(predictors, args_list$lags)
  return(list(args_list, predictors))
}

# TODO replace with `step_arx_forecaster`
#' add the default steps for arx_forecaster
#' @description
#' add the default steps for arx_forecaster
#' @param rec an [`epipredict::epi_recipe`]
#' @param outcome a character of the column to be predicted
#' @param predictors a character vector of the columns used as predictors
#' @param args_list an [`epipredict::arx_args_list`]
#' @seealso [arx_postprocess] for the layer equivalent
#' @export
arx_preprocess <- function(rec, outcome, predictors, args_list) {
  # input already validated
  lags <- args_list$lags
  for (l in seq_along(lags)) {
    p <- predictors[l]
    rec %<>% step_epi_lag(!!p, lag = lags[[l]])
  }
  rec %<>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead) %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training)
  return(rec)
}

# TODO replace with `layer_arx_forecaster`
#' add the default layers for arx_forecaster
#' @description
#' add the default layers for arx_forecaster
#' @param frost an [`epipredict::frosting`]
#' @param outcome a character of the column to be predicted
#' @param predictors a character vector of the columns used as predictors
#' @param args_list an [`epipredict::arx_args_list`]
#' @param forecast_date the date from which the forecast was made. defaults to
#'   the default of `layer_add_forecast_date`, which is currently the max
#'   time_value present in the data
#' @param target_date the date about which the forecast was made. defaults to
#'   the default of `layer_add_target_date`, which is either
#'   `forecast_date+ahead`, or the `max time_value + ahead`
#' @seealso [arx_preprocess] for the step equivalent
#' @export
arx_postprocess <- function(frost,
                            trainer,
                            args_list,
                            forecast_date = NULL,
                            target_date = NULL) {
  frost %<>% layer_predict()
  if (inherits(trainer, "quantile_reg")) {
    # add all levels to the forecaster and update postprocessor
    tau <- sort(epipredict:::compare_quantile_args(
      args_list$levels,
      rlang::eval_tidy(trainer$args$tau)
    ))
    args_list$levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
    frost %<>% layer_quantile_distn(levels = tau) %>% layer_point_from_distn()
  } else {
    frost %<>% layer_residual_quantiles(
      probs = args_list$levels, symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key
    )
  }
  frost %<>% layer_add_forecast_date(forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  if (args_list$nonneg) frost %<>% layer_threshold(dplyr::starts_with(".pred"))
  return(frost)
}

#' helper function to run a epipredict model and reformat to hub format
run_workflow_and_format <- function(preproc, postproc, trainer, epi_data) {
  workflow <- epi_workflow(preproc, trainer) %>%
    fit(epi_data) %>%
    add_frosting(postproc)
  latest <- get_test_data(recipe = preproc, x = epi_data)
  # TODO stop the constant warnings, we know its out of date
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
#' @export
forecaster_pred <- function(data,
                            outcome,
                            extra_sources = "",
                            forecaster = scaled_pop,
                            slide_training = Inf,
                            slide_training_pad = 20L,
                            ahead = 1,
                            trainer = parsnip::linear_reg(),
                            n_training = 32,
                            n_training_pad = 0,
                            ...) {
  archive <- data
  # restrict the dataset to areas where training is possible
  if (slide_training < Inf) {
    start_date <- min(archive$DT$time_value) + slide_training + slide_training_pad
  } else {
    start_date <- min(archive$DT$time_value) + slide_training_pad
  }
  end_date <- max(archive$DT$time_value) - ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = 1)
  browser()
  # first generate the forecasts
  # TODO forecaster probably needs a do.call
  res <- archive %>%
    group_by(geo_value) %>%
    epix_slide(
      function(data, gk, rtv, ...) {
        forecaster(
          epi_data = data,
          outcome = outcome,
          extra_sources = extra_sources,
          ahead = ahead,
          trainer = trainer,
          ...
        )
      },
      before = n_training + n_training_pad - 1,
      ref_time_values = valid_predict_dates,
      new_col_name = ".pred_distn",
    ) %>%
    ungroup()
  # TODO append the truth data
  return(res)
}
