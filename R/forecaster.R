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
#' predict, na omit, threshold and add dates
#' @description
#' add some basic layers that make sure a prediction is present, that the values
#'   in the prediction are thresholded to be positive, and that the default
#'   forecast date and target date are available as columns.
#' @param frost an [`epipredict::frosting`]
#' @param forecast_date the date from which the forecast was made. defaults to
#'   the default of `layer_add_forecast_date`, which is currently the max
#'   time_value present in the data
#' @param target_date the date about which the forecast was made. defaults to
#'   the default of `layer_add_target_date`, which is either
#'   `forecast_date+ahead`, or the `max time_value + ahead`
#' @import epipredict
#' @export
arx_basics <- function(frost, forecast_date = NULL, target_date = NULL, nonneg = TRUE) {
  frost %>%
    layer_predict() %>%
    layer_naomit(.pred)
  frost %<>% layer_add_forecast_date(forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  if (nonneg) {
    frost %<>% layer_threshold(dplyr::starts_with(".pred"))
  }
  return(frost)
}

# TODO replace with layer version
#' make sure to have a quantile forecast
#' @description
#' dispatch on the trainer to make sure that the right kind of quantiles are added
#' @param frost an [`epipredict::frosting`]
#' @param args_list an [`epipredict::arx_args_list`]
#' @seealso [arx_preprocess] for the step equivalent
#' @export
add_quantiles <- function(frost,
                          trainer,
                          args_list) {
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
  return(frost)
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
#' @param forecaster a function that does the actual forecasting for a given
#'   day. See `exampleSpec.R` for an example function and its documentation for
#'   the general parameter requirements.
#' @param slide_training a required parameter that governs the window size that
#'   epix_slide hands off to epipredict. Note that
#' @param slide_training_pad a required parameter that determines how much extra
#'   to hand-off to guarantee that at least `slide_training` examples are passed
#'   on (e.g. b/c of missing data).
#' @param trainer should be given as a string, which will be converted to a
#'   function.
#' @param ahead a necessary parameter to specify an experiment
#' @param ... any extra parameters the user has defined for forecaster.
#' @import rlang epipredict
#' @export
forecaster_pred <- function(data,
                            outcome,
                            extra_sources = "",
                            forecaster = scaled_pop,
                            slide_training = Inf,
                            slide_training_pad = 20L,
                            n_training = 32,
                            n_training_pad = 0,
                            forecaster_args = list(),
                            forecaster_args_names = list()) {
  archive <- data
  if (length(forecaster_args) > 0) {
    names(forecaster_args) <- forecaster_args_names
  }
  # restrict the dataset to areas where training is possible
  if (slide_training < Inf) {
    start_date <- min(archive$DT$time_value) + slide_training + slide_training_pad
  } else {
    start_date <- min(archive$DT$time_value) + slide_training_pad
  }
  end_date <- max(archive$DT$time_value) - forecaster_args$ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = 1)
  # first generate the forecasts
  res <- epix_slide(archive,
    function(data, gk, rtv, ...) {
      do.call(
        forecaster,
        append(
          list(
            epi_data = data,
            outcome = outcome,
            extra_sources = extra_sources
          ),
          forecaster_args
        )
      )
    },
    before = n_training + n_training_pad - 1,
    ref_time_values = valid_predict_dates,
  )
  res %<>% select(-time_value)
  names(res) <- sub("^slide_value_", "", names(res))

  # append the truth data
  true_value <- archive$as_of(archive$versions_end) %>%
    select(geo_value, time_value, outcome) %>%
    rename(true_value = !!outcome)
  res %<>%
    inner_join(true_value,
      by = join_by(geo_value, target_end_date == time_value)
    )
  return(res)
}
