#' helper function for those writing forecasters
#' @description
#' a smorgasbord of checks that any epipredict-based forecaster should do:
#' 1. check that the args list is created correctly,
#' 2. rewrite an empty extra sources list from an empty string
#' 3. validate the outcome and predictors as present,
#' 4. make sure the trainer is a `regression` model from `parsnip`
#' 5. adjust the trainer's quantiles based on those in args_list if it's a
#'    quantile trainer
#' 6. remake the lags to match the numebr of predictors
#' @inheritParams scaled_pop
#' @param predictors the full list of predictors including the outcome. can
#'   include empty strings
#' @param args_list the args list created by [`epipredict::arx_args_list`]
#' @export
perform_sanity_checks <- function(epi_data,
                                  outcome,
                                  predictors,
                                  trainer,
                                  args_list) {
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `arx_args_list().")
  }

  predictors <- predictors[predictors != ""]
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)

  if (!is.null(trainer) && !epipredict:::is_regression(trainer)) {
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  } else if (inherits(trainer, "quantile_reg")) {
    # add all levels to the trainer and update args list
    tau <- sort(epipredict:::compare_quantile_args(
      args_list$levels,
      rlang::eval_tidy(trainer$args$tau)
    ))
    args_list$levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
  }
  args_list$lags <- epipredict:::arx_lags_validator(predictors, args_list$lags)
  return(list(args_list, predictors, trainer))
}

#' confirm that there's enough data to run this model
#' @description
#' epipredict is a little bit fragile about having enough data to train; we want
#'   to be able to return a null result rather than error out; this check say to
#'   return a null
#' @param buffer how many training data to insist on having (e.g. if `buffer=1`,
#'   this trains on one sample; the default is set so that `linear_reg` isn't
#'   rank deficient)
#' @export
confirm_insufficient_data <- function(epi_data, ahead, args_input, buffer = 9) {
  if (!is.null(args_input$lags)) {
    lag_max <- max(args_input$lags)
  } else {
    lag_max <- 14
  }
  return(
    is.infinite(ahead) ||
      as.integer(max(epi_data$time_value) - min(epi_data$time_value)) <=
        lag_max + ahead + 9
  )
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
#' @param postproc an [`epipredict::frosting`]
#' @param trainer the trainer used (e.g. linear_reg() or quantile_reg())
#' @param args_list an [`epipredict::arx_args_list`]
#' @param forecast_date the date from which the forecast was made. defaults to
#'   the default of `layer_add_forecast_date`, which is currently the max
#'   time_value present in the data
#' @param target_date the date about which the forecast was made. defaults to
#'   the default of `layer_add_target_date`, which is either
#'   `forecast_date+ahead`, or the `max time_value + ahead`
#' @seealso [arx_preprocess] for the step equivalent
#' @export
arx_postprocess <- function(postproc,
                            trainer,
                            args_list,
                            forecast_date = NULL,
                            target_date = NULL) {
  postproc %<>% layer_predict()
  if (inherits(trainer, "quantile_reg")) {
    postproc %<>% layer_quantile_distn(levels = args_list$levels) %>% layer_point_from_distn()
  } else {
    postproc %<>% layer_residual_quantiles(
      probs = args_list$levels, symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key
    )
  }
  if (args_list$nonneg) {
    postproc %<>% layer_threshold(dplyr::starts_with(".pred"))
  }

  postproc %<>% layer_naomit(dplyr::starts_with(".pred"))
  postproc %<>% layer_add_forecast_date(forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  return(postproc)
}

#' helper function to run a epipredict model and reformat to hub format
#' @description
#' helper function to run a epipredict model and reformat to hub format
#' @param preproc the preprocessing steps
#' @param postproc the postprocessing frosting
#' @param trainer the parsnip trainer
#' @param epi_data the actual epi_df to train on
#' @export
#' @import epipredict recipes
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
#' @param data the epi_df object
#' @param outcome the name of the target column
#' @param extra_sources any extra columns used for prediction that aren't
#'   the target
#' @param forecaster a function that does the actual forecasting for a given
#'   day. See `exampleSpec.R` for an example function and its documentation
#'   for the general parameter requirements.
#' @param slide_training a required parameter that governs how much data to
#'   exclude before starting the evaluation.
#' @param n_training_pad a required parameter that determines how many extra
#'   samples for epix_slide to hand to the forecaster to guarantee that at
#'   least `ntraining` examples are available to the forecaster.
#' @param forecaster_args the list of arguments to the forecaster; it must
#'   contain `ahead`
#' @param forecaster_args_names a bit of a hack around targets, it contains
#'   the names of the `forecaster_args`.
#' @import rlang epipredict dplyr
#' @importFrom epiprocess epix_slide
#' @export
forecaster_pred <- function(data,
                            outcome,
                            extra_sources = "",
                            forecaster = scaled_pop,
                            slide_training = 0,
                            n_training_pad = 5,
                            forecaster_args = list(),
                            forecaster_args_names = list()) {
  archive <- data
  if (length(forecaster_args) > 0) {
    names(forecaster_args) <- forecaster_args_names
  }
  if (is.null(forecaster_args$ahead)) {
    cli::cli_abort(
      c(
        "exploration-tooling error: forecaster_pred needs some value for ahead."
      ),
      class = "explorationToolingError"
    )
  }
  if (!is.numeric(forecaster_args$n_training) && !is.null(forecaster_args$n_training)) {
    n_training <- as.numeric(forecaster_args$n_training)
    net_slide_training <- max(slide_training, n_training) + n_training_pad
  } else {
    n_training <- Inf
    net_slide_training <- slide_training + n_training_pad
  }
  # restrict the dataset to areas where training is possible
  start_date <- min(archive$DT$time_value) + net_slide_training
  end_date <- max(archive$DT$time_value) - forecaster_args$ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = 1)

  # first generate the forecasts
  before <- n_training + n_training_pad - 1
  ## TODO epix_slide doesn't support infinite `before`
  ## https://github.com/cmu-delphi/epiprocess/issues/219
  if (before == Inf) before <- 365L * 10000
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
    before = before,
    ref_time_values = valid_predict_dates,
  )
  res %<>% select(-time_value)
  names(res) <- sub("^slide_value_", "", names(res))

  # append the truth data
  true_value <- archive$as_of(archive$versions_end) %>%
    select(geo_value, time_value, !!outcome) %>%
    rename(true_value = !!outcome)
  res %<>%
    inner_join(true_value,
      by = join_by(geo_value, target_end_date == time_value)
    )
  return(res)
}
