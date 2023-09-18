new_forecaster <- function(pre_preprocessing, preprocessing, postprocessing, post_postprocessing, params) {
  # TODO add checks
  structure(pre_preprocessing, preprocessing, postprocessing, post_postprocessing, params,)
}


#' given a particular epi_df
#' @description
#' The equivalent to arx_forecaster, but for the more general forecaster model
#' that allows for pre- and post- processing.
predict_forecasting_workflow <- function(epi_data, outcome, predictors, trainer = NULL, pre_preprocessing = list(), preprocessing = list(list(step = "default")), postprocessing = list(list(step = "default")), post_postprocessing = list(), args_list = arx_args_list()) {
  if (!epipredict:::is_regression(trainer)) {
    cli::cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'regression'.")
  }
  for (step in pre_preprocessing) {
    epi_data %<>% rlang::exec(step$step, ., !!!step$args)
  }

  workflow <- build_forecasting_workflow(epi_data, outcome, predictors, trainer, pre_preprocessing, preprocessing, postprocessing, post_postprocessing, args_list)

  latest <- get_test_data(
    hardhat::extract_preprocessor(workflow), epi_data, TRUE, args_list$nafill_buffer,
    args_list$forecast_date %||% max(epi_data$time_value)
  )
  workflow <- generics::fit(workflow, epi_data)
  preds <- predict(workflow, new_data = latest) %>%
    tibble::as_tibble() %>%
    dplyr::select(-time_value)
  for (step in postprocessing) {
    if (is.character(step$step) && step$step == "default") {
      # the proccessing steps of arx_forecaster
      r %<>% arx_preprocess(outcome, predictors, lags, args_list)
    } else if (is.function(step$step) && grepl("step", as.character(step$step))) {
      # its a step as defined by epipredict or parsnip
      r %<>% rlang::exec(step$step, ., !!!step$args)
    }
  }

  structure(
    list(
      predictions = preds,
      epi_workflow = wf,
      metadata = list(
        training = attr(epi_data, "metadata"),
        forecast_created = Sys.time()
      )
    ),
    class = c("arx_fcast", "canned_epipred")
  )
}
#' turn a model spec into an actually run workflow
#' @description
#' this is the equivalent of arx_fcast_epi_workflow, and is mostly internal
create_forecaster_workflow <- function(epi_data, outcome, predictors, trainer = NULL, pre_preprocessing = list(), preprocessing = list(list(step = "default")), postprocessing = list(list(step = "default")), post_postprocessing = list(), args_list = arx_args_list()) {
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `arx_args_list().")
  }
  if (!(is.null(trainer) || epipredict:::is_regression(trainer))) {
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  }
  lags <- epipredict:::arx_lags_validator(predictors, args_list$lags)
  # first, transform the data using any pure functions in the pipeline (these have to happen before the epipredict steps)
  r <- epi_recipe(epi_data)
  for (step in preprocessing) {
    if (is.character(step$step) && step$step == "default") {
      # the proccessing steps of arx_forecaster
      r %<>% arx_preprocess(outcome, predictors, lags, args_list)
    } else if (is.function(step$step) && getNamespaceName(environment(step$step)) == "epipredict") {
      # its a step as defined by epipredict or parsnip
      r %<>% rlang::exec(step$step, ., !!!step$args)
    }
  }
  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% forecast_date + args_list$ahead
  f <- frosting()
}

# TODO replace with `step_arx_forecaster`
arx_preprocess <- function(r, outcome, predictors, lags, args_list) {
  # input already validated
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
arx_postprocess <- function(f, trainer, outcome, predictors, lags, args_list, forecast_date, target_date) {
  f %<>% layer_predict()
  if (inherits(trainer, "quantile_reg")) {
    # add all levels to the forecaster and update postprocessor
    tau <- sort(compare_quantile_args(
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



