# Hmm, annoying. I think the short answer is to wrap a workflow in a function of the arguments. Then loop over all the combinations. The longer answer is that plus something like batchtools that will let you parallelize all that (and track the results in a database): https://mllg.github.io/batchtools/
#' build a workflow given a list of preprocessing steps (step =, args =) and post-processing steps (layer = , args=). Each has a special "step" given by list(step = "default") and list(layer = "default") that performs the operations as arx_forecaster would, using the given args_list. This allows the


# instead of this, define dummy data, and directly build the workflows
# add `step_arx_forecaster`
# add `step_arx_classifier`
# add `layer_arx_forecaster`
# add `layer_interval` (or something that dispatches on the trainer)
build_forecasting_workflow <- function(epi_data, outcome, predictors, trainer = NULL, pre_preprocessing = list(), preprocessing = list(list(step = "default")), postprocessing = list(list(step = "default")), post_postprocessing = list(), args_list = arx_args_list()) {
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `arx_args_list().")
  }
  if (!(is.null(trainer) || epipredict:::is_regression(trainer))) {
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  }
  lags <- epipredict:::arx_lags_validator(predictors, args_list$lags)
  # first, transform the data using any pure functions in the pipeline (these have to happen before the epipredict steps)
  for (step in pre_preprocessing) {
    epi_data %<>% rlang::exec(step$step, ., !!!step$args)
  }

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
  for (step in postprocessing) {
    if (is.character(step$step) && step$step == "default") {
      # the proccessing steps of arx_forecaster
      r %<>% arx_preprocess(outcome, predictors, lags, args_list)
    } else if (is.function(step$step) && grepl("step", as.character(step$step))) {
      # its a step as defined by epipredict or parsnip
      r %<>% rlang::exec(step$step, ., !!!step$args)
    }
  }
}

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



execute_forecasting_workflow <- function(epi_data, outcome, predictors, trainer = NULL, pre_preprocessing = list(), preprocessing = list(list(step = "default")), postprocessing = list(list(step = "default")), post_postprocessing = list(), args_list = arx_args_list()) {
  browser()
  if (!epipredict:::is_regression(trainer)) {
    cli::cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'regression'.")
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

# example pre-workflow
args_list <- arx_args_list()
pre_preprocessing <- list(
  list(
    step = your_favorite_smoother,
    args = some_extra_args()
  )
)
preprocessing <- list(
  list(
    step = step_population_scaling,
    args = list(
      all_numeric(),
      df = state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  ),
  list(step = "default")
)
postprocessing <- list(
  list(
    layer = layer_population_scaling,
    args = list(
      ".pred",
      df = state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  ),
  list(step = "default")
)


source(file = file.path("..", "hospitalization-forecaster", "basic-epipredict-forecaster", "download-script.R"))
epi_data <- archive$as_of(as.Date("2022-01-01"))
outcome <- "hhs"
predictors <- "chng"
trainer <- parsnip::linear_reg()

library(epipredict)
library(epiprocess)
library(tidyverse)
library(parsnip)
library(recipes)

workflow <- build_forecasting_workflow(epi_data, outcome, predictors, trainer, preprocessing = preprocessing, postprocessing = postprocessing)
execute_forecasting_workflow(epi_data, outcome, predictors, trainer, preprocessing = preprocessing, postprocessing = postprocessing)
