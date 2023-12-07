# TODO replace with `step_arx_forecaster`
#' add the default steps for arx_forecaster
#' @description
#' add the default steps for arx_forecaster
#' @param rec an [`epipredict::epi_recipe`]
#' @param outcome a character of the column to be predicted
#' @param predictors a character vector of the columns used as predictors
#' @param args_list an [`epipredict::arx_args_list`]
#' @seealso [arx_postprocess] for the layer equivalent
#' @importFrom epipredict step_epi_lag step_epi_ahead step_epi_naomit step_training_window
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
#' @importFrom epipredict layer_predict layer_quantile_distn layer_point_from_distn layer_residual_quantiles layer_threshold layer_naomit layer_add_target_date
#' @export
arx_postprocess <- function(postproc,
                            trainer,
                            args_list,
                            forecast_date = NULL,
                            target_date = NULL) {
  postproc %<>% layer_predict()
  if (inherits(trainer, "quantile_reg")) {
    postproc %<>%
      layer_quantile_distn(quantile_levels = args_list$quantile_levels) %>%
      layer_point_from_distn()
  } else {
    postproc %<>% layer_residual_quantiles(
      quantile_levels = args_list$quantile_levels, symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key
    )
  }
  if (args_list$nonneg) {
    postproc %<>% layer_threshold(dplyr::starts_with(".pred"))
  }

  postproc %<>%
    layer_naomit(dplyr::starts_with(".pred")) %>%
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
#' @importFrom epipredict epi_workflow fit add_frosting get_test_data
run_workflow_and_format <- function(preproc, postproc, trainer, epi_data) {
  workflow <- epi_workflow(preproc, trainer) %>%
    fit(epi_data) %>%
    add_frosting(postproc)
  latest <- get_test_data(recipe = preproc, x = epi_data)
  pred <- predict(workflow, latest)
  # the forecast_date may currently be the max time_value
  as_of <- attributes(epi_data)$metadata$as_of
  if (is.null(as_of)) {
    as_of <- max(epi_data$time_value)
  }
  true_forecast_date <- as_of
  return(format_storage(pred, true_forecast_date))
}
