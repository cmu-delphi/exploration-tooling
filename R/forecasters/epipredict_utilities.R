#' Add the default steps for arx_forecaster
#'
#' @param preproc an [`epipredict::epi_recipe`]
#' @param outcome a character of the column to be predicted
#' @param predictors a character vector of the columns used as predictors
#' @param args_list an [`epipredict::default_args_list`]
#' @seealso [arx_postprocess] for the layer equivalent
#'
#' @importFrom epipredict step_epi_lag step_epi_ahead step_epi_naomit step_training_window
#' @export
arx_preprocess <- function(preproc, outcome, predictors, args_list) {
  # input already validated
  if (args_list$adjust_latency != "none") {
    preproc %<>% step_adjust_latency(
      method = args_list$adjust_latency,
      keys_to_ignore = args_list$keys_to_ignore
    )
    if (args_list$adjust_latency == "extend_lags") {
      # this is a bit of a hack to make sure that *all* predictors are present at the correct lag
      preproc %<>% step_epi_lag(has_role("pre-predictor"), lag = 0, role = "predictor")
    }
  }

  lags <- args_list$lags
  if (any(predictors != "")) {
    for (l in seq_along(predictors)) {
      p <- predictors[l]
      preproc %<>% step_epi_lag(!!p, lag = lags[[l]])
    }
  }
  preproc %<>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead) %>%
    # TODO: Uncomment after debugging
    # step_epi_naomit() %>%
    step_training_window(
      n_recent = args_list$n_training,
      # n_forward = args_list$n_forward,
      # seasonal = args_list$seasonal_window
    )
  return(preproc)
}

#' Add the default layers for arx_forecaster
#'
#' @param postproc an [`epipredict::frosting`]
#' @param trainer the trainer used (e.g. linear_reg() or quantile_reg())
#' @param args_list an [`epipredict::default_args_list`]
#' @param forecast_date the date from which the forecast was made. defaults to
#'   the default of `layer_add_forecast_date`, which is currently the max
#'   time_value present in the data
#' @param target_date the date about which the forecast was made. defaults to
#'   the default of `layer_add_target_date`, which is either
#'   `forecast_date+ahead`, or the `max time_value + ahead`
#' @seealso [arx_preprocess] for the step equivalent
#'
#' @importFrom epipredict layer_predict layer_quantile_distn
#' layer_point_from_distn layer_residual_quantiles layer_threshold layer_naomit
#' layer_add_target_date
#' @export
arx_postprocess <- function(postproc,
                            trainer,
                            args_list,
                            forecast_date = NULL,
                            target_date = NULL) {
  postproc %<>% layer_predict()
  if (inherits(trainer, "quantile_reg") || trainer$engine == "grf_quantiles") {
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
    layer_add_target_date() %>%
    layer_add_forecast_date()
  return(postproc)
}

#' Run workflow and format
#'
#' Helper function to run a epipredict model and reformat to hub format
#'
#' @param preproc the preprocessing steps
#' @param postproc the postprocessing frosting
#' @param trainer the parsnip trainer
#' @param train_data the actual epi_df to train on; this is after any
#'   transformations that epipredict can't apply (e.g. whitening, smoothing).
#'   This may be narrowed down to exclude data we don't want to train on (such
#'   as off season).
#' @param full_data all of epi_df with the pre-epipredict transformations, used
#'   to construct a test dataset (useful if train_data is excluding summers or
#'   otherwise restricted to a subset of the data). If null, this assumes the
#'   train and test data are exactly the same
#'
#' @importFrom epipredict epi_workflow fit add_frosting get_test_data
#' @export
run_workflow_and_format <- function(preproc,
                                    postproc,
                                    trainer,
                                    train_data,
                                    full_data = NULL,
                                    test_data_interval = as.difftime(52, units = "weeks"),
                                    return_model = FALSE) {
  as_of <- attributes(train_data)$metadata$as_of
  if (is.null(as_of)) {
    as_of <- max(train_data$time_value)
  }
  workflow <- epi_workflow(preproc, trainer) %>%
    fit(train_data) %>%
    add_frosting(postproc)
  # filter full_data to less than full but more than we need
  test_data <- get_oversized_test_data(full_data %||% train_data, test_data_interval, preproc)
  # predict, and filter out those forecasts for less recent days (predict
  # predicts for every day that has enough data)
  pred <- predict(workflow, test_data)
  # keeping only the last time_value for any given location/key
  pred %<>%
    group_by(across(all_of(key_colnames(train_data, exclude = "time_value")))) %>%
    # TODO: slice_max(time_value)?
    arrange(time_value) %>%
    filter(row_number() == n()) %>%
    ungroup()
  return(format_storage(pred, as_of))
}

#' get_test_data is broken, this is a hack that manually sets the amount of data
#' kept to a large interval to avoid errors/missing data
#' @param full_data the full data to narrow down from
#' @param test_data_interval the amount of time to go backwards from the last
#'   day
get_oversized_test_data <- function(full_data, test_data_interval, preproc, predicting = "nhsn") {
  # getting the max time value of data columns actually used
  non_na_indicators <- preproc$var_info %>%
    filter(role == "pre-predictor") %>%
    pull(variable)
  max_time_value <- full_data %>%
    na.omit(non_na_indicators) %>%
    pull(time_value) %>%
    max()
  if ("source" %in% names(full_data)) {
    full_data <- full_data %>% filter(source == predicting)
  }
  full_data %>%
    filter((max_time_value - time_value) < test_data_interval) %>%
    arrange(time_value)
}




epi_as_of <- function(epi_dataframe) {
  attributes(epi_dataframe)$metadata$as_of
}
