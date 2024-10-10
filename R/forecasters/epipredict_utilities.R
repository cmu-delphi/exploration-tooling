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
  lags <- args_list$lags
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
  for (l in seq_along(lags)) {
    p <- predictors[l]
    preproc %<>% step_epi_lag(!!p, lag = lags[[l]])
  }
  preproc %<>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead) %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training)
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
#' @param epi_data the actual epi_df to train on
#'
#' @importFrom epipredict epi_workflow fit add_frosting get_test_data
#' @export
run_workflow_and_format <-
  function(preproc,
           postproc,
           trainer,
           epi_data,
           full_data = NULL,
           test_data_interval = as.difftime(52, units = "weeks"),
           return_model = FALSE) {
    workflow <-
      epi_workflow(preproc, trainer) %>%
      fit(epi_data) %>%
      add_frosting(postproc)
    if (is.null(full_data)) {
      test_data <- epi_data
    } else {
      test_data <- full_data
    }
    # getting test data
    ## rec <- extract_recipe(workflow)
    ## rec$steps[[6]]$latency_table %>% print(n = 74)
    max_time_value <- test_data %>%
      na.omit() %>%
      pull(time_value) %>%
      max()
    test_data %<>% filter((max_time_value - time_value) < test_data_interval) %>% arrange(time_value)
    pred <- predict(workflow, test_data)
    # to have actually predicted at a time, we need that the prepredictors are
    # non-na
    non_na_indicators <- preproc$var_info %>%
      filter(role == "pre-predictor") %>%
      pull(variable)
    # get_test_data ends up predicting slightly more points than it needs,
    # so we winnow them out
    possible_time_values <-
      test_data %>%
      group_by(across(
        key_colnames(test_data, exclude = "time_value")
      )) %>%
      drop_na(all_of(non_na_indicators)) %>%
      summarize(max_time_value = max(time_value), .groups = "drop")
    pred <- pred %>%
      left_join(
        possible_time_values,
        by = key_colnames(test_data, exclude = "time_value")
      ) %>%
      filter(time_value == max_time_value) %>%
      select(-max_time_value)
    if (return_model) {
      return(list(pred = format_storage(pred, possible_time_values), workflow = workflow))
    } else {
      return(format_storage(pred, possible_time_values))
    }
  }

