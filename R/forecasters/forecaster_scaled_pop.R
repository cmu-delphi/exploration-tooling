#' Example forecaster definition
#'
#' A forecaster is a function that takes in an epi_df, an outcome column name, a
#' list of extra columns, and an ahead, and produces a forecast with a
#' `target_end_date` that is `ahead` many days after
#' `attributes(epi_df)$metadata$as_of`. The resulting output should be a
#' tibble with columns `(geo_value, forecast_date, target_end_date, quantile,
#' value)`, preferably in that order.
#'
#' To define a forecaster:
#' 1. first define any pre-epipredict steps (they should be operating on a
#'   epi_df)
#' 2. then define any post-epipredict steps (same)
#' 3. then create the forecaster as a function, which must have the following
#' arguments:
#'
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
#' @param filter_source if multiple sources are mixed together, if this is non-null it filters to just ones that match this value
#' @param filter_agg_level if multiple geographic levels are mixed together, if this is non-null it filters to just ones that match this value (e.g. you probably want "state")
#' @param ... it can also have any number of other parameters. In this case, the
#'   `...` args are all inputs to [`epipredict::default_args_list`].  Consult the
#'   repository for existing parameter names so that your function will follow a
#'   similar schema (e.g. `trainer`, while not strictly required, is a common
#'   parameter, as are any of the `default_args_list()` parameters) these parameters
#'   should be ones that will store well in a data.table; if you need more
#'   complicated parameters, it is better to store them in separate files, and
#'   use the filename as the parameter.
#' @param quantile_levels The quantile levels to predict. Defaults to those required by
#'   covidhub.
#' @seealso some utilities for making forecasters: [format_storage],
#'   [sanitize_args_predictors_trainer]
#'
#' @importFrom epipredict epi_recipe step_population_scaling frosting default_args_list layer_population_scaling
#' @importFrom tibble tibble
#' @importFrom zeallot %<-%
#' @importFrom recipes all_numeric
#' @export
scaled_pop <- function(epi_data,
                       outcome,
                       extra_sources = "",
                       ahead = 1,
                       pop_scaling = TRUE,
                       drop_non_seasons = FALSE,
                       scale_method = c("none", "quantile", "std"),
                       center_method = c("median", "mean", "none"),
                       nonlin_method = c("quart_root", "none"),
                       trainer = epipredict::quantile_reg(),
                       quantile_levels = covidhub_probs(),
                       filter_source = "",
                       filter_agg_level = "",
                       ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)

  epi_data <- validate_epi_data(epi_data)
  extra_sources <- unwrap_argument(extra_sources)
  trainer <- unwrap_argument(trainer)

  # perform any preprocessing not supported by epipredict
  #
  # this is for the case where there are multiple sources in the same column
  epi_data %<>% filter_extraneous(filter_source, filter_agg_level)
  # this next part is basically unavoidable boilerplate you'll want to copy
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, ahead, args_input, outcome, extra_sources)) {
    null_result <- tibble(
      geo_value = character(),
      forecast_date = lubridate::Date(),
      target_end_date = lubridate::Date(),
      quantile = numeric(),
      value = numeric()
    )
    return(null_result)
  }
  # this is to deal with grouping by source in tests that don't include it
  adding_source <- FALSE
  if (!("source" %in% names(epi_data))) {
    adding_source <- TRUE
    epi_data$source <- c("nhsn")
    attributes(epi_data)$metadata$other_keys <- "source"
  }
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_input[["nonneg"]] <- scale_method == "none"
  args_list <- inject(default_args_list(!!!args_input))
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources)
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict


  # if we're dropping non-seasonal and data from the years when flu wasn't active
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }

  # whiten to get the sources on the same scale
  learned_params <- calculate_whitening_params(season_data, predictors, scale_method, center_method, nonlin_method)
  epi_data %<>% data_whitening(predictors, learned_params, nonlin_method)

  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }

  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  if (pop_scaling) {
    preproc %<>% step_population_scaling(
      all_of(predictors),
      df = epidatasets::state_census,
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
      df = epidatasets::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  # with all the setup done, we execute and format
  pred <- run_workflow_and_format(preproc, postproc, trainer, season_data, epi_data)
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict e.g. calibration
  # reintroduce color into the value
  pred_final <- pred %>%
    rename({{ outcome }} := value) %>%
    data_coloring(outcome, learned_params, join_cols = key_colnames(epi_data, exclude = "time_value"), nonlin_method = nonlin_method) %>%
    rename(value = {{ outcome }}) %>%
    mutate(value = pmax(0, value))
  if (adding_source) {
    pred_final %<>% select(-source)
  }
  gc()
  return(pred_final)
}
