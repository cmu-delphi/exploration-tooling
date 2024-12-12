#' Scaled pop seasonal
#'
#' This is identical to the `scaled_pop` forecaster, but with seasonal features
#' added in. The seasonal features are pre-cached from a PCA analysis of past
#' data.
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
scaled_pop_seasonal <- function(epi_data,
                                outcome,
                                extra_sources = "",
                                ahead = 1,
                                pop_scaling = TRUE,
                                drop_non_seasons = FALSE,
                                scale_method = c("quantile", "std", "none"),
                                center_method = c("median", "mean", "none"),
                                nonlin_method = c("quart_root", "none"),
                                seasonal_method = c("none", "flu", "covid", "indicator", "window", "climatological"),
                                season_backward_window = 5 * 7,
                                season_forward_window = 3 * 7,
                                train_residual = FALSE,
                                trainer = parsnip::linear_reg(),
                                quantile_levels = covidhub_probs(),
                                filter_source = "",
                                filter_agg_level = "",
                                ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  if (typeof(seasonal_method) == "list") {
    seasonal_method <- seasonal_method[[1]]
  }
  if (all(seasonal_method == c("none", "flu", "covid", "indicator", "window", "climatological"))) {
    seasonal_method <- "none"
  }
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
  args_input[["n_training"]] <- season_backward_window
  args_input[["n_forward"]] <- season_forward_window + ahead
  args_input[["seasonal_window"]] <- "window" %in% seasonal_method
  args_list <- inject(default_args_list(!!!args_input))
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources[[1]])
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)

  if ("season_week" %nin% names(epi_data)) {
    epi_data %<>% add_season_info()
  }
  # end of the copypasta

  # whiten to get the sources on the same scale
  # TODO Jank way to avoid having hhs_region get centered
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }
  learned_params <- calculate_whitening_params(season_data, setdiff(predictors, "hhs_region"), scale_method, center_method, nonlin_method)
  epi_data %<>% data_whitening(setdiff(predictors, "hhs_region"), learned_params, nonlin_method)

  # get the seasonal features
  # first add PCA
  if (("flu" %in% seasonal_method) || ("covid" %in% seasonal_method)) {
    epi_data <- compute_pca(epi_data, seasonal_method, ahead, scale_method, center_method, nonlin_method, normalize = train_residual)

    if (train_residual) {
      epi_data <- epi_data %>% mutate(across(all_of(outcome), ~ .x - PC1))
      values_subtracted <- epi_data %>%
        mutate(epiweek = epiweek(time_value)) %>%
        select(geo_value, source, epiweek, value = PC1) %>%
        distinct(geo_value, source, epiweek, .keep_all = TRUE)
    }
    args_list$lags <- c(args_list$lags, 0)
  }

  # then the climatological median
  if ("climatological" %in% seasonal_method) {
    epi_data <- epi_data %>%
      climate_median(target = outcome, ahead = ahead, scale_rate = pop_scaling, normalize = train_residual)
    if (train_residual) {
      epi_data <- epi_data %>% mutate(across(all_of(outcome), ~ .x - climate_median))
      values_subtracted <- epi_data %>%
        mutate(epiweek = epiweek(time_value)) %>%
        select(geo_value, source, epiweek, value = climate_median) %>%
        distinct(geo_value, source, epiweek, .keep_all = TRUE)
    }
  }

  # TODO: Replace with step_training_window2
  if ("window" %in% seasonal_method) {
    last_data_season_week <- epi_data %>%
      filter(source == "nhsn") %>%
      filter(time_value == max(time_value)) %>%
      pull(season_week) %>%
      max()
    current_season_week <- convert_epiweek_to_season_week(epiyear(epi_as_of(epi_data)), epiweek(epi_as_of(epi_data)))
    date_ranges <- epi_data %>%
      filter(season_week == last_data_season_week) %>%
      pull(time_value) %>%
      unique() %>%
      map(~ c(.x - seq(from = 7, to = season_backward_window, by = 7), .x + seq(from = 0, to = season_forward_window, by = 7))) %>%
      unlist() %>%
      as.Date() %>%
      unique()
    epi_data <- epi_data %>% filter(time_value %in% unlist(date_ranges))
  }

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
  if ("indicator" %in% seasonal_method) {
    preproc %<>%
      # Really jank way of accounting for ahead.
      step_mutate(before_peak = (season_week - (ahead / 7) < 16), role = "predictor") %>%
      step_mutate(after_peak = (season_week - (ahead / 7) > 20), role = "predictor")
  }
  if (!train_residual && ("flu" %in% seasonal_method)) {
    preproc %<>% add_role(PC1, new_role = "predictor")
  } else if (!train_residual && ("covid" %in% seasonal_method)) {
    preproc %<>% add_role(PC1, new_role = "predictor")
  }
  if (!train_residual && ("climatological" %in% seasonal_method)) {
    preproc %<>% add_role(pooled_climate_median, climate_median, new_role = "predictor")
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
  #
  # undo subtraction if we're training on residuals
  if (train_residual && (("flu" %in% seasonal_method) || ("covid" %in% seasonal_method) || ("climatological" %in% seasonal_method))) {
    pred <- pred %>%
      mutate(epi_week = epiweek(target_end_date)) %>%
      left_join(values_subtracted, by = join_by(geo_value, source, epi_week == epiweek)) %>%
      mutate(value = value.x + value.y) %>%
      select(geo_value, source, forecast_date, target_end_date, quantile, value)
  }

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
