#' Predict on smoothed data and the standard deviation
#'
#' This is a variant of `scaled_pop`, which predicts on a smoothed version of
#' the data. Even if the target is smoothed when used as a /predictor/, as a
#' /target/ it still uses the raw value (this captures some of the noise).  It
#' also uses a rolling standard deviation as an auxillary signal, window of
#' withd `sd_width`, which by default is 28 days. If you are using `sd_width`,
#' you should restrict the lags on the `sd` to only include `0`, so set your
#' lags to be e.g. `list(c(0,7,14), c(0))`.
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
#' @param pop_scaling bool; if `TRUE`, assume all numeric columns are on the
#'   count scale and translate them to a rate scale for model fitting.
#'   Predictions will be translated back to count scale. Any
#'   `layer_residual_quantiles` (for non-`"quantile_reg"` `trainer`s) will be
#'   done on the rate scale. When specifying predictor lags, note that rate
#'   variables will use the same names as and overwrite the count variables.
#'   Rates here will be counts per 100k population, based on
#'   `epidatasets::state_census`.
#' @param trainer optional; parsnip model specification to use for the core
#'   fitting & prediction (the `spec` of the internal
#'   [`epipredict::epi_workflow`]).  Default is `parsnip::linear_reg()`.
#' @param smooth_width the number of days over which to do smoothing. If `NULL`,
#'   then no smoothing is applied.
#' @param smooth_cols the names of the columns to smooth. If `NULL` it smooths
#'   everything
#' @param sd_width the number of days over which to take a moving average of the
#'   standard deviation. If `NULL`, the sd_width isn't included.
#' @param sd_mean_width to calculate the sd, we need a window size for the mean
#'   used.
#' @param sd_cols the names of the columns to smooth. If `NULL` its includes
#'   the sd of everything
#' @param quantile_levels The quantile levels to predict. Defaults to those
#' @param ... any additional arguments as used by [default_args_list]
#'   required by covidhub.
#' @seealso some utilities for making forecasters: [format_storage],
#'   [sanitize_args_predictors_trainer]
#'
#' @importFrom epipredict epi_recipe step_population_scaling frosting default_args_list layer_population_scaling
#' @importFrom tibble tibble
#' @importFrom recipes all_numeric
#' @importFrom zeallot %<-%
#' @export
smoothed_scaled <- function(epi_data,
                            outcome,
                            extra_sources = "",
                            ahead = 1,
                            pop_scaling = TRUE,
                            trainer = parsnip::linear_reg(),
                            quantile_levels = covidhub_probs(),
                            smooth_width = 7,
                            smooth_cols = NULL,
                            sd_width = 28,
                            sd_mean_width = 14,
                            sd_cols = NULL,
                            drop_non_seasons = FALSE,
                            scale_method = c("none", "quantile", "std"),
                            center_method = c("median", "mean", "none"),
                            nonlin_method = c("quart_root", "none"),
                            filter_source = "",
                            filter_agg_level = "",
                            ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)

  epi_data <- validate_epi_data(epi_data)

  # perform any preprocessing not supported by epipredict
  #
  # this is for the case where there are multiple sources in the same column
  epi_data %<>% filter_extraneous(filter_source, filter_agg_level)
  # this is a temp fix until a real fix gets put into epipredict
  epi_data <- clear_lastminute_nas(epi_data, outcome, extra_sources)
  # see latency_adjusting for other examples
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, ahead, args_input, outcome, extra_sources)) {
    null_result <- epi_data[0L, c("geo_value", attr(epi_data, "metadata", exact = TRUE)[["other_keys"]])] %>%
      mutate(
        forecast_date = epi_data$time_value[0],
        target_end_date = epi_data$time_value[0],
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
  # `extra_sources` sets which variables beyond the outcome are lagged and used as predictors
  # any which are modified by `rolling_mean` or `rolling_sd` have their original values dropped later
  predictors <- c(outcome, extra_sources[[1]])
  predictors <- predictors[predictors != ""]
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict



  #######################
  # robust whitening
  #######################
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }
  # whiten to get the sources on the same scale
  learned_params <- calculate_whitening_params(season_data, predictors, scale_method, center_method, nonlin_method)
  epi_data %<>% data_whitening(predictors, learned_params, nonlin_method)

  ###############
  # smoothing
  ###############
  keep_mean <- !is.na(smooth_width) && !is.na(sd_width) && !is.na(sd_width) && !is.null(sd_mean_width) &&
    smooth_width == sd_mean_width # do we (not) need to do the mean separately?
  # since we're adding columns, we need to figure out which to exclude
  all_names <- get_nonkey_names(epi_data)
  unused_columns <- all_names[!(all_names %in% predictors)]
  if (is.null(smooth_cols)) {
    smooth_cols <- predictors
  }
  # if we smooth it, we're not using the original version for prediction
  unused_columns <- c(unused_columns, smooth_cols[!(smooth_cols %in% unused_columns)])

  if (is.null(sd_cols)) {
    sd_cols <- predictors
  }
  # same idea for sd if we're keeping the mean
  if (keep_mean) {
    unused_columns <- c(unused_columns, sd_cols[!(sd_cols %in% unused_columns)])
  }


  # make sure that sd_width etc have the right units; the process of going through targets strips the type
  time_type <- attributes(epi_data)$metadata$time_type
  if (time_type != "day") {
    sd_width <- as.difftime(sd_width, units = paste0(time_type, "s"))
    sd_mean_width <- as.difftime(sd_mean_width, units = paste0(time_type, "s"))
    smooth_width <- as.difftime(smooth_width, units = paste0(time_type, "s"))
  }

  if (!is.null(smooth_width) && !is.na(smooth_width) && !keep_mean) {
    epi_data %<>% rolling_mean(
      width = smooth_width,
      cols_to_mean = smooth_cols
    )
  }

  # measuring standard deviation
  if (!is.null(sd_width) && !is.na(sd_width)) {
    epi_data %<>% rolling_sd(
      sd_width = sd_width,
      mean_width = sd_mean_width,
      cols_to_sd = sd_cols,
      keep_mean = keep_mean
    )
  }

  # need to make a version with the non seasonal and problematic flu seasons removed
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }

  # and need to make sure we exclude the original variables as predictors
  all_names <- get_nonkey_names(epi_data)
  predictors <- all_names[!(all_names %in% unused_columns)]
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)
  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  if (pop_scaling) {
    preproc %<>% step_population_scaling(
      all_numeric(),
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
  pred <- run_workflow_and_format(
    preproc, postproc, trainer,
    season_data, epi_data
  )
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
