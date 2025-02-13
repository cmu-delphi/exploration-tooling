#' predict the value using only the week in the season, maybe the population, and any extra sources
#' it may whiten any old data as the outcome
no_recent_outcome <- function(epi_data,
                              outcome,
                              extra_sources = "",
                              ahead = 7,
                              pop_scaling = FALSE,
                              trainer = epipredict::quantile_reg(),
                              quantile_levels = covidhub_probs(),
                              use_population = FALSE,
                              use_density = FALSE,
                              drop_non_seasons = FALSE,
                              scale_method = c("quantile", "std", "none"),
                              center_method = c("median", "mean", "none"),
                              nonlin_method = c("quart_root", "none"),
                              week_method = c("linear", "sine"),
                              filter_source = "",
                              filter_agg_level = "",
                              sources_to_pop_scale = c(),
                              ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  week_method <- arg_match(week_method)

  epi_data <- validate_epi_data(epi_data)

  # this is for the case where there are multiple sources in the same column
  epi_data %<>% filter_extraneous(filter_source, filter_agg_level)
  args_input <- list(...)
  # this next part is basically unavoidable boilerplate you'll want to copy
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
  # perform any preprocessing not supported by epipredict
  # make sure we've got the seasonweek
  adding_source <- FALSE
  if (!("source" %in% names(epi_data))) {
    adding_source <- TRUE
    epi_data$source <- c("nhsn")
    attributes(epi_data)$metadata$other_keys <- "source"
  }
  if ("season_week" %nin% names(epi_data)) {
    epi_data %<>% add_season_info()
  }
  if (!("population" %in% names(epi_data))) {
    epi_data %<>% add_pop_and_density()
    epi_data %<>% mutate(agg_level = ifelse(is.na(agg_level), "state", agg_level))
  }

  # only force to be non-negative if we're not scaling
  args_input[["nonneg"]] <- scale_method == "none"
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(default_args_list, args_input)
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources[[1]])
  c(args_list, tmp_pred, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)
  if (extra_sources[[1]] == "") {
    predictors <- character()
  } else {
    predictors <- extra_sources[[1]]
  }
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  # need factors for most things
  #
  epi_data %<>% ungroup() %>% mutate(across(where(is.character), as.factor))
  keys <- key_colnames(epi_data, exclude = "time_value")

  full_data <- epi_data
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }
  if (scale_method != "none") {
    learned_params <- calculate_whitening_params(season_data, outcome, scale_method, center_method, nonlin_method)
    full_data %<>% data_whitening(outcome, learned_params, nonlin_method = nonlin_method)
  }
  season_data <- full_data %>% drop_non_seasons()
  # preprocessing supported by epipredict
  preproc <- epi_recipe(season_data)
  if (use_population) {
    # population
    preproc %<>%
      add_role(population, new_role = "pre-predictor")
  }
  if (use_density) {
    # density
    preproc %<>%
      add_role(population, new_role = "pre-predictor")
  }
  # week of the season
  if (week_method == "linear") {
    preproc %<>% add_role(season_week, new_role = "pre-predictor")
  } else if (week_method == "sine") {
    preproc %<>% step_season_week_sine(season = 35)
  }
  # any relevant lags, the aheads, latency adjustment, that sort of thing
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
    preproc, postproc,
    trainer, season_data,
    full_data
  )

  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict
  # reintroduce color into the value
  if (scale_method != "none") {
    pred <- pred %>%
      rename({{ outcome }} := value) %>%
      data_coloring(outcome, learned_params, join_cols = key_colnames(epi_data, exclude = "time_value"), nonlin_method = nonlin_method) %>%
      rename(value = {{ outcome }}) %>%
      mutate(value = pmax(0, value))
  }
  if (adding_source) {
    pred %<>% select(-source)
  }
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict
  gc()
  return(pred)
}
