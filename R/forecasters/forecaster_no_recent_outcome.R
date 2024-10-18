#' predict the value using only the week in the season, maybe the population, and any extra sources
#' it may whiten any old data as the outcome
no_recent_outcome <- function(epi_data,
                              outcome,
                              extra_sources = "",
                              ahead = 7,
                              pop_scaling = FALSE,
                              trainer = epipredict::quantile_reg(),
                              quantile_levels = covidhub_probs(),
                              use_population = TRUE,
                              use_density = TRUE,
                              scale_method = c("quantile", "std", "none"),
                              center_method = c("median", "mean", "none"),
                              filter_source = "",
                              filter_agg_level = "",
                              smooth_width = 3,
                              smooth_cols = NULL,
                              sources_to_pop_scale = c(),
                              ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  if (is.null(smooth_cols)) {
    smooth_cols <- extra_sources
  }
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
    epi_data$source <- c("none")
    attributes(epi_data)$metadata$other_keys <- "source"
  }
  if (!("season_week" %in% names(epi_data))) {
    epi_data %<>%
      mutate(
        epiweek = epiweek(time_value),
        year = epiyear(time_value),
        season_week = convert_epiweek_to_season_week(year, epiweek)
      )
  }
  if (!("season" %in% names(epi_data))) {
    epi_data %<>% mutate(season = convert_epiweek_to_season(year, epiweek))
  }
  if (!("population" %in% names(epi_data))) {
    epi_data %<>% add_pop_and_density()
    epi_data %<>% mutate(agg_level = ifelse(is.na(agg_level), "state", agg_level))
  }

  args_input[["nonneg"]] <- TRUE
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(default_args_list, args_input)
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- extra_sources
  # TODO: Partial match quantile_level coming from here (on Dmitry's machine)
  c(args_list, tmp_pred, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, outcome, trainer, args_list)

  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  # need factors for most things
  #
  epi_data %<>% ungroup() %>% mutate(across(where(is.character), as.factor))
  keys <- key_colnames(epi_data, exclude = "time_value")
  # add the slightly smoothed values beforehand; this is about speed, since step_epi_slide isn't ready yet
  if (any(predictors != "")) {
    epi_data %<>%
      rolling_mean(
        width = smooth_width,
        cols_to_mean = predictors
      )
  }

  full_data <- epi_data
  season_data <- epi_data %>% drop_non_seasons()
  if (scale_method != "none") {
    learned_params <- calculate_whitening_params(season_data, outcome, scale_method, center_method)
    full_data %<>% data_whitening(outcome, learned_params)
  }
  # preprocessing supported by epipredict
  preproc <- epi_recipe(season_data)
  # slide is currently done before for efficiency reasons, added as predictors here
  if (any(predictors != "")) {
    preproc %>%
      add_role(starts_with("slide_"), new_role = "pre-predictor")
  }
  if (use_population == TRUE) {
    # population and density
    preproc %<>%
      add_role(population, density, new_role = "pre-predictor")
  }
  if (use_population == TRUE) {
    # population and density
    preproc %<>%
      add_role(population, density, new_role = "pre-predictor")
  }
  # week of the season
  preproc %<>% add_role(season_week, new_role = "pre-predictor")
  # any relevant lags, the aheads, latency adjustment, that sort of thing
  preproc %<>% arx_preprocess(outcome, predictors, args_list)
  # postprocessing supported by epipredict
  postproc <- frosting()
  postproc %<>% arx_postprocess(trainer, args_list)
  if (pop_scaling) {
    postproc %<>% layer_population_scaling(
      .pred, .pred_distn,
      df = epipredict::state_census,
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
      data_coloring(outcome, learned_params, join_cols = key_colnames(epi_data, exclude = "time_value")) %>%
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
