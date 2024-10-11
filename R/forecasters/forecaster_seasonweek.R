#' predict the value using only the week in the season and any extra sources
season_week_forecaster <- function(epi_data,
                                   outcome,
                                   extra_sources = "",
                                   ahead = 7,
                                   pop_scaling = FALSE,
                                   trainer = parsnip::linear_reg(),
                                   quantile_levels = covidhub_probs(),
                                   smooth_width = 3,
                                   smooth_cols = NULL,
                                   sources_to_pop_scale = c(),
                                   ...) {
  if (is.null(smooth_cols)) {
    smooth_cols <- extra_sources
  }
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

  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  if (pop_scaling && !is.null(sources_to_pop_scale)) {
    preproc %<>% step_population_scaling(
      sources_to_pop_scale,
      df = epipredict::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  # slide is currently done before for efficiency reasons, added as predictors here
  if (any(predictors != "")) {
    preproc %>%
      add_role(starts_with("slide_"), new_role = "pre-predictor")
  }
  # population and density
  preproc %<>%
    add_role(population, density, new_role = "pre-predictor") %>%
    # week of the season
    add_role(season_week, new_role = "pre-predictor")
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
    trainer, epi_data
  )
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict
  return(pred)
}

