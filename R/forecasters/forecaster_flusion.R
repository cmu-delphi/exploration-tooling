flusion <- function(
  epi_data,
  outcome,
  extra_sources = character(),
  ahead = 7,
  pop_scaling = FALSE,
  trainer = rand_forest(
    engine = "grf_quantiles",
    mode = "regression"
  ),
  quantile_levels = covidhub_probs(),
  drop_non_seasons = FALSE,
  scale_method = c("quantile", "std", "none"),
  center_method = c("median", "mean", "none"),
  nonlin_method = c("quart_root", "none"),
  dummy_states = TRUE,
  dummy_source = TRUE,
  sources_to_pop_scale = c(),
  derivative_estimator = c("growth_rate", "quadratic_regression", "none"),
  difference = FALSE,
  ...
) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  derivative_estimator <- arg_match(derivative_estimator)

  epi_data <- validate_epi_data(epi_data)
  extra_sources <- unlist(extra_sources)

  # perform any preprocessing not supported by epipredict
  args_input <- list(...)
  # this next part is basically unavoidable boilerplate you'll want to copy
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, ahead, args_input, outcome, extra_sources)) {
    return(make_null_forecast())
  }
  # this is to deal with grouping by source in tests that don't include it
  adding_source <- FALSE
  if (!("source" %in% names(epi_data))) {
    adding_source <- TRUE
    epi_data$source <- c("nhsn")
    attributes(epi_data)$metadata$other_keys <- "source"
  }
  if ("season_week" %nin% names(epi_data) | "season" %nin% names(epi_data)) {
    epi_data %<>% add_season_info()
  }

  if (!("population" %in% names(epi_data))) {
    epi_data %<>% add_pop_and_density()
    epi_data %<>% mutate(agg_level = ifelse(is.na(agg_level), "state", agg_level))
  }
  time_type <- attributes(epi_data)$metadata$time_type
  # because we're whitening, we don't want to threshold the predictions inside epipredict
  args_input[["nonneg"]] <- scale_method == "none"
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(default_args_list, args_input)
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources)
  c(args_list, predictors, trainer) %<-%
    sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  # flusurv is out of date, so we need to drop it from latency considerations,
  # and several states are also no longer participating in ILI+ (with wy only having `NA` values)
  # need factors for most things
  epi_data %<>% ungroup() %>% mutate(across(where(is.character), as.factor))
  # drop between-season values for actual training; we'll need them for prediction though
  full_data <- epi_data
  # only train on the season, but we need the off-season data for prediction purposes
  season_data <- full_data %>%
    drop_non_seasons(min_window = 32)
  # preprocessing supported by epipredict
  preproc <- epi_recipe(full_data)
  if (scale_method != "none") {
    preproc %<>% step_epi_whitening(
      colname = predictors,
      scale_method = scale_method,
      center_method = center_method,
      nonlin_method = nonlin_method
    )
  }
  preproc %<>%
    step_epi_rolling_stats(colname = predictors, mean_width = 2) %>%
    step_epi_rolling_stats(colname = predictors, mean_width = 4)
  if (derivative_estimator == "quadratic_regression") {
    preproc %<>% step_epi_poly_coefs(
      colname = predictors[[1]],
      windows = c(quad4 = 4L, quad6 = 6L, lin3 = 3L, lin5 = 5L),
      degree = 2L,
      slide_window = as.difftime(6, units = "weeks")
    )
  }
  if (pop_scaling && !is.null(sources_to_pop_scale)) {
    preproc %<>%
      step_population_scaling(
        sources_to_pop_scale,
        df = epidatasets::state_census,
        df_pop_col = "pop",
        create_new = FALSE,
        rate_rescaling = 1e5,
        by = c("geo_value" = "abbr")
      )
  }
  if (derivative_estimator == "quadratic_regression") {
    preproc %<>%
      add_role(matches("(lin|quad)[0-9]_c[1-3]"), new_role = "pre-predictor")
  }
  preproc %<>%
    add_role(starts_with("slide_value"), new_role = "pre-predictor")
  # one-hot encoding of the data source
  if (all(levels(epi_data$source) != "nhsn") && dummy_source) {
    preproc %<>% step_dummy(source, one_hot = TRUE, keep_original_cols = TRUE, role = "pre-predictor")
  }
  # one-hot encoding of location
  if (dummy_states) {
    preproc %<>% step_dummy(geo_value, one_hot = TRUE, keep_original_cols = TRUE, role = "pre-predictor")
  }
  # one-hot encoding of scale (probably redundant with geo_value)
  # population and density
  preproc %<>%
    add_role(population, density, new_role = "pre-predictor") %>%
    # week of the year
    step_date(time_value, features = "week") %>%
    # week of the season
    add_role(season_week, new_role = "pre-predictor")
  if (derivative_estimator == "growth_rate") {
    preproc %<>% step_growth_rate(all_of(predictors), horizon = 3 * 7, log_scale = FALSE, role = "pre-predictor")
    # add a second difference
    preproc %<>% step_growth_rate(matches("gr_[0-9]*_rel_change_.*"), horizon = 2 * 7, log_scale = FALSE)
  }
  preproc %<>% arx_preprocess(outcome, predictors, args_list)
  # postprocessing supported by epipredict
  postproc <- frosting()
  postproc %<>% arx_postprocess(trainer, args_list)
  if (pop_scaling) {
    postproc %<>%
      layer_population_scaling(
        .pred,
        .pred_distn,
        df = epidatasets::state_census,
        df_pop_col = "pop",
        create_new = FALSE,
        rate_rescaling = 1e5,
        by = c("geo_value" = "abbr")
      )
  }
  if (scale_method != "none") {
    postproc %<>% layer_epi_coloring(colname = outcome, nonlin_method = nonlin_method)
  }
  pred_final <- run_workflow_and_format(
    preproc,
    postproc,
    trainer,
    season_data,
    full_data
  ) %>% mutate(value = pmax(0, value))
  if (adding_source) {
    pred_final %<>% select(-source)
  }
  gc()
  return(pred_final)
}
