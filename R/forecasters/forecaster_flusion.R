flusion <- function(epi_data,
                    outcome,
                    extra_sources = "",
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
                    ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  derivative_estimator <- arg_match(derivative_estimator)
  # perform any preprocessing not supported by epipredict
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
  # this is to deal with grouping by source in tests that don't include it
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
  time_type <- attributes(epi_data)$metadata$time_type
  # because we're whitening, we don't want to threshold the predictions inside epipredict
  args_input[["nonneg"]] <- scale_method == "none"
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(default_args_list, args_input)
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources)
  # TODO: Partial match quantile_level coming from here (on Dmitry's machine)
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  # flusurv is out of date, so we need to drop it from latency considerations,
  # and several states are also no longer participating in ILI+ (with wy only having `NA` values)
  # need factors for most things
  epi_data %<>% ungroup() %>% mutate(across(where(is.character), as.factor))
  # drop between-season values for actual training; we'll need them for prediction though
  full_data <- epi_data
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }

  # whiten to get the sources on the same scale
  learned_params <- calculate_whitening_params(season_data, predictors, scale_method, center_method, nonlin_method)
  full_data %<>% data_whitening(predictors, learned_params, nonlin_method)
  keys <- key_colnames(epi_data, exclude = "time_value")
  # add the slightly smoothed values beforehand; this is about speed, since step_epi_slide isn't ready yet
  full_data %<>%
    group_by(across(all_of(keys))) %>%
    epi_slide_mean(
      all_of(predictors),
      .window_size = as.difftime(2, units = glue::glue("{time_type}s"))
    ) %>%
    rename_with(~ gsub("slide_value_", "slide_value_2_", .x)) %>%
    epi_slide_mean(
      all_of(predictors),
      .window_size = as.difftime(4, units = glue::glue("{time_type}s")),
    ) %>%
    rename_with(~ gsub("slide_value_(?!1wk)", "slide_value_4_", .x, perl = TRUE))
  # this is actually for 7-8, epi_slide just needs an actual epi_df to run, and several of the later operations don't maintain type stability
  # also, they're in the same step to speed up the epi_slide
  if (derivative_estimator == "quadratic_regression") {
    full_data <- full_data %<>%
      group_by(across(all_of(keys))) %>%
      epi_slide(
        .f = \(x, gk, rtv) {
          rel_col <- x[[predictors[1]]]
          quad4 <- rel_col %>%
            tail(n = 4) %>%
            get_poly_coefs(degree = 2, n_points = 4) %>%
            setNames(paste0("quad4_", names(.)))
          quad6 <- rel_col %>%
            tail(n = 6) %>%
            get_poly_coefs(degree = 2, n_points = 6) %>%
            setNames(paste0("quad6_", names(.)))
          lin3 <- rel_col %>%
            tail(n = 3) %>%
            get_poly_coefs(degree = 2, n_points = 3) %>%
            setNames(paste0("lin3_", names(.)))
          lin5 <- rel_col %>%
            tail(n = 5) %>%
            get_poly_coefs(degree = 2, n_points = 5) %>%
            setNames(paste0("lin5_", names(.)))
          return(bind_cols(quad4, quad6, lin3, lin5))
        },
        .window_size = as.difftime(6, units = "weeks")
      )
  }
  # only train on the season, but we need the off-season data for prediction purposes
  season_data <- full_data %>%
    drop_non_seasons(min_window = 32)
  # preprocessing supported by epipredict
  preproc <- epi_recipe(full_data)
  if (pop_scaling && !is.null(sources_to_pop_scale)) {
    preproc %<>% step_population_scaling(
      sources_to_pop_scale,
      df = epidatasets::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  # slide is currently done before for efficiency reasons, added as predictors here
  if (derivative_estimator == "quadratic_regression") {
    # anything similar to quad4_c3, or lin3_c2, etc
    preproc %<>%
      add_role(matches("(lin|quad)[0-9]_c[1-3]"), new_role = "pre-predictor")
  }
  preproc %<>%
    add_role(all_of(starts_with("slide_value")), new_role = "pre-predictor")
  # one-hot encoding of the data source
  if (all(levels(epi_data$source) != "none") && dummy_source) {
    preproc %<>% step_dummy(source, one_hot = TRUE, keep_original_cols = TRUE, role = "pre-predictor")
  }
  # one-hot encoding of location
  if (dummy_states) {
    preproc %<>% step_dummy(geo_value, one_hot = TRUE, keep_original_cols = TRUE, role = "pre-predictor")
  }
  # one-hot encoding of scale (probably redundant with geo_value)
  # population and density
  preproc %<>% add_role(population, density, new_role = "pre-predictor") %>%
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
    trainer, season_data, full_data
  )
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict
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
