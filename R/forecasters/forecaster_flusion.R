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
                    scale_method = c("quantile", "std"),
                    center_method = c("median", "mean"),
                    sources_to_pop_scale = c(),
                    ...) {
  # perform any preprocessing not supported by epipredict
  # this is to deal with grouping by source in tests that don't include it
  if (!("source" %in% names(epi_data))) {
    epi_data$source <- "none"
  }
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)

  args_input <- list(...)
  # because we're whitening, we don't want to threshold the predictions inside epipredict
  args_input[["nonneg"]] <- FALSE
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
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- do.call(arx_args_list, args_input)
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources)
  # TODO: Partial match quantile_level coming from here (on Dmitry's machine)
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)
  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  # need factors for most things
  epi_data %<>% ungroup() %>% mutate(across(where(is.character), as.factor))
  # drop between-season values for actual training; we'll need them for prediction though
  full_data <- epi_data
  season_data <- epi_data %>% drop_non_seasons()
  # whiten to get the sources on the same scale
  learned_params <- calculate_whitening_params(season_data, predictors, scale_method, center_method)
  full_data %<>% data_whitening(predictors, learned_params)
  keys <- epipredict:::kill_time_value(key_colnames(epi_data))
  # add the slightly smoothed values beforehand; this is about speed, since step_epi_slide isn't ready yet
  full_data %<>%
    group_by(across(all_of(keys))) %>%
    epi_slide_mean(
      predictors,
      .window_size = as.difftime(2, units = "weeks")
    ) %>%
    rename_with(~ gsub("slide_value_", "slide_value_1wk_", .x)) %>%
    epi_slide_mean(
      predictors,
      .window_size = as.difftime(4, units = "weeks"),
    ) %>%
    rename_with(~ gsub("slide_value_(?!1wk)", "slide_value_3wk_", .x, perl = TRUE))
  # this is actually for 7-8, epi_slide just needs an actual epi_df to run, and several of the later operations don't maintain type stability
  # also, they're in the same step to speed up the epi_slide
  full_data_with_derivatives <- full_data %<>%
    group_by(across(all_of(keys))) %>%
    epi_slide(
      .f = \(x, gk, rtv) {
        rel_col <- x$value
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
  # only train on the season, but we need the off-season data for prediction purposes
  season_data <- full_data_with_derivatives %>%
    drop_non_seasons()

  # preprocessing supported by epipredict
  preproc <- epi_recipe(season_data)
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
  preproc %<>%
    add_role(starts_with("slide_value"), new_role = "predictor") %>%
    # anything similar to quad4_c3, or lin3_c2, etc
    add_role(matches("(lin|quad)[0-9]_c[1-3]"), new_role = "predictor") %>%
    # one-hot encoding of the data source
    step_dummy(source, one_hot = TRUE, keep_original_cols = TRUE) %>%
    # one-hot encoding of location
    step_dummy(geo_value, one_hot = TRUE, keep_original_cols = TRUE) %>%
    # one-hot encoding of scale (probably redundant with geo_value)
    # population and density
    add_role(population, density, new_role = "predictor") %>%
    # week of the year
    step_date(time_value, features = "week") %>%
    # distance to christmas
    step_mutate(dist_to_christmas = (time_value_week - 52)) %>%
    # week of the season
    add_role(season_week, new_role = "predictor")
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
  pred <- run_workflow_and_format(preproc, postproc, trainer, season_data, full_data_with_derivatives)
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict
  # reintroduce color into the value
  pred_final <- data_coloring(pred, "value", learned_params)
  return(pred_final)
}

#' this is semi temporary to apply the same logic twice to the with and the without
local_pre_slide <- function(epi_data) {
}

#' for training, we don't want off-season times or anomalous seasons, but for
#' prediction we do
drop_non_seasons <- function(epi_data) {
  epi_data %>% filter(season_week > 35, season != "2020/21", season != "2021/22", season != "2008/09")
}

