flusion <- function(epi_data,
                    outcome,
                    extra_sources = "",
                    ahead = 7,
                    pop_scaling = FALSE,
                    trainer = parsnip::linear_reg(),
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
  # this is a temp fix until a real fix gets put into epipredict
  epi_data <- clear_lastminute_nas(epi_data, outcome, extra_sources)
  # one that every forecaster will need to handle: how to manage max(time_value)
  # that's older than the `as_of` date
  c(epi_data, effective_ahead) %<-% extend_ahead(epi_data, ahead)
  # see latency_adjusting for other examples

  # because we're whitening, we don't want to threshold the predictions inside epipredict
  args_input[["nonneg"]] <- FALSE

  # this next part is basically unavoidable boilerplate you'll want to copy
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, effective_ahead, args_input, outcome, extra_sources)) {
    null_result <- tibble(
      geo_value = character(),
      forecast_date = lubridate::Date(),
      target_end_date = lubridate::Date(),
      quantile = numeric(),
      value = numeric()
    )
    return(null_result)
  }
  args_input[["ahead"]] <- effective_ahead
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
  epi_data %<>% drop_non_seasons()
  # whiten to get the sources on the same scale
  learned_params <- calculate_whitening_params(epi_data, predictors, scale_method, center_method)
  full_data %<>% data_whitening(predictors, learned_params)
  keys <- epipredict:::kill_time_value(key_colnames(epi_data))
  # add the slightly smoothed values beforehand; this is about speed, since step_epi_slide isn't ready yet
  full_data %<>%
    group_by(across(all_of(keys))) %>%
    epi_slide_mean(
      predictors,
      before = as.difftime(2 - 1, units = "weeks"),
      after =  as.difftime(0, units = "weeks")
    ) %>%
    rename_with(~ gsub("slide_value_", "slide_value_1wk_", .x)) %>%
    epi_slide_mean(
      predictors,
      before = as.difftime(4 - 1, units = "weeks"),
      after =  as.difftime(0, units = "weeks")
    ) %>%
    rename_with(~ gsub("slide_value_(?!1wk)", "slide_value_3wk_", .x, perl = TRUE)) %>% ungroup()
  # only train on the season, but we need the off-season data for training purposes
  epi_data <- full_data %>%
    drop_non_seasons()

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
  preproc %<>%
    # this is actually for 7-8, epi_slide just needs an actual epi_df to run, and several of the later operations don't maintain type stability
    step_epi_slide(value,
                   .f = \(x, gk, rtv) {
                     get_poly_coefs(x, degree = 2, n_points = 4) },
                   f_name = "quad4",
                   before = as.difftime(4-1, units = "weeks"),
                   after =  as.difftime(0, units = "weeks")) %>%
    step_epi_slide(value,
                   .f = \(x, gk, rtv) {
                     get_poly_coefs(x, degree = 2, n_points = 6)
                   },
                   f_name = "quad6",
                   before = as.difftime(6-1, units = "weeks"),
                   after =  as.difftime(0, units = "weeks")) %>%
    step_epi_slide(value,
                   .f = \(x, gk, rtv) {
                     get_poly_coefs(x, degree = 1, n_points = 3)
                   },
                   f_name = "lin3",
                   before = as.difftime(3-1, units = "weeks"),
                   after =  as.difftime(0, units = "weeks")) %>%
    step_epi_slide(value,
                   .f = \(x, gk, rtv) {
                     get_poly_coefs(x, degree = 1, n_points = 5)
                   },
                   f_name = "lin5",
                   before = as.difftime(5-1, units = "weeks"),
                   after =  as.difftime(0, units = "weeks")) %>%
    # slide is currently done before for efficiency reasons, added as predictors here
    add_role(starts_with("slide_value"), new_role = "predictor") %>%
    # one-hot encoding of the data source
    step_dummy(source, one_hot = TRUE, keep_original_cols = TRUE) %>%
    # one-hot encoding of location
    step_dummy(geo_value, one_hot = TRUE, keep_original_cols = TRUE) %>%
    # one-hot encoding of scale (probably redundant with geo_value)
    add_role(population, new_role = "predictor") %>%
    # population (included in data creation)
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
  pred <- run_workflow_and_format(preproc, postproc, trainer, full_data)
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict
  #
  # reintroduce color into the value
  pred <- data_coloring(pred, "value", learned_params)
  return(pred)
}

#' for training, we don't want off-season times or anomalous seasons, but for
#' prediction we do
drop_non_seasons <- function(epi_data) {
  epi_data %>% filter(season_week > 35, season != "2020/21", season != "2021/22", season != "2008/09")
}
