flusion <- function(epi_data,
                    outcome,
                    extra_sources = "",
                    ahead = 1,
                    pop_scaling = TRUE,
                    trainer = parsnip::linear_reg(),
                    quantile_levels = covidhub_probs(),
                       ...) {
  # perform any preprocessing not supported by epipredict
  # this is a temp fix until a real fix gets put into epipredict
  epi_data <- clear_lastminute_nas(epi_data, outcome, extra_sources)
  # one that every forecaster will need to handle: how to manage max(time_value)
  # that's older than the `as_of` date
  epidataAhead <- extend_ahead(epi_data, ahead)
  # see latency_adjusting for other examples
  # this next part is basically unavoidable boilerplate you'll want to copy
  epi_data <- epidataAhead[[1]]
  effective_ahead <- epidataAhead[[2]]
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

  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  if (pop_scaling) {
    preproc %<>% step_population_scaling(
      all_numeric(),
      df = epipredict::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  preproc %<>% arx_preprocess(outcome, predictors, args_list)
  preproc %<>%
    # this is actually for 7-8, epi_slide just needs an actual epi_df to run, and several of these don't maintain type stability
    step_epi_slide(value, .f = \(x, gk, rtv) get_poly_coefs(x, degree = 2, n_points = 4),f_name = "quad4", before = 4L-1L) %>%
    step_epi_slide(value, .f = \(x, gk, rtv) get_poly_coefs(x, degree = 2, n_points = 6),f_name = "quad6", before = 6L-1L) %>%
    step_epi_slide(value, .f = \(x, gk, rtv) get_poly_coefs(x, degree = 1, n_points = 3),f_name = "lin3", before = 3L-1L) %>%
    step_epi_slide(value, .f = \(x, gk, rtv) get_poly_coefs(x, degree = 1, n_points = 5),f_name = "lin5", before =  5L-1L) %>%
    step_epi_slide(value, .f = mean, before = 2L-1L) %>%
    step_epi_slide(value, .f = mean, before = 4L-1L) %>%
    # one-hot encoding of the data source
    step_dummy(source, one_hot = TRUE, keep_original_cols = TRUE) %>%
    # one-hot encoding of location
    step_dummy(geo_value, one_hot = TRUE, keep_original_cols = TRUE) %>%
    # one-hot encoding of scale (probably redundant with geo_value)
    # population (included in data creation)
    # week of the year
    step_date(time_value, features = "week") %>%
    # distance to christmas
    step_mutate(dist_to_christmas = (time_value_week - 52)) %>%
    # week of the season
    step_mutate_at(time_value_week, fn = \(t) (t-flu_season_start) %% 52)
  # still needs lags
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
  pred <- run_workflow_and_format(preproc, postproc, trainer, epi_data)
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict e.g. calibration
  return(pred)
}
