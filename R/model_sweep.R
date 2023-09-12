gen_cross_prod <- function(file, example_dataset) {
  source(file.path(base_dir, file))
  cross_product <- expand.grid(ahead = aheads, model = names(models), lags = sets_of_lags, training_window = training_windows, extra_sources = extra_sources, refit = refit)
  cross_product <- cross_product %>%
    mutate(training_window_pad = training_windows_pad[match(training_window, training_windows)], .keep = "all") %>%
    as_tibble()
  net_models <<- c(net_models, models)
  return(cross_product)
}

testing_forecast <- function(ahead, signals, model, lags = c(0L, 2L, 7L, 14L), extra_sources = c("chng"), quantiles = evalcast::covidhub_probs(), target = "hhs", training_window) {
  # check that we actually have `training_window` points
  smallest_window <- signals %>%
    group_by(geo_value) %>%
    summarize(time_range = max(time_value) - min(time_value)) %>%
    pull(time_range) %>%
    min()
  if (smallest_window < training_window) {
    abort(glue::glue("The smallest training window is {smallest_window}. Increase the padding to avoid this error"))
  }
  effective_ahead <- as.numeric(attributes(signals)$metadata$as_of - max(signals$time_value) + ahead)
  # in case there's not enough data, throw a more intelligible error telling the user that
  effective_ahead
  # edge case where there is no data
  if (is.infinite(effective_ahead)) {
    effective_ahead <- 0
    null_result <- tibble(geo_value = character(), .pred = numeric(), .pred_distn = numeric(), forecast_date = numeric(), target_date = numeric())
    return(null_result)
  }
  # only needed an empty string to prevent the tibble from going crazy
  if (extra_sources == c("")) {
    extra_sources <- c()
  }
  if (all(model == "flatline")) {
    res <- flatline_forecaster(signals, target,
      args_list = flatline_args_list(
        ahead = effective_ahead,
        levels = quantiles
      )
    )
    return(res %>% extract2("predictions"))
  } else {
    #

    forecast_date <- max(signals$time_value)
    preproc <- epi_recipe(signals) %>%
      step_population_scaling(
        c(target, extra_sources),
        df = relevant_state_census,
        df_pop_col = "pop",
        create_new = FALSE,
        rate_rescaling = 1e5,
        by = c("geo_value" = "abbr"),
      ) %>%
      step_epi_lag(c(target, extra_sources), lag = lags) %>%
      step_epi_ahead(target, ahead = effective_ahead) %>%
      step_epi_naomit() %>%
      step_training_window(n_recent = training_window)
    postproc <- frosting() %>%
      layer_predict() %>%
      layer_population_scaling(
        .pred,
        df = relevant_state_census,
        df_pop_col = "pop",
        create_new = FALSE,
        rate_rescaling = 1e5,
        by = c("geo_value" = "abbr"),
      ) %>%
      layer_threshold(starts_with(".pred")) %>%
      layer_residual_quantiles(probs = quantiles) %>%
      layer_add_forecast_date(forecast_date) %>%
      layer_add_target_date(forecast_date + effective_ahead) %>%
      layer_naomit(.pred)
    workflow <- epi_workflow(preproc, model) %>%
      fit(signals) %>%
      add_frosting(postproc)
    latest <- get_test_data(recipe = preproc, x = signals)
    pred <- predict(workflow, latest)
    return(pred)
  }
}
