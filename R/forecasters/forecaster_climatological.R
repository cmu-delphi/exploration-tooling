#' @params model_used the model used. "climate" means just climatological_model, "climate_linear" means the weighted ensemble with a linear model, "climatological_forecaster" means using the model from epipredict
#'
climate_linear_ensembled <- function(epi_data,
                                     outcome,
                                     extra_sources = "",
                                     ahead = 7,
                                     trainer = parsnip::linear_reg(),
                                     quantile_levels = covidhub_probs(),
                                     model_used = "climate_linear",
                                     filter_source = "",
                                     filter_agg_level = "",
                                     scale_method = c("quantile", "std", "none"),
                                     center_method = c("median", "mean", "none"),
                                     nonlin_method = c("quart_root", "none"),
                                     drop_non_seasons = FALSE,
                                     residual_tail = 0.99,
                                     residual_center = 0.35,
                                     ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)

  epi_data <- validate_epi_data(epi_data)

  args_list <- list(...)
  ahead <- as.integer(ahead / 7)
  epi_data %<>% filter_extraneous(filter_source, filter_agg_level)
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
        epiyear = epiyear(time_value)
      ) %>%
      left_join(
        (.) %>%
          distinct(epiweek, epiyear) %>%
          mutate(
            season = convert_epiweek_to_season(epiyear, epiweek),
            season_week = convert_epiweek_to_season_week(epiyear, epiweek)
          ),
        by = c("epiweek", "epiyear")
      )
  }
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }
  learned_params <- calculate_whitening_params(season_data, outcome, scale_method, center_method, nonlin_method)
  season_data %<>% data_whitening(outcome, learned_params, nonlin_method)
  # epi_data %>% drop_non_seasons() %>% ggplot(aes(x = time_value, y = hhs, color = source)) + geom_line() + facet_wrap(~geo_value)
  season_data <- season_data %>%
    select(geo_value, source, time_value, season, value = !!outcome) %>%
    mutate(epiweek = epiweek(time_value))
  if (model_used == "climate" || model_used == "climate_linear") {
    pred_climate <- climatological_model(season_data, ahead, geo_agg = FALSE, floor_value = min(season_data$value, na.rm = TRUE)) %>% mutate(forecaster = "climate")
    pred <- pred_climate %>% select(-forecaster)
  }

  # the linear prediction should always use nhsn/none
  if (model_used == "climate_linear") {
    pred_linear <- forecaster_baseline_linear(
      season_data %>% filter(source %in% c("nhsn", "none")),
      ahead,
      residual_tail = residual_tail,
      residual_center = residual_center,
      no_intercept = TRUE,
      floor_value = min(season_data$value, na.rm = TRUE, population_scale = FALSE)
    ) %>%
      mutate(forecaster = "linear")
    pred <- bind_rows(pred_climate, pred_linear) %>%
      ensemble_climate_linear((args_list$aheads[[1]]) / 7) %>%
      ungroup()
  } else if (model_used == "climatological_forecaster") {
    if (ahead == args_list$aheads[[1]][[1]] / 7) {
      clim_res <- climatological_forecaster(
        season_data,
        "value",
        args_list = climate_args_list(
          nonneg = (scale_method == "none"),
          time_type = "epiweek",
          quantile_levels = quantile_levels,
          forecast_horizon = args_list$aheads[[1]] / 7
        )
      )
      pred <- clim_res$predictions %>%
        filter(source %in% c("nhsn", "none")) %>%
        pivot_quantiles_longer(.pred_distn) %>%
        select(geo_value, forecast_date, target_end_date = target_date, value = .pred_distn_value, quantile = .pred_distn_quantile_level) %>%
        mutate(target_end_date = ceiling_date(target_end_date, unit = "weeks", week_start = 6))
    } else {
      # we're fitting everything all at once in the first ahead for the
      # climatological_forecaster, so just return a null result for the other
      # aheads
      null_result <- tibble(
        geo_value = character(),
        forecast_date = lubridate::Date(),
        target_end_date = lubridate::Date(),
        quantile = numeric(),
        value = numeric()
      )
      return(null_result)
    }
  }
  # undo whitening
  if (adding_source) {
    pred %<>%
      rename({{ outcome }} := value) %>%
      mutate(source = "none")
  } else {
    pred %<>%
      rename({{ outcome }} := value) %>%
      mutate(source = "nhsn")
  }
  pred_final <- pred %>%
    data_coloring(outcome, learned_params, join_cols = key_colnames(season_data, exclude = "time_value"), nonlin_method = nonlin_method) %>%
    rename(value = {{ outcome }}) %>%
    mutate(value = pmax(0, value)) %>%
    select(-source)
  # move dates to appropriate markers
  pred_final <- pred_final %>%
    mutate(target_end_date = target_end_date - 3) %>%
    sort_by_quantile()
  return(pred_final)
}
