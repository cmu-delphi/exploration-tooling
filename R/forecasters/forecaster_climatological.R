climate_linear_ensembled <- function(epi_data,
                                     outcome,
                                     extra_sources = "",
                                     ahead = 7,
                                     trainer = parsnip::linear_reg(),
                                     quantile_levels = covidhub_probs(),
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
  epi_data %<>% data_whitening(outcome, learned_params, nonlin_method)
  epi_data <- epi_data %>%
    select(geo_value, source, time_value, season, value = !!outcome) %>%
    mutate(epiweek = epiweek(time_value))
  pred_climate <- climatological_model(epi_data, ahead, floor_value = min(epi_data$value, na.rm = TRUE)) %>% mutate(forecaster = "climate")

  pred_geo_climate <- climatological_model(epi_data, ahead, geo_agg = FALSE, floor_value = min(epi_data$value, na.rm = TRUE)) %>% mutate(forecaster = "climate_geo")
  pred_linear <- forecaster_baseline_linear(epi_data, ahead, residual_tail = residual_tail, residual_center = residual_center, no_intercept = TRUE, floor_value = min(epi_data$value, na.rm = TRUE, population_scale = FALSE)) %>%
    mutate(forecaster = "linear")
  pred <- bind_rows(pred_climate, pred_linear, pred_geo_climate) %>%
    ensemble_climate_linear((args_list$aheads[[1]]) / 7) %>%
    ungroup()
  # undo whitening
  pred_final <- pred %>%
    rename({{ outcome }} := value) %>%
    mutate(source = "nhsn") %>%
    data_coloring(outcome, learned_params, join_cols = key_colnames(epi_data, exclude = "time_value"), nonlin_method = nonlin_method) %>%
    rename(value = {{ outcome }}) %>%
    mutate(value = pmax(0, value)) %>%
    select(-source)
  # move dates to appropriate markers
  pred_final <- pred_final %>%
    mutate(target_end_date = target_end_date - 3) %>%
    sort_by_quantile()
  return(pred_final)
}
