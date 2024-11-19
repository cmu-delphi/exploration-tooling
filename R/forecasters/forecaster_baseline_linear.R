#' epi_data is expected to have: geo_value, time_value, and value columns.
forecaster_baseline_linear <- function(epi_data) {
  df_processed <- epi_data %>%
    mutate(epiweek = epiweek(time_value), epiyear = epiyear(time_value)) %>%
    left_join(
      (.) %>%
        distinct(epiweek, epiyear) %>%
        mutate(
          season = convert_epiweek_to_season(epiyear, epiweek),
          season_week = convert_epiweek_to_season_week(epiyear, epiweek)
        ),
      by = c("epiweek", "epiyear")
    ) %>%
    left_join(
      tar_read(hhs_evaluation_data) %>% distinct(geo_value, population),
      by = "geo_value"
    ) %>%
    mutate(value = ifelse(value == 0, NA, value / population * 10**5)) %>%
    arrange(geo_value, time_value) %>%
    group_by(geo_value) %>%
    fill(value)

  tau <- covidhub_probs()
  # Fit lines per geo_value
  train_data <- df_processed %>%
    filter(time_value >= max(time_value) - 30)
  point_forecast <- tibble(
    geo_value = unique(train_data$geo_value),
    model = map(geo_value, ~ lm(value ~ season_week, data = train_data %>% filter(geo_value == .x))),
    value = map(model, ~ predict(.x, newdata = data.frame(season_week = max(train_data$season_week) + 1:3)))
  ) %>%
    unnest_longer(col = value, indices_to = "season_week") %>%
    mutate(season_week = max(train_data$season_week) + as.double(season_week))
  pooled_residuals <- point_forecast$model %>%
    map(~ residuals(.x) %>% as.numeric()) %>%
    unlist()
  get_quantile <- function(point, ahead) {
    epipredict:::propagate_samples(pooled_residuals, point, tau, aheads = 1:3, nsim = 1e3, symmetrize = TRUE, nonneg = TRUE)[[1]] %>%
      slice(.env$ahead) %>%
      pull(.pred_distn)
  }
  quantile_forecast <- point_forecast %>%
    rowwise() %>%
    mutate(quantile = get_quantile(value, season_week - max(train_data$season_week)) %>% nested_quantiles()) %>%
    unnest(quantile) %>%
    left_join(
      tar_read(hhs_evaluation_data) %>% distinct(geo_value, population),
      by = "geo_value"
    ) %>%
    rename(quantile = quantile_levels) %>%
    mutate(
      value = values * population / 10**5,
      forecaster = "linear",
      target_end_date = max(train_data$time_value) + (season_week - max(train_data$season_week)) * 7,
      forecast_date = max(train_data$time_value),
    ) %>%
    select(-model, -values, -population, -season_week)
}
