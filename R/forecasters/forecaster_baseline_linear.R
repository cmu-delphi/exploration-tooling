#' epi_data is expected to have: geo_value, time_value, and value columns.
forecaster_baseline_linear <- function(epi_data, ahead, log = FALSE, sort = FALSE) {
  forecast_date <- attributes(epi_data)$metadata$as_of

  browser()
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
      get_population_data() %>% rename(geo_value = state_id) %>% distinct(geo_value, population),
      by = "geo_value"
    ) %>%
    mutate(value = value / population * 10**5)

  if (log) {
    df_processed <- df_processed %>% mutate(value = log(value))
  }
  if (sort) {
    df_processed <- df_processed %>%
      group_by(geo_value) %>%
      mutate(value = sort(value))
  }

  forecast_season_week <- convert_epiweek_to_season_week(epiyear(forecast_date), epiweek(forecast_date))
  target_season_week <- forecast_season_week + ahead
  geos <- unique(df_processed$geo_value)

  train_data <- df_processed %>%
    filter(time_value >= max(time_value) - 30)
  # group_by(geo_value) %>%
  # filter(!is.na(value)) %>%
  # filter(n() >= 2)

  point_forecast <- tibble(
    geo_value = train_data$geo_value %>% unique(),
    model = map(geo_value, ~ lm(value ~ season_week, data = train_data %>% filter(geo_value == .x))),
    value = map_dbl(model, ~ predict(.x, newdata = data.frame(season_week = target_season_week))),
    season_week = target_season_week
  )

  missing_geos <- setdiff(geos, unique(point_forecast$geo_value))

  point_forecast <- bind_rows(
    point_forecast,
    expand_grid(
      geo_value = missing_geos,
      tibble(
        season_week = target_season_week,
        value = point_forecast %>%
          pull(value) %>%
          median()
      )
    )
  )

  pooled_residuals <- point_forecast$model %>%
    map(~ residuals(.x) %>% as.numeric()) %>%
    unlist()
  pooled_residuals <- 3 * pooled_residuals

  if (FALSE) {
    # Debug residuals
    saveRDS(pooled_residuals, "pooled_residuals.rds")
  }

  get_quantile <- function(point, ahead) {
    epipredict:::propagate_samples(pooled_residuals, point, quantile_levels = covidhub_probs(), aheads = aheads, nsim = 1e3, symmetrize = TRUE, nonneg = TRUE)[[1]] %>%
      slice(.env$ahead) %>%
      pull(.pred_distn)
  }
  quantile_forecast <- point_forecast %>%
    rowwise() %>%
    mutate(quantile = get_quantile(value, season_week - forecast_season_week) %>% nested_quantiles()) %>%
    unnest(quantile) %>%
    left_join(
      get_population_data() %>% rename(geo_value = state_id) %>% distinct(geo_value, population),
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

  if (log) {
    quantile_forecast <- quantile_forecast %>% mutate(values = exp(values))
  }

  if (FALSE) {
    # Show fit on the train values
    train_weeks <- df_processed %>%
      filter(time_value >= max(time_value) - 30) %>%
      pull(season_week) %>%
      unique()
    fit_points <- tibble(
      geo_value = train_data$geo_value %>% unique(),
      model = map(geo_value, ~ lm(value ~ season_week, data = train_data %>% filter(geo_value == .x))),
      prediction = map(model, ~ tibble(season_week = train_weeks, value = predict(.x, newdata = data.frame(season_week = train_weeks))))
    ) %>%
      unnest_longer(col = prediction) %>%
      mutate(value = prediction$value, season_week = prediction$season_week) %>%
      left_join(
        get_population_data() %>% rename(geo_value = state_id) %>% distinct(geo_value, population),
        by = "geo_value"
      ) %>%
      mutate(
        value = value * population / 10**5,
        forecaster = "linear",
        target_end_date = max(train_data$time_value) + (season_week - max(train_data$season_week)) * 7,
        forecast_date = max(train_data$time_value),
      ) %>%
      select(-prediction, -model, -population, -season_week)

    quantile_forecast %>% bind_rows(
      fit_points
    )
  } else {
    quantile_forecast
  }
}
