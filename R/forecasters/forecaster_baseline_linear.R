#' epi_data is expected to have: geo_value, time_value, and value columns.
forecaster_baseline_linear <- function(epi_data, ahead, log = FALSE, sort = FALSE, residual_tail = 0.85, residual_center = 0.085) {
  forecast_date <- attributes(epi_data)$metadata$as_of
  population_data <- get_population_data() %>%
    rename(geo_value = state_id) %>%
    distinct(geo_value, population)
  df_processed <- epi_data %>%
    left_join(population_data, by = "geo_value") %>%
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
  reference_date <- MMWRweek2Date(epiyear(forecast_date), epiweek(forecast_date)) + 6
  target_season_week <- forecast_season_week + ahead
  geos <- unique(df_processed$geo_value)
  # only train on the last 30 days of data
  train_data <-
    df_processed %>%
    filter(time_value >= max(time_value) - 30) %>%
    group_by(geo_value) %>%
    filter(!(is.na(value) | is.infinite(value))) %>%
    filter(n() >= 2) %>%
    mutate(weeks_back = as.integer(time_value - epi_as_of(df_processed)) / 7)

  point_forecast <- tibble(
    geo_value = train_data$geo_value %>% unique(),
    model = map(geo_value, ~ lm(value ~ weeks_back, data = train_data %>% filter(geo_value == .x))),
    value = map_dbl(model, ~ predict(.x, newdata = data.frame(weeks_back = ahead))),
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

  # residuals from the data
  residuals <- point_forecast$model %>%
    map(~ residuals(.x) %>% as.numeric()) %>%
    unlist()
  residuals <- c(residuals, -residuals)

  # Jank attempt to get saner residuals
  if (!log) {
    abs_residuals <- abs(residuals)
    abs_residuals <- abs_residuals[abs_residuals < quantile(abs_residuals, residual_tail) & abs_residuals > quantile(abs_residuals, residual_center)]
    residuals <- c(abs_residuals, -abs_residuals)
  }

  if (FALSE) {
    # Debug residuals
    saveRDS(residuals, "pooled_residuals.rds")
  }

  get_quantile <- function(point, ahead) {
    epipredict:::propagate_samples(residuals, point, quantile_levels = covidhub_probs(), aheads = ahead + 2, nsim = 1e4, symmetrize = TRUE, nonneg = FALSE)[[1]] %>%
      pull(.pred_distn)
  }
  quantile_forecast <- point_forecast %>%
    rowwise() %>%
    mutate(quantile = get_quantile(value, ahead) %>% nested_quantiles()) %>%
    unnest(quantile) %>%
    left_join(population_data, by = "geo_value") %>%
    rename(quantile = quantile_levels) %>%
    {
      if (log) {
        (.) %>% mutate(values = exp(values))
      } else {
        .
      }
    } %>%
    mutate(
      value = values * population / 10**5,
      target_end_date = reference_date + ahead * 7,
      forecast_date = forecast_date,
    ) %>%
    select(-model, -values, -population, -season_week) %>%
    mutate(value = pmax(0, value))
}
