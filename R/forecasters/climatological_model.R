#' predict using the quantiles of the past values in the region around
#' @param epi_data expected to have columns time_value, geo_value, season, value,
climatological_model <- function(
  epi_data,
  ahead,
  window_size = 3,
  recent_window = 3,
  quantile_method = c("baseR", "epipredict"),
  quant_type = 8,
  geo_agg = FALSE,
  floor_value = 0,
  pop_scale = FALSE,
  include_forecast_date = TRUE
) {
  quantile_method <- arg_match(quantile_method)
  forecast_date <- attributes(epi_data)$metadata$as_of
  forecast_week <- epiweek(forecast_date)
  last_date_data <- epi_data %>%
    pull(time_value) %>%
    max()
  if (quantile_method == "epipredict") {
    probs <- c(.25, .5, .75)
  } else {
    probs <- covidhub_probs()
  }
  filtered <-
    epi_data %>%
    filter(!is.na(value))
  # drop weird years
  filtered %<>% filter((season != "2020/21") & (season != "2021/22"))
  # keep data either within the window, or within the past window weeks
  if (include_forecast_date) {
    filtered %<>%
      filter(
        (abs(forecast_week + ahead - epiweek) <= window_size) |
          (last_date_data - time_value <= recent_window * 7)
      )
  } else {
    filtered %<>%
      filter(
        (abs(forecast_week + ahead - epiweek) <= window_size)
      )
  }
  # filtered %>% ggplot(aes(x = epiweek, y = value, color = source)) + geom_point() + facet_wrap(~geo_value); epi_data %>% autoplot(value, .facet_by = "geo_value", color = "source")
  if (geo_agg && pop_scale) {
    filtered %<>%
      add_pop_and_density() %>%
      mutate(value = value / population * 1e5) %>%
      select(any_of(c("geo_value", "epiweek", "epiyear", "season", "season_week", "value", "population")))
  } else if (!geo_agg) {
    filtered %<>%
      group_by(geo_value)
  }
  naive_preds <- filtered %>%
    reframe(
      enframe(
        quantile(value, probs = probs, na.rm = TRUE, type = quant_type),
        name = "quantile"
      )
    ) %>%
    mutate(
      forecast_date = forecast_date,
      target_end_date = get_forecast_reference_date(forecast_date) + ahead * 7,
      quantile = as.numeric(sub("%", "", quantile)) / 100
    )
  if (!geo_agg) {
    naive_preds %<>% group_by(geo_value)
  }
  if (quantile_method == "epipredict") {
    naive_preds %<>%
      filter(!is.na(value)) %>%
      group_by(forecast_date, target_end_date, .add = TRUE) %>%
      summarize(.dist_quantile = dist_quantiles(value, quantile), .groups = "keep") %>%
      reframe(tibble(quantile = covidhub_probs(), value = quantile(.dist_quantile, p = covidhub_probs())[[1]]))
  }
  naive_preds %<>% mutate(value = pmax(floor_value, value))
  if (geo_agg) {
    naive_preds %<>%
      expand_grid(
        filtered %>% distinct(geo_value)
      )
    if (pop_scale) {
      naive_preds %<>%
        left_join(
          filtered %>%
            distinct(geo_value, population)
        ) %>%
        mutate(value = value * population / 1e5)
    }
    naive_preds %<>%
      select(-any_of("population")) %>%
      select(geo_value, forecast_date, target_end_date, quantile, value) %>%
      arrange(geo_value, forecast_date, target_end_date)
  }
  naive_preds %>%
    mutate(value = pmax(floor_value, value)) %>%
    ungroup()
}
