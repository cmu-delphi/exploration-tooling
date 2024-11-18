if (FALSE) {
  suppressPackageStartupMessages(source(here::here("R", "load_all.R")))
  forecast_date <- as.Date("2024-11-20")
  epi_data <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfflunewadm")
  epi_data <- epi_data %>%
    mutate(
      epiweek = epiweek(weekendingdate),
      epiyear = epiyear(weekendingdate)
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
  epi_data <- epi_data %>%
    mutate(time_value = as.Date(weekendingdate), geo_value = tolower(jurisdiction), nhsn = totalconfflunewadm) %>%
    select(-weekendingdate, -jurisdiction, -totalconfflunewadm)
  ahead <- 0
  window_size <- 3
  recent_window <- 3

  epi_data
  effective_ahead
}

climatological_model <- function(epi_data, forecast_date, ahead, window_size = 3, recent_window = 3, quantile_method = c("baseR", "epipredict"), quant_type = 8, geo_agg = FALSE, disease = c("flu", "covid")) {
  quantile_method <- arg_match(quantile_method)
  disease <- arg_match(disease)
  forecast_week <- epiweek(forecast_date)
  last_week_data <- epi_data %>%
    filter(time_value > "2024-08-01") %>%
    pull(epiweek) %>%
    max()
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
    filter(!is.na(nhsn))
    # drop weird years
  if (disease == "flu") {
    filtered %<>% filter((season != "2020/21") & (season != "2021/22"))
  }
    # keep data either within the window, or within the past window weeks
    filtered %<>% filter(
      (abs(forecast_week + ahead - epiweek) <= window_size) |
        (last_date_data - time_value <= recent_window * 7)
    )
  if (geo_agg) {
    filtered %<>%
      add_pop_and_density() %>%
      mutate(nhsn = nhsn / population * 1e5) %>%
      select(geo_value, epiweek, epiyear, season, season_week, nhsn, population)
  } else {
    filtered %<>%
      group_by(geo_value)
  }
  filtered %>% filter(geo_value == "usa")
  naive_preds <- filtered %>%
    reframe(
      enframe(
        quantile(nhsn, probs = probs, na.rm = TRUE, type = quant_type),
        name = "quantile"
      )
    ) %>%
    mutate(
      forecast_date = forecast_date,
      target_end_date = forecast_date + ahead * 7,
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
  naive_preds %<>% mutate(value = pmax(0, value))
  if (geo_agg) {
    naive_preds %<>%
      expand_grid(
        filtered %>% distinct(geo_value, population)
      ) %>%
      mutate(value = value * population / 1e5) %>%
      select(-population) %>%
      select(geo_value, forecast_date, target_end_date, quantile, value) %>%
      arrange(geo_value, forecast_date, target_end_date)
  }
  naive_preds %>% mutate(value = pmax(0, value))
}
