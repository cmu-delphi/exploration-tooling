# this is adding the data downloaded from the forecast eval dashboard

data_to_add <- aws.s3::s3readRDS(object = "score_cards_state_hospitalizations.rds", bucket = "forecast-eval")
names(data_to_add)
data_to_add %>% glimpse()
data_to_add$forecaster %>% unique()
n_geos <- data_to_add$geo_value %>%
  unique() %>%
  length()
more_than_one_year_forecasters <- data_to_add %>%
  group_by(forecaster) %>%
  filter(target_end_date > as.Date("2021-01-01")) %>%
  summarize(enough = length(wis) / n_geos) %>%
  filter(enough > 365) %>%
  pull(forecaster)
best_15_forecasters <- data_to_add %>%
  filter(forecaster %in% more_than_one_year_forecasters) %>%
  group_by(forecaster) %>%
  summarize(net_wis = sum(wis, na.rm = TRUE) / length(!is.na(wis))) %>%
  arrange(net_wis) %>%
  `[`(1:15, ) %>%
  pull(forecaster)
best_15_forecasters
internal_scores <- qs::qread("covid_hosp_explore/legacy-exploration-scorecards.qs")
new_data <- internal_scores %>%
  add_row(
    data_to_add %>%
      filter(forecaster %in% best_15_forecasters) %>%
      select(
        -c(
          "cov_10",
          "cov_20",
          "cov_30",
          "cov_40",
          "cov_50",
          "cov_60",
          "cov_70",
          "cov_90",
          "cov_95",
          "cov_98",
          "value_20",
          "value_50",
          "value_80",
          "sharpness",
          "overprediction",
          "underprediction"
        )
      )
  )
new_data$forecaster %>% unique()
new_data
qs::qsave(new_data, "covid_hosp_explore/legacy-exploration-scorecards.qs")
setdiff(names(data_to_add), names(internal_scores))

external_scores <- targets::tar_read("external_scores")
external_scores
