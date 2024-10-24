# This script assumes that you have used `make pull` to download the needed
# forecasts and data from S3. Make sure that your project is set appropriately
# in .Renviron, for instance: TAR_PROJECT=flu_hosp_tiny.
source(here::here("R", "load_all.R"))

state_geo_values <- tar_read(hhs_evaluation_data) %>%
  pull(geo_value) %>%
  unique()

df <- tar_manifest()
forecasts <- df %>%
  filter(stringr::str_detect(name, "forecast_")) %>%
  pull(name) %>%
  map(~ tibble(model = ., tar_read_raw(.) %>% filter(geo_value %in% state_geo_values))) %>%
  bind_rows()
flu_2023_forecasts_scaled_1 <- forecasts %>%
  left_join(
    tar_read(hhs_evaluation_data) %>%
      select(geo_value, population) %>%
      distinct(),
    by = "geo_value"
  ) %>%
  mutate(prediction = prediction * population / 10L**5) %>%
  select(-source, -population)
s3save(flu_2023_forecasts_scaled_1, object = "flu_2023_forecasts_scaled_1.rds", bucket = "forecasting-team-data")


flu_2023_forecaster_scores_1 <- left_join(
  flu_2023_forecasts_scaled_1,
  tar_read(hhs_evaluation_data) %>% select(-population),
  by = c("geo_value", "target_end_date")
) %>%
  drop_na() %>%
  scoringutils::score(metrics = c("interval_score", "ae_median", "coverage")) %>%
  scoringutils::add_coverage(by = c("model", "geo_value", "forecast_date", "target_end_date"), ranges = c(80)) %>%
  scoringutils::summarize_scores(by = c("model", "geo_value", "forecast_date", "target_end_date")) %>%
  rename(wis = interval_score, ae = ae_median) %>%
  tibble()

s3save(flu_2023_forecaster_scores_1, object = "flu_2023_forecaster_scores_1.rds", bucket = "forecasting-team-data")
