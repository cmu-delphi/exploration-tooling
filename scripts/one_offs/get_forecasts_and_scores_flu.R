# Bundle the forecasts from targets and put it in the S3 bucket.
# This script assumes that you have used `make pull` to download the needed
# forecasts and data from S3. Make sure that your project is set appropriately
# in .Renviron, for instance: TAR_PROJECT=flu_hosp_tiny.
source(here::here("R", "load_all.R"))



# Get the truth data used for scoring
flu_2023_truth_data <- tar_read(hhs_evaluation_data) %>%
  # This is a correction for the fact that we date the forecasts for the
  # Wednesday they were made, but FluSight dates the for the upcoming Saturday
  mutate(target_end_date = target_end_date + 3) %>%
  # The data has unserializable ALTREP objects that bork on S3 load without
  # this.
  as.data.table() %>%
  as_tibble()
s3save(flu_2023_truth_data, object = "flu_2023_truth_data.rds", bucket = "forecasting-team-data")


# Get the local forecasts
state_geo_values <- flu_2023_truth_data %>%
  pull(geo_value) %>%
  unique()
df <- tar_manifest()
forecasts <- df$name %>%
  keep(~ stringr::str_detect(., "forecast_")) %>%
  map(function(name) {
    tryCatch(
      {
        tibble(forecaster = str_remove(name, "forecast_"), tar_read_raw(name))
      },
      error = function(e) {
        print(name)
        print(e)
        tibble()
      }
    )
  }) %>%
  bind_rows() %>%
  filter(geo_value %in% state_geo_values)
cmu_flu_2023_forecasts <- forecasts %>%
  filter(source == "nhsn") %>%
  left_join(
    flu_2023_truth_data %>%
      select(geo_value, population) %>%
      distinct(),
    by = "geo_value"
  ) %>%
  mutate(prediction = prediction * population / 10L**5) %>%
  select(-source, -population) %>%
  # This is a correction for the fact that we date the forecasts for the
  # Wednesday they were made, but FluSight dates the for the upcoming Saturday
  mutate(forecast_date = forecast_date + 3, target_end_date = target_end_date + 3)


# Get the forecasts from FluSight 2023
s3load("flusight_forecasts_2023.rds", bucket = "forecasting-team-data")
cmu_forecast_dates <- cmu_flu_2023_forecasts %>%
  pull(forecast_date) %>%
  unique()

# Join and score
flu_2023_joined_forecasts <- bind_rows(
  cmu_flu_2023_forecasts, flusight_forecasts_2023 %>% filter(forecast_date %in% cmu_forecast_dates)
)
s3save(flu_2023_joined_forecasts, object = "flu_2023_joined_forecasts.rds", bucket = "forecasting-team-data")

flu_2023_joined_scores <- flu_2023_joined_forecasts %>%
  left_join(
    flu_2023_truth_data,
    by = c("geo_value", "target_end_date")
  ) %>%
  as.data.table() %>%
  rename(model = forecaster) %>%
  scoringutils::score(metrics = c("interval_score", "ae_median", "coverage")) %>%
  scoringutils::add_coverage(by = c("model", "geo_value", "forecast_date", "target_end_date"), ranges = c(80)) %>%
  scoringutils::summarize_scores(by = c("model", "geo_value", "forecast_date", "target_end_date")) %>%
  as_tibble() %>%
  select(
    forecaster = model,
    geo_value,
    forecast_date,
    target_end_date,
    wis = interval_score,
    ae = ae_median,
    coverage_80
  ) %>%
  mutate(ahead = as.numeric(target_end_date - forecast_date))
s3save(flu_2023_joined_scores, object = "flu_2023_joined_scores.rds", bucket = "forecasting-team-data")
