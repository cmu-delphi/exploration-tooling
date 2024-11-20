source(here::here("R", "load_all.R"))

# Works correctly if you have exactly one version where the previous Friday data
# is the latest so it is ignored and the week before THAT is summed (10-27 to
# 11-02), but is then dated to the Wednesday of the week (10-30).
tribble(
  ~geo_value, ~time_value, ~version, ~value,
  "us", "2024-11-08", "2024-11-13", 1,
  "us", "2024-11-07", "2024-11-13", 2,
  "us", "2024-11-06", "2024-11-13", 3,
  "us", "2024-11-05", "2024-11-13", 4,
  "us", "2024-11-04", "2024-11-13", 5,
  "us", "2024-11-03", "2024-11-13", 6,
  "us", "2024-11-02", "2024-11-13", 7,
  "us", "2024-11-01", "2024-11-13", 8,
  "us", "2024-10-31", "2024-11-13", 9,
  "us", "2024-10-30", "2024-11-13", 10,
  "us", "2024-10-29", "2024-11-13", 11,
  "us", "2024-10-28", "2024-11-13", 12,
  "us", "2024-10-27", "2024-11-13", 13
) %>%
  mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
  as_epi_archive(versions_end = as.Date("2024-11-13"), compactify = TRUE) %>%
  daily_to_weekly_archive("value")

# Same thing as above, but the versions go backwards, constant lag of 5 days
tribble(
  ~geo_value, ~time_value, ~version, ~value,
  "us", "2024-11-08", "2024-11-13", 1,
  "us", "2024-11-07", "2024-11-12", 2,
  "us", "2024-11-06", "2024-11-11", 3,
  "us", "2024-11-05", "2024-11-10", 4,
  "us", "2024-11-04", "2024-11-09", 5,
  "us", "2024-11-03", "2024-11-08", 6,
  "us", "2024-11-02", "2024-11-07", 7,
  "us", "2024-11-01", "2024-11-06", 8,
  "us", "2024-10-31", "2024-11-05", 9,
  "us", "2024-10-30", "2024-11-04", 10,
  "us", "2024-10-29", "2024-11-03", 11,
  "us", "2024-10-28", "2024-11-02", 12,
  "us", "2024-10-27", "2024-11-01", 13
) %>%
  mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
  as_epi_archive(versions_end = as.Date("2024-11-13"), compactify = TRUE) %>%
  daily_to_weekly_archive("value")

# Now add the Friday
# Same thing as above, but the versions go backwards, constant lag of 5 days
tribble(
  ~geo_value, ~time_value, ~version, ~value,
  "us", "2024-11-09", "2024-11-13", 0.5,
  "us", "2024-11-08", "2024-11-13", 1,
  "us", "2024-11-07", "2024-11-12", 2,
  "us", "2024-11-06", "2024-11-11", 3,
  "us", "2024-11-05", "2024-11-10", 4,
  "us", "2024-11-04", "2024-11-09", 5,
  "us", "2024-11-03", "2024-11-08", 6,
  "us", "2024-11-02", "2024-11-07", 7,
  "us", "2024-11-01", "2024-11-06", 8,
  "us", "2024-10-31", "2024-11-05", 9,
  "us", "2024-10-30", "2024-11-04", 10,
  "us", "2024-10-29", "2024-11-03", 11,
  "us", "2024-10-28", "2024-11-02", 12,
  "us", "2024-10-27", "2024-11-01", 13
) %>%
  mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
  as_epi_archive(versions_end = as.Date("2024-11-13"), compactify = TRUE) %>%
  daily_to_weekly_archive("value")

# NA
tribble(
  ~geo_value, ~time_value, ~version, ~value,
  "us", "2024-11-16", "2024-11-16", 1,
  "us", "2024-11-15", "2024-11-16", 2,
  "us", "2024-11-09", "2024-11-09", 3,
  "us", "2024-11-08", "2024-11-09", 4,
  "us", "2024-11-02", "2024-11-02", 5,
  "us", "2024-11-01", "2024-11-02", 6,
) %>%
  mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
  as_epi_archive(.versions_end = as.Date("2024-11-16"), compactify = TRUE) %>%
  daily_to_weekly_archive("value")
