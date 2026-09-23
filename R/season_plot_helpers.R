Mean <- function(x) mean(x, na.rm = TRUE)
GeoMean <- function(x, offset = 0) exp(Mean(log(x + offset)))

compute_peak_season <- function(data_current, threshold = 0.5, start_of_year = as.Date("2024-11-01")) {
  season_length <- data_current %>% pull(time_value) %>% max() - start_of_year
  data_current %>%
    filter(time_value > start_of_year) %>%
    group_by(geo_value) %>%
    mutate(max_val = max(value)) %>%
    filter(value >= threshold * max_val) %>%
    summarize(first_above = min(time_value), last_above = max(time_value)) %>%
    mutate(
      duration = last_above - first_above,
      rel_duration = as.integer(duration) / as.integer(season_length)
    )
}

classify_phase <- function(time_value, first_above, last_above, rel_duration, threshold) {
  case_when(
    rel_duration > threshold ~ "flat",
    time_value < first_above ~ "increasing",
    time_value > last_above ~ "decreasing",
    .default = "peak"
  ) %>% factor(levels = c("increasing", "peak", "decreasing", "flat"))
}
