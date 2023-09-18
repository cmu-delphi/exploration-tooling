library(epidatr)
library(epiprocess)
archive_filename <- file.path(here::here(), "basic-epipredict-forecaster", "dataArchive", "archive.rds")
earliest_issue_date <- as.Date("2020-11-16")
prev_monday <- function(date) date - as.numeric(date - 1 + 4) %% 7
reformat_data <- function(data) {
  if (length(data) == 0) {
    cleaned <- tibble(
      geo_value = character(),
      time_value = as.Date(character()),
      chng = numeric(),
      version = as.Date(character())
    ) %>% as_epi_archive(
      geo_type = "state",
      time_type = "day",
      clobberable_versions_start = NA,
      versions_end = as.Date("2001-01-01")
    )
    return(cleaned)
  } else {
    cleaned <- data %>%
      select(geo_value, time_value, value, issue) %>%
      rename("{source}" := value) %>%
      rename(version = issue) %>%
      as_epi_archive(compactify = TRUE)
    return(cleaned)
  }
}
# the old function, it's a bit overly specific
get_data <- function(target_date, version_start, version_end, source = c("hhs", "chng"), geo_type = "state", time_type = "day", timeout_seconds = 30) {
  if (source == "hhs") {
    signals <- "confirmed_admissions_covid_1d"
  } else {
    signals <- "smoothed_adj_outpatient_covid"
  }
  data <- epidatr::covidcast(
    data_source = source, signals = signals,
    geo_type = geo_type,
    time_type = time_type,
    time_values = epirange(from = "1980-05-08", to = target_date),
    geo_values = "*",
    issues = epirange(version_start, version_end),
    fetch_args = fetch_args_list(return_empty = TRUE, timeout_seconds = timeout_seconds)
  )

  if (length(data) == 0) {
    cleaned <- tibble(
      geo_value = character(),
      time_value = as.Date(character()),
      chng = numeric(),
      version = as.Date(character())
    ) %>% as_epi_archive(
      geo_type = "state",
      time_type = "day",
      clobberable_versions_start = NA,
      versions_end = as.Date("2001-01-01")
    )
    return(cleaned)
  } else {
    cleaned <- data %>%
      select(geo_value, time_value, value, issue) %>%
      rename("{source}" := value) %>%
      rename(version = issue) %>%
      as_epi_archive(compactify = TRUE)
    return(cleaned)
  }
}
