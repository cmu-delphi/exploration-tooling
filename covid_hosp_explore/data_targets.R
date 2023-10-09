data <- list(
  tar_target(
    name = hhs_evaluation_data,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "2020-01-01", to = "2024-01-01"),
      ) %>%
        rename(
          actual = value,
          target_end_date = time_value
        )
    }
  ),
  tar_target(
    name = hhs_archive_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        issues = "*",
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = chng_archive_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_covid",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        issues = "*",
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = joined_archive_data_2022,
    command = {
      hhs_archive_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("hhs" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      chng_archive_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("chng" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      epix_merge(hhs_archive_data_2022, chng_archive_data_2022, sync = "locf")
    }
  ),
  tar_target(
    name = hhs_latest_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = chng_latest_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_covid",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  )
)
