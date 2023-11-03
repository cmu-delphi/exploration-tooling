geo_type <- "state"
time_type <- "day"
geo_values <- "*"
time_values <- epidatr::epirange(from = "2022-01-01", to = "2024-01-01")
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 200)
issues <- "*"

data_targets <- list(
  tar_target(
    name = hhs_latest_data,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_influenza_1d_prop_7dav",
        geo_type = geo_type,
        time_type = time_type,
        geo_values = geo_values,
        time_values = time_values,
        fetch_args = fetch_args
      )
    }
  ),
  tar_target(
    name = chng_latest_data,
    command = {
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_flu",
        geo_type = geo_type,
        time_type = time_type,
        geo_values = geo_values,
        time_values = time_values,
        fetch_args = fetch_args
      )
    }
  ),
  tar_target(
    name = hhs_evaluation_data,
    command = {
      hhs_latest_data %>%
        rename(
          actual = value,
          target_end_date = time_value
        )
    }
  ),
  tar_target(
    name = hhs_latest_data_2022,
    command = {
      hhs_latest_data %>%
        filter(time_value >= "2022-01-01")
    }
  ),
  tar_target(
    name = chng_latest_data_2022,
    command = {
      chng_latest_data %>%
        filter(time_value >= "2022-01-01")
    }
  ),
  tar_target(
    name = hhs_archive_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_influenza_1d_prop_7dav",
        geo_type = geo_type,
        time_type = time_type,
        geo_values = geo_values,
        time_values = time_values,
        issues = issues,
        fetch_args = fetch_args
      )
    }
  ),
  tar_target(
    name = chng_archive_data_2022,
    command = {
      # TODO: Filter out unused columns like missing, direction, etc.
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_flu",
        geo_type = geo_type,
        time_type = time_type,
        geo_values = geo_values,
        time_values = time_values,
        issues = issues,
        fetch_args = fetch_args
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
          geo_type = geo_type,
          time_type = time_type,
          compactify = TRUE
        )
      chng_archive_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("chng" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = geo_type,
          time_type = time_type,
          compactify = TRUE
        )
      epix_merge(hhs_archive_data_2022, chng_archive_data_2022, sync = "locf")$DT %>%
        filter(!is.na(hhs) & !is.na(chng)) %>%
        filter(!geo_value %in% c("as", "pr", "vi", "gu", "mp")) %>%
        epiprocess::as_epi_archive()
    }
  )
)
