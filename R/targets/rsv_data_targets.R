#' RSV data targets
#'
#' This file contains functions to create targets for RSV data.

#' Create data targets for RSV forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' NHSN RSV admissions are raw weekly counts (like covid), not rates. Reporting
#' became mandatory in Nov 2024; the 2023-24 season is voluntary and partial,
#' and the archive has no versions before 2024-12-11.
#'
#' @return A list of targets for data
#' @export
create_rsv_data_targets <- function() {
  rlang::list2(
    tar_change(
      name = nhsn_archive,
      change = get_s3_object_last_modified("nhsn_data_archive.parquet", "forecasting-team-data"),
      command = {
        # NHSN weeks end Saturday; shift to the Wednesday label the rest of this
        # pipeline uses internally (g_time_value_adjust pushes back to Saturday
        # at scoring/output).
        nhsn_archive <- get_nhsn_data_archive("rsv")$DT %>%
          as.data.frame() %>%
          mutate(time_value = time_value - 3L) %>%
          as_epi_archive(compactify = TRUE)
        nhsn_archive$geo_type <- "state"
        nhsn_archive
      }
    ),
    tar_target(
      name = hhs_evaluation_data,
      command = {
        joined_archive_data %>%
          epix_as_of(joined_archive_data$versions_end) %>%
          transmute(
            signal = "nhsn",
            geo_value,
            target_end_date = time_value + g_time_value_adjust,
            true_value = hhs
          ) %>%
          drop_na(true_value)
      }
    ),
    tar_target(
      name = state_geo_values,
      command = {
        hhs_evaluation_data %>%
          pull(geo_value) %>%
          unique()
      }
    ),
    tar_target(
      name = nssp_archive,
      command = {
        # States only: the joined archive drops the national NHSN row.
        nssp <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = epidatr::epidata_archive,
          source = "nssp",
          signals = "pct_ed_visits_rsv",
          geo_type = "state",
          geo_values = "*",
          fetch_args = g_fetch_args
        ) %>%
          select(geo_value, time_value = reference_time, version = report_time, nssp = value) %>%
          mutate(across(c(time_value, version), as.Date)) %>%
          # reference_time is the last day of the epi week (Saturday); shift to
          # the Wednesday label matching NHSN.
          mutate(time_value = time_value - g_time_value_adjust) %>%
          as_epi_archive(compactify = TRUE)
        nssp$geo_type <- "custom"
        nssp
      }
    ),
    tar_target(
      name = hhs_region,
      command = {
        hhs_region <- readr::read_csv(
          "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv"
        )
        state_id <- readr::read_csv(
          "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv"
        )
        hhs_region %>%
          left_join(state_id, by = "state_code") %>%
          select(hhs_region = hhs, geo_value = state_id) %>%
          mutate(hhs_region = as.character(hhs_region))
      }
    ),
    tar_change(
      name = beds_archive,
      change = get_s3_object_last_modified("nhsn_data_archive.parquet", "forecasting-team-data"),
      command = {
        result <- get_nhsn_beds_archive()$DT %>%
          as.data.frame() %>%
          mutate(
            geo_value = ifelse(geo_value == "usa", "us", geo_value),
            time_value = time_value - 3L
          ) %>%
          as_epi_archive(compactify = TRUE)
        result$geo_type <- "custom"
        result
      }
    ),
    tar_target(
      name = joined_archive_data,
      command = {
        joined_archive_data <- nhsn_archive$DT %>%
          select(geo_value, time_value, value, version) %>%
          rename("hhs" := value) %>%
          add_hhs_region_sum(hhs_region) %>%
          filter(geo_value != "us") %>%
          # Always convert to data.frame after dplyr operations on data.table
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
        joined_archive_data$geo_type <- "custom"
        joined_archive_data %<>% epix_merge(nssp_archive, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        joined_archive_data %<>% epix_merge(beds_archive, sync = "locf")
        joined_archive_data <- joined_archive_data$DT %>%
          filter(grepl("[a-z]{2}", geo_value), !(geo_value %in% g_insufficient_data_geos)) %>%
          # `signal` is a reserved epiprocess name; if epidatr metadata leaks it
          # in, epix_as_of treats the archive as long-format.
          select(-any_of("signal")) %>%
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
        joined_archive_data$geo_type <- "state"
        # TODO: This is a hack to ensure the as_of data is cached. Maybe there's a better way.
        epix_slide_simple(joined_archive_data, dummy_forecaster, forecast_dates, cache_key = "joined_archive_data")
        joined_archive_data
      }
    ),
    tar_target(
      validate_joined_archive_data,
      command = {
        min_time_value <- joined_archive_data$DT %>%
          filter(!is.na(hhs)) %>%
          distinct(time_value) %>%
          pull(time_value) %>%
          min()
        if (min_time_value > (forecast_dates[1] - 30)) {
          stop(
            "Joined archive data does not have at least 30 days of training data for the earliest forecast date.
               Update your forecast_dates to be later than ",
            min_time_value + 30,
            "."
          )
        }
      }
    )
  )
}
