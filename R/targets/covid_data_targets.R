#' COVID data targets
#'
#' This file contains functions to create targets for COVID data.

#' Create data targets for COVID forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' @return A list of targets for data
#' @export
create_covid_data_targets <- function() {
  rlang::list2(
    tar_change(
      name = nhsn_archive,
      change = get_s3_object_last_modified("nhsn_data_archive.parquet", "forecasting-team-data"),
      command = {
        # NHSN hospitalization counts, the same source covid prod uses, replacing
        # the discontinued healthdata.gov g62h-syeh pull (get_health_data), which
        # froze in 2024 and no longer covers the forecast season. This carries
        # NHSN's real revision history rather than a synthetic version-per-date.
        # NHSN weeks end Saturday; shift to the Wednesday label the rest of this
        # pipeline uses internally (g_time_value_adjust pushes back to Saturday at
        # scoring/output).
        nhsn_archive <- get_nhsn_data_archive("covid")$DT %>%
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
        # Oracle = the finalized NHSN hospitalizations (counts) straight from the
        # joined archive, matching what the forecasters target. The old HHS
        # covidcast signal (confirmed_admissions_covid_1d) was discontinued when
        # the reporting mandate lapsed (~2024-04-27), so it no longer covers the
        # forecast season; flu already sources its truth from NHSN the same way.
        joined_archive_data %>%
          epix_as_of(joined_archive_data$versions_end) %>%
          transmute(
            signal = "nhsn",
            geo_value,
            # Push the Wednesday week label to Saturday, as the old truth did.
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
        nssp_state <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "nssp",
          signals = "pct_ed_visits_covid",
          time_type = "week",
          geo_type = "state",
          geo_values = "*",
          fetch_args = g_fetch_args
        )
        nssp_hhs <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "nssp",
          signals = "pct_ed_visits_covid",
          time_type = "week",
          geo_type = "hhs",
          geo_values = "*",
          fetch_args = g_fetch_args
        )
        nssp_state %>%
          bind_rows(nssp_hhs) %>%
          select(geo_value, time_value, issue, nssp = value) %>%
          as_epi_archive(compactify = TRUE) %>%
          extract2("DT") %>%
          # weekly data is indexed from the start of the week
          mutate(time_value = time_value + 6 - g_time_value_adjust) %>%
          group_by(.data$geo_value, .data$time_value) %>%
          slice_max(.data$version, n = 1L, with_ties = FALSE) %>%
          ungroup() %>%
          # Artifically add in a one-week latency.
          mutate(version = time_value + 7) %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
      }
    ),
    # see git history for the google symptoms target
    tar_target(
      name = nwss_coarse,
      command = {
        nwss <- get_nwss_coarse_data("covid") %>%
          rename(value = state_med_conc) %>%
          arrange(geo_value, time_value) %>%
          add_pop_and_density() %>%
          drop_na() %>%
          select(-agg_level, -year, -agg_level, -population, -density)
        pop_data <- gen_pop_and_density_data()
        cw <- readr::read_csv(
          "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv",
          show_col_types = FALSE,
          progress = FALSE
        ) %>%
          left_join(
            readr::read_csv(
              "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv",
              show_col_types = FALSE,
              progress = FALSE
            ),
            by = join_by(state_code == state_code)
          ) %>%
          mutate(hhs = as.character(hhs)) %>%
          select(geo_value = state_id, hhs_region = hhs)
        nwss_hhs_region <- nwss %>%
          left_join(cw, by = "geo_value") %>%
          mutate(year = year(time_value)) %>%
          left_join(pop_data, by = join_by(geo_value, year)) %>%
          select(-year, density) %>%
          group_by(time_value, hhs_region) %>%
          summarize(
            value = sum(value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            activity_level = sum(activity_level * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            region_value = mean(region_value * population) / sum(population, na.rm = TRUE),
            national_value = sum(national_value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(agg_level = "hhs_region", hhs_region = as.character(hhs_region)) %>%
          rename(geo_value = hhs_region)
        nwss %>%
          mutate(agg_level = "state") %>%
          bind_rows(nwss_hhs_region) %>%
          select(
            geo_value,
            time_value,
            nwss = value,
            nwss_region = region_value,
            nwss_national = national_value
          ) %>%
          mutate(time_value = time_value - g_time_value_adjust, version = time_value) %>%
          arrange(geo_value, time_value) %>%
          as_epi_archive(compactify = TRUE)
      }
    ),
    tar_target(
      name = va_respiratory_archive,
      command = {
        fetch_va <- function(geo_type, geo_values = "*") {
          retry_fn(
            max_attempts = 10,
            wait_seconds = 1,
            fn = epidatr::epidata_archive,
            source = "va_respiratory",
            signals = "covid_cases_per_100k_7dav",
            geo_type = geo_type,
            geo_values = geo_values,
            fetch_args = g_fetch_args
          ) %>%
            select(geo_value, time_value = reference_time, version = report_time, va_covid_per_100k = value) %>%
            mutate(across(c(time_value, version), as.Date))
        }
        va <- bind_rows(
          fetch_va("state"),
          # National fetched directly so us geo has the correct value, not a
          # sum of all state values.
          fetch_va("nation", "us")
        ) %>%
          # Convert daily reference_time to weekly, keeping each report_time
          # version distinct so real revisions are preserved. The source signal
          # is already a 7-day trailing average, so aggregate with "mean" (not
          # the default "sum", which would inflate an already-smoothed rate
          # ~7x by summing near-identical daily values).
          daily_to_weekly(agg_method = "mean", keys = c("geo_value", "version"), values = "va_covid_per_100k") %>%
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
        va$geo_type <- "custom"
        va
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
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
        result$geo_type <- "custom"
        result
      }
    ),
    tar_target(
      name = joined_archive_data,
      command = {
        # reformat the nhsn archive, remove data spotty locations
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
        joined_archive_data <- joined_archive_data %>% epix_merge(nwss_coarse, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        joined_archive_data %<>% epix_merge(nssp_archive, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        # see git history for google_symptoms
        joined_archive_data %<>% epix_merge(va_respiratory_archive, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        joined_archive_data %<>% epix_merge(beds_archive, sync = "locf")
        joined_archive_data <- joined_archive_data$DT %>%
          filter(grepl("[a-z]{2}", geo_value), !(geo_value %in% g_insufficient_data_geos)) %>%
          # `signal` is leftover epidatr metadata from the nssp/va_respiratory
          # merges, not a key. It is a reserved epiprocess name, so leaving it in makes
          # epix_as_of treat the archive as long-format and pivot every wide column
          # into junk -- breaking every snapshot forecaster.
          select(-any_of("signal")) %>%
          # Always convert to data.frame after dplyr operations on data.table
          # https://github.com/cmu-delphi/epiprocess/issues/618
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
        # TODO: This can be a bit more granular (per geo, per source, etc.)
        min_time_value <- joined_archive_data$DT %>%
          filter(if_all(all_of(c("hhs")), ~ !is.na(.))) %>%
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
