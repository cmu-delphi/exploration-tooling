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
          # Artifically add in a one-week latency.
          mutate(version = time_value + 7) %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
      }
    ),
    tar_target(
      name = google_symptoms_archive,
      command = {
        used_searches <- c(4, 5)
        # not using actual versions here because the only revision behavior is the
        # source going down completely, which means we're actually just comparing
        # with the version without this source
        all_of_them <- lapply(used_searches, \(search_name) {
          google_symptoms_state_archive <- retry_fn(
            max_attempts = 10,
            wait_seconds = 1,
            fn = pub_covidcast,
            source = "google-symptoms",
            signals = glue::glue("s0{search_name}_smoothed_search"),
            time_type = "day",
            geo_type = "state",
            geo_values = "*",
            fetch_args = g_fetch_args
          )
          google_symptoms_hhs_archive <- retry_fn(
            max_attempts = 10,
            wait_seconds = 1,
            fn = pub_covidcast,
            source = "google-symptoms",
            signals = glue::glue("s0{search_name}_smoothed_search"),
            time_type = "day",
            geo_type = "hhs",
            geo_values = "*",
            fetch_args = g_fetch_args
          )
          google_symptoms_archive_min <- google_symptoms_state_archive %>%
            bind_rows(google_symptoms_hhs_archive) %>%
            select(geo_value, time_value, value) %>%
            daily_to_weekly() %>%
            mutate(version = time_value) %>%
            filter(!is.na(value)) %>%
            relocate(geo_value, time_value, version, value) %>%
            # Always convert to data.frame after dplyr operations on data.table.
            # https://github.com/cmu-delphi/epiprocess/issues/618
            as.data.frame() %>%
            as_epi_archive(compactify = TRUE)
        })
        all_of_them[[1]] <- all_of_them[[1]]$DT %>%
          rename(google_symptoms_4_bronchitis = value) %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
        all_of_them[[2]] <- all_of_them[[2]]$DT %>%
          rename(google_symptoms_5_ageusia = value) %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
        google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]])
        pre_pipeline <- google_symptoms_archive %>%
          epix_as_of(as.Date("2023-10-04")) %>%
          mutate(source = "none")
        # Google Symptoms has two signals that have different and unknown scales,
        # so we need to whiten them.
        colnames <- c("google_symptoms_4_bronchitis", "google_symptoms_5_ageusia")
        for (colname in colnames) {
          learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
          google_symptoms_archive$DT %<>% data_whitening(colname = colname, learned_params, join_cols = "geo_value")
        }
        # Sum the two signals.
        google_symptoms_archive$DT %>%
          mutate(
            google_symptoms = ifelse(is.na(google_symptoms_4_bronchitis), 0, google_symptoms_4_bronchitis) +
              ifelse(is.na(google_symptoms_5_ageusia), 0, google_symptoms_5_ageusia)
          ) %>%
          select(-starts_with("source")) %>%
          # Always convert to data.frame after dplyr operations on data.table
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
      }
    ),
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
      name = veteran_state_archive,
      command = {
        # These files were generated by scripts/va_explore/process_va_data.py
        # then uploaded to the bucket manually. That script depends on private
        # data not included in the repo.
        vdf <- s3read_using(read_csv, bucket = "forecasting-team-data", object = "va_explore/veteran_state_df.csv") %>%
          left_join(
            s3read_using(read_csv, bucket = "forecasting-team-data", object = "va_explore/veteran_pop.csv"),
            by = "state"
          )

        # Fill in missing values with national normalized values
        vdf <- vdf %>%
          inner_join(
            vdf %>%
              group_by(date) %>%
              summarize(
                flu_national_normalized = sum(unique_patients_flu, na.rm = TRUE) /
                  sum(veteran_population, na.rm = TRUE),
                covid_national_normalized = sum(unique_patients_covid, na.rm = TRUE) /
                  sum(veteran_population, na.rm = TRUE)
              ),
            by = "date"
          ) %>%
          mutate(
            unique_patients_flu = ifelse(
              near(unique_patients_flu, 0),
              flu_national_normalized * veteran_population,
              unique_patients_flu
            ),
            unique_patients_covid = ifelse(
              near(unique_patients_covid, 0),
              covid_national_normalized * veteran_population,
              unique_patients_covid
            )
          )

        # Rename schema
        varch <- vdf %>%
          rename(geo_value = state, time_value = date) %>%
          # Convert to per 100k
          mutate(across(starts_with("unique_patients"), ~ . * 1e5 / veteran_population)) %>%
          # Rename columns
          rename_with(~ paste0(., "_per_100k"), starts_with("unique_patients")) %>%
          # Filter to just covid
          select(geo_value, time_value, va_covid_per_100k = unique_patients_covid_per_100k) %>%
          # Convert to weekly
          daily_to_weekly(values = "va_covid_per_100k") %>%
          # Add version and source columns to make it a faux-archive
          mutate(time_value = time_value, version = time_value) %>%
          # Filter to when data is decent quality
          filter(time_value >= "2020-01-01") %>%
          # Make it an archive
          as_epi_archive(compactify = TRUE)
        varch$geo_type <- "custom"
        varch
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
        joined_archive_data %<>% epix_merge(google_symptoms_archive, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        joined_archive_data %<>% epix_merge(veteran_state_archive, sync = "locf")
        joined_archive_data <- joined_archive_data$DT %>%
          filter(grepl("[a-z]{2}", geo_value), !(geo_value %in% g_insufficient_data_geos)) %>%
          # `signal` is leftover epidatr metadata from the nssp/veteran merges, not
          # a key. It is a reserved epiprocess name, so leaving it in makes
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
