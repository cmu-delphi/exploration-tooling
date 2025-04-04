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
    tar_target(
      name = hhs_archive_data_asof,
      command = {
        get_health_data(as.Date(forecast_dates), disease = "covid") %>%
          mutate(version = as.Date(forecast_dates)) %>%
          relocate(geo_value, time_value, version, hhs)
      },
      pattern = map(forecast_dates)
    ),
    tar_target(
      name = hhs_archive,
      command = {
        hhs_archive <- hhs_archive_data_asof %>%
          rename(value = hhs) %>%
          as_epi_archive(compactify = TRUE) %>%
          daily_to_weekly_archive(agg_columns = "value")
        hhs_archive$geo_type <- "state"
        hhs_archive
      }
    ),
    tar_target(
      name = hhs_evaluation_data,
      command = {
        retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "hhs",
          signals = g_hhs_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = "*",
          fetch_args = g_fetch_args
        ) %>%
          select(signal, geo_value, time_value, value) %>%
          # This aggregates the data to the week and labels each Sunday - Saturday
          # summation with the Wednesday of that week.
          daily_to_weekly(keys = c("geo_value", "signal")) %>%
          select(signal, geo_value, target_end_date = time_value, true_value = value) %>%
          # Correction for timing offsets
          mutate(target_end_date = target_end_date + g_time_value_adjust)
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
          # Append national aggregate
          append_us_aggregate() %>%
          # Convert to per 100k
          mutate(across(starts_with("unique_patients"), ~ . * 1e5 / veteran_population)) %>%
          # Rename columns
          rename_with(~ paste0(., "_per_100k"), starts_with("unique_patients")) %>%
          # Filter to just flu
          select(geo_value, time_value, va_covid_per_100k = unique_patients_covid_per_100k) %>%
          # Convert to weekly
          daily_to_weekly(values = "va_covid_per_100k") %>%
          # Add version and source columns to make it a faux-archive
          mutate(time_value = time_value, version = time_value) %>%
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
        # reformt hhs_archive, remove data spotty locations
        joined_archive_data <- hhs_archive$DT %>%
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
