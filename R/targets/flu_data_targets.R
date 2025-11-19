#' Flu data targets
#'
#' This file contains functions to create targets for flu data.

#' Create data targets for flu forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' @return A list of targets for data
#' @export
create_flu_data_targets <- function() {
  # TODO: Share code with covid?
  rlang::list2(
    tar_target(
      name = hhs_archive_data_asof,
      command = {
        get_health_data(as.Date(forecast_dates), disease = "flu") %>%
          mutate(version = as.Date(forecast_dates)) %>%
          relocate(geo_value, time_value, version, hhs)
      },
      pattern = map(forecast_dates)
    ),
    # TODO: Share code with covid?
    tar_target(
      name = hhs_archive,
      command = {
        hhs_archive <- hhs_archive_data_asof %>%
          as_epi_archive(compactify = TRUE) %>%
          daily_to_weekly_archive(agg_columns = "hhs")
        hhs_archive$DT %>%
          add_season_info() %>%
          mutate(
            agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse("us" == geo_value, "nation", "state"))
          ) %>%
          add_pop_and_density() %>%
          mutate(hhs = hhs / population * 10L^5) %>%
          mutate(source = "nhsn") %>%
          as_epi_archive(other_keys = "source", compactify = TRUE) %>%
          extract2("DT") %>%
          select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
      }
    ),
    tar_target(
      name = flusurv,
      command = {
        flusurv_adjusted <- generate_flusurv_adjusted()
        flusurv_adjusted$DT %>%
          mutate(
            time_value = time_value + g_time_value_adjust,
            version = version + g_time_value_adjust
          ) %>%
          mutate(source = "flusurv") %>%
          select(geo_value, time_value, version, hhs = adj_hosp_rate, source, agg_level, season, season_week)
      }
    ),
    tar_target(
      name = ili_plus,
      command = {
        ili_plus <- gen_ili_data()
        ili_plus <- ili_plus$DT %>%
          drop_na() %>%
          filter(hhs > 0.0001) %>%
          mutate(
            time_value = time_value + g_time_value_adjust,
            version = time_value
          ) %>%
          select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
        to_keep <-
          ili_plus %>%
          group_by(geo_value, season) %>%
          summarize(total_count = length(hhs), .groups = "drop") %>%
          filter(total_count >= 20) %>%
          select(geo_value, season)
        to_keep %>%
          left_join(ili_plus, by = join_by(geo_value, season), relationship = "many-to-many") %>%
          as_epi_archive(other_keys = "source", compactify = TRUE) %>%
          extract2("DT")
      }
    ),
    tar_target(
      name = flusion_data_archive,
      command = {
        flusion_data_archive <- bind_rows(ili_plus, flusurv, hhs_archive) %>%
          add_pop_and_density() %>%
          as_epi_archive(compactify = TRUE, other_keys = "source")
        flusion_data_archive <- flusion_data_archive$DT %>%
          filter(!is.na(hhs), time_value <= max(forecast_dates)) %>%
          relocate(
            source,
            geo_value,
            time_value,
            version,
            hhs,
            agg_level,
            season,
            season_week,
            year,
            population,
            density
          ) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        flusion_data_archive$geo_type <- "custom"
        flusion_data_archive
      }
    ),
    # TODO: Share code with covid?
    tar_target(
      name = nssp_archive,
      command = {
        nssp_state <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "nssp",
          signals = "pct_ed_visits_influenza",
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
          signals = "pct_ed_visits_influenza",
          time_type = "week",
          geo_type = "hhs",
          geo_values = "*",
          fetch_args = g_fetch_args
        )
        nssp <- nssp_state %>%
          select(geo_value, time_value, issue, nssp = value) %>%
          append_us_aggregate("nssp", group_keys = c("time_value", "issue")) %>%
          bind_rows(nssp_hhs) %>%
          as_epi_archive(compactify = TRUE) %>%
          extract2("DT") %>%
          # weekly data is indexed from the start of the week
          mutate(time_value = time_value + 6 - g_time_value_adjust) %>%
          # Artifically add in a one-week latency.
          mutate(version = time_value + 7) %>%
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        nssp$geo_type <- "custom"
        nssp
      }
    ),
    # TODO: Share code with covid?
    tar_target(
      name = google_symptoms_archive,
      command = {
        used_searches <- c(1, 3, 4)
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
            append_us_aggregate("value") %>%
            bind_rows(google_symptoms_hhs_archive) %>%
            select(geo_value, time_value, value) %>%
            daily_to_weekly() %>%
            mutate(version = time_value) %>%
            mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
            unnest(cols = "source") %>%
            filter(!is.na(value)) %>%
            relocate(source, geo_value, time_value, version, value) %>%
            # Always convert to data.frame after dplyr operations on data.table.
            # https://github.com/cmu-delphi/epiprocess/issues/618
            as.data.frame() %>%
            as_epi_archive(other_keys = "source", compactify = TRUE)
        })
        all_of_them[[1]]$DT %<>% rename(google_symptoms_1_cough = value)
        all_of_them[[2]]$DT %<>% rename(google_symptoms_3_fever = value)
        all_of_them[[3]]$DT %<>% rename(google_symptoms_4_bronchitis = value)
        google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]]) %>% epix_merge(all_of_them[[3]])
        pre_pipeline <- google_symptoms_archive %>% epix_as_of(as.Date("2023-10-04"))
        colnames <- c("google_symptoms_1_cough", "google_symptoms_3_fever", "google_symptoms_4_bronchitis")
        for (colname in colnames) {
          learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
          google_symptoms_archive$DT %<>%
            data_whitening(colname = colname, learned_params, join_cols = c("geo_value", "source"))
        }
        google_symptoms_archive <- google_symptoms_archive$DT %>%
          mutate(
            google_symptoms = ifelse(is.na(google_symptoms_1_cough), 0, google_symptoms_1_cough) +
              ifelse(is.na(google_symptoms_3_fever), 0, google_symptoms_3_fever) +
              ifelse(is.na(google_symptoms_4_bronchitis), 0, google_symptoms_4_bronchitis)
          ) %>%
          # Always convert to data.frame after dplyr operations on data.table
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        google_symptoms_archive$geo_type <- "custom"
        google_symptoms_archive
      }
    ),
    # TODO: Share code with covid?
    # TODO: Geo code share?
    tar_target(
      name = nwss_coarse,
      command = {
        nwss <- get_nwss_coarse_data("flu") %>%
          arrange(geo_value, time_value) %>%
          add_pop_and_density() %>%
          drop_na() %>%
          select(-agg_level, -year, -population, -density)
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
        nwss %<>%
          mutate(agg_level = "state") %>%
          append_us_aggregate("value") %>%
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
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        nwss$geo_type <- "custom"
        nwss
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
      name = evaluation_data,
      command = {
        new_flu_data <- flusion_data_archive$DT %>%
          filter(
            source == "nhsn",
            agg_level %in% c("state", "nation"),
            time_value >= as.Date("2023-10-04")
          ) %>%
          drop_na() %>%
          mutate(hhs = hhs * population / 10**5) %>%
          as_epi_archive(compactify = TRUE)
        new_flu_data %>%
          epix_as_of(new_flu_data$versions_end) %>%
          rename(
            true_value = hhs,
            target_end_date = time_value,
            signal = source
          ) %>%
          select(
            signal,
            geo_value,
            target_end_date,
            true_value,
            population
          ) %>%
          # Push Wednesday labels to Saturday.
          mutate(target_end_date = target_end_date + g_time_value_adjust)
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
          select(geo_value, time_value, va_flu_per_100k = unique_patients_flu_per_100k) %>%
          # Convert to weekly
          daily_to_weekly(values = "va_flu_per_100k") %>%
          # Add version and source columns to make it a faux-archive
          mutate(time_value = time_value, version = time_value) %>%
          # Add source columns, to make it mergeable with flusion_data_archive
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          # Make it an archive
          as_epi_archive(other_keys = "source", compactify = TRUE)
        varch$geo_type <- "custom"
        varch
      }
    ),
    tar_target(
      name = state_geo_values,
      command = {
        evaluation_data %>%
          pull(geo_value) %>%
          unique()
      }
    ),
    tar_target(
      name = joined_archive_data,
      command = {
        joined_archive_data <- flusion_data_archive %>%
          epix_merge(google_symptoms_archive, sync = "locf") %>%
          epix_merge(nwss_coarse, sync = "locf") %>%
          epix_merge(nssp_archive, sync = "locf") %>%
          epix_merge(veteran_state_archive, sync = "locf") %>%
          extract2("DT") %>%
          drop_na("hhs") %>%
          filter(geo_value %nin% g_insufficient_data_geos) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)

        # Jank adding of hhs_region column to the data
        joined_archive_data <- joined_archive_data$DT %>%
          add_hhs_region_sum(hhs_region) %>%
          as_epi_archive(other_keys = "source")

        # TODO: This is a hack to ensure the as_of data is cached. Maybe there's a better way.
        epix_slide_simple(joined_archive_data, dummy_forecaster, forecast_dates, cache_key = "joined_archive_data")
        joined_archive_data
      }
    )
  )
}
