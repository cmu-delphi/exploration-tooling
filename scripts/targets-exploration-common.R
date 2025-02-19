#' Make common targets for fetching data
#'
#' Relies on the following globals:
#' - `hhs_signal`
#' - `chng_signal`
#' - `fetch_args`
#' - `eval_time`
#' - `training_time`
make_data_targets <- function() {
  list2(
    tar_target(
      name = hhs_latest_data,
      command = {
        retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "hhs",
          signals = hhs_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = eval_time,
          fetch_args = fetch_args
        )
      }
    ),
    tar_target(
      name = chng_latest_data,
      command = {
        retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "chng",
          signals = chng_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = eval_time,
          fetch_args = fetch_args
        )
      }
    ),
    tar_target(
      name = hhs_evaluation_data,
      command = {
        hhs_latest_data %>%
          rename(
            true_value = value,
            target_end_date = time_value
          ) %>%
          select(
            signal,
            geo_value,
            target_end_date,
            true_value
          )
      }
    ),
    tar_target(
      name = hhs_latest_data_2022,
      command = {
        hhs_latest_data # %>% filter(time_value >= "2022-01-01", time_value < "2022-04-01")
      }
    ),
    tar_target(
      name = chng_latest_data_2022,
      command = {
        chng_latest_data # %>% filter(time_value >= "2022-01-01", time_value < "2022-04-01")
      }
    ),
    tar_target(
      name = hhs_archive_data,
      command = {
        retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "hhs",
          signals = hhs_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = training_time,
          issues = "*",
          fetch_args = fetch_args
        )
      }
    ),
    tar_target(
      name = chng_archive_data_2022,
      command = {
        start_time <- as.Date(training_time$from, format = "%Y%m%d")
        stop_time <- Sys.Date()
        half <- floor((stop_time - start_time) / 2)
        first_half <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "chng",
          signals = chng_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = training_time,
          issues = epidatr::epirange(from = start_time, to = start_time + half),
          fetch_args = fetch_args
        )
        second_half <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "chng",
          signals = chng_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = training_time,
          issues = epidatr::epirange(from = start_time + half + 1, to = stop_time),
          fetch_args = fetch_args
        )
        add_row(first_half, second_half)
      }
    ),
    tar_target(
      name = joined_archive_data,
      command = {
        hhs_archive_data %<>%
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
        epix_merge(hhs_archive_data, chng_archive_data, sync = "locf")$DT %>%
          filter(!geo_value %in% c("as", "pr", "vi", "gu", "mp")) %>%
          epiprocess::as_epi_archive()
      }
    )
  )
}

#' Relies on the following globals:
#' - `forecaster_grid`
#' - `date_step`
#' - `ref_time-values` (can be NULL)
#' - `start_date` (can be NULL)
#' - `end_date` (can be NULL)
#' Relies on the following targets:
#' - joined_archive_data: the target data, it needs the outcome column to be hhs
#' - hhs_evaluation_data: the true values of the target data
make_forecasts_and_scores <- function() {
  forecasts_and_scores <- tar_map(
    values = forecaster_grid,
    names = id,
    unlist = FALSE,
    tar_target(
      name = forecast,
      command = {
        slid <- slide_forecaster(
          epi_archive = joined_archive_data,
          outcome = "hhs",
          ahead = aheads,
          forecaster = forecaster,
          n_training_pad = 30L,
          forecaster_args = params,
          forecaster_args_names = param_names,
          ref_time_values = ref_time_values,
          start_date = start_date,
          end_date = end_date,
          date_range_step_size = date_step,
          cache_key = "joined_archive_data"
        ) %>%
          rename(prediction = value) %>%
          mutate(ahead = as.numeric(target_end_date - forecast_date)) %>%
          mutate(id = id)
        gc()
        return(slid)
      },
      pattern = map(aheads)
    ),
    tar_target(
      name = score,
      command = {
        # If the data has already been scaled, hhs needs to include the
        # population and undo scaling.
        if ("population" %in% colnames(hhs_evaluation_data)) {
          actual_eval_data <- hhs_evaluation_data %>% select(-population)
          forecast_scaled <- forecast %>%
            left_join(
              hhs_evaluation_data %>% distinct(geo_value, population),
              by = "geo_value"
            ) %>%
            mutate(prediction = prediction * population / 10L**5)
        } else {
          forecast_scaled <- forecast
          actual_eval_data <- hhs_evaluation_data
        }
        # Fix for timing offsets
        actual_eval_data <- actual_eval_data %>% mutate(target_end_date = target_end_date + 3)
        state_geo_values <- actual_eval_data %>%
          pull(geo_value) %>%
          unique()
        forecast_scaled <- forecast_scaled %>%
          filter(geo_value %in% state_geo_values) %>%
          mutate(forecast_date = forecast_date + 3, target_end_date = target_end_date + 3) %>%
          rename("model" = "id")

        # Score
        evaluate_predictions(predictions_cards = forecast_scaled, truth_data = actual_eval_data) %>%
          rename("id" = "model")
      }
    )
  )
  rlang::list2(
    forecasts_and_scores,
    tar_combine(
      delphi_forecasts,
      forecasts_and_scores[["forecast"]],
      command = dplyr::bind_rows(!!!.x) %>% rename(forecaster = id)
    ),
    tar_combine(
      delphi_scores,
      forecasts_and_scores[["score"]],
      command = dplyr::bind_rows(!!!.x) %>% rename(forecaster = id)
    ),
  )
}

#' Relies on the following globals:
#' - `ensemble_grid`
#' Relies on the following targets:
#' - joined_archive_data: the target data, it needs the outcome column to be hhs
#' - hhs_evaluation_data: the true values of the target data
make_ensembles_and_scores <- function() {
  tar_map(
    values = ensemble_grid,
    names = id,
    tar_target(
      name = ensemble_forecast,
      command = {
        ensemble(
          joined_archive_data,
          children_ids,
          "hhs",
          extra_sources = "chng",
          ensemble_args,
          ensemble_args_names
        )
      },
      priority = .9999
    ),
    tar_target(
      name = ensemble_scores,
      command = {
        evaluate_predictions(predictions_cards = ensemble_forecast, truth_data = hhs_evaluation_data)
      }
    )
  )
}


make_external_names_and_scores <- function() {
  external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
  project_path <- Sys.getenv("TAR_PROJECT", "")
  if (external_scores_path != "") {
    external_names_and_scores <- list(
      tar_target(
        name = external_scores_df,
        command = {
          qs::qread(paste0(project_path, "/", external_scores_path)) %>%
            group_by(forecaster) %>%
            targets::tar_group()
        },
        iteration = "group",
        garbage_collection = TRUE
      ),
      tar_target(
        name = external_names,
        command = {
          external_scores_df %>%
            group_by(forecaster) %>%
            group_keys() %>%
            pull(forecaster)
        },
        garbage_collection = TRUE
      ),
      tar_target(
        name = external_scores,
        pattern = map(external_scores_df),
        command = {
          external_scores_df
        },
        # This step causes the pipeline to exit with an error, apparently due to
        # running out of memory. Run this in series on a non-parallel `crew`
        # controller to avoid.
        # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
        resources = tar_resources(
          crew = tar_resources_crew(controller = "serial_controller")
        ),
        memory = "transient",
        garbage_collection = TRUE
      )
    )
  } else {
    external_names_and_scores <- list(
      tar_target(
        name = external_names,
        command = {
          c()
        }
      ),
      tar_target(
        name = external_scores,
        command = {
          data.frame()
        }
      )
    )
  }
}

make_historical_flu_data_targets <- function() {
  rlang::list2(
    tar_target(
      name = hhs_archive_data_asof,
      command = {
        get_health_data(as.Date(ref_time_values), disease = "flu") %>%
          mutate(version = as.Date(ref_time_values)) %>%
          relocate(geo_value, time_value, version, hhs)
      },
      pattern = map(ref_time_values)
    ),
    tar_target(
      name = hhs_archive,
      command = {
        hhs_archive_data <- hhs_archive_data_asof %>%
          as_epi_archive(compactify = TRUE) %>%
          daily_to_weekly_archive(agg_columns = "hhs")
        hhs_archive_data$DT %>%
          add_season_info() %>%
          mutate(agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse("us" == geo_value, "nation", "state"))) %>%
          add_pop_and_density() %>%
          mutate(hhs = hhs / population * 10L^5) %>%
          mutate(source = "nhsn") %>%
          mutate(agg_level = ifelse(geo_value == "us", "nation", "state")) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE) %>%
          `$`("DT") %>%
          select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
      }
    ),
    tar_target(
      name = flusurv,
      command = {
        flusurv_adjusted <- generate_flusurv_adjusted()
        flusurv_adjusted$DT %>%
          mutate(time_value = time_value + 3, version = version + 3) %>%
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
          mutate(time_value = time_value + 3, version = time_value) %>%
          select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
        to_keep <-
          ili_plus %>%
          group_by(geo_value, season) %>%
          summarize(total_count = length(hhs), .groups = "drop") %>%
          filter(total_count >= 20) %>%
          select(geo_value, season)
        to_keep %>%
          left_join(
            ili_plus,
            by = join_by(geo_value, season),
            relationship = "many-to-many"
          ) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE) %>%
          `$`("DT")
      }
    ),
    tar_target(
      name = flusion_data_archive,
      command = {
        flusion_data_archive <-
          bind_rows(ili_plus, flusurv, hhs_archive) %>%
          add_pop_and_density() %>%
          as_epi_archive(compactify = TRUE, other_keys = "source")
        flusion_data_archive <- flusion_data_archive$DT %>%
          filter(
            !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
            !is.na(hhs),
            time_value <= end_date
          ) %>%
          relocate(source, geo_value, time_value, version, hhs, agg_level, season, season_week, year, population, density) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        flusion_data_archive
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
          signal = "pct_ed_visits_influenza",
          time_type = "week",
          geo_type = "state",
          geo_values = "*"
        )
        nssp_hhs <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "nssp",
          signal = "pct_ed_visits_influenza",
          time_type = "week",
          geo_type = "hhs",
          geo_values = "*"
        )
        nssp_archive <- nssp_state %>%
          bind_rows(nssp_hhs) %>%
          select(geo_value, time_value, issue, nssp = value) %>%
          as_epi_archive(compactify = TRUE) %>%
          `$`("DT") %>%
          # End of week to midweek correction.
          mutate(time_value = time_value + 3) %>%
          # Artifically add in a one-week latency.
          mutate(version = time_value + 7) %>%
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        nssp_archive
      }
    ),
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
            signal = glue::glue("s0{search_name}_smoothed_search"),
            time_type = "day",
            geo_type = "state",
            geo_values = "*"
          )
          google_symptoms_hhs_archive <- retry_fn(
            max_attempts = 10,
            wait_seconds = 1,
            fn = pub_covidcast,
            source = "google-symptoms",
            signal = glue::glue("s0{search_name}_smoothed_search"),
            time_type = "day",
            geo_type = "hhs",
            geo_values = "*"
          )
          google_symptoms_archive_min <-
            google_symptoms_state_archive %>%
            bind_rows(google_symptoms_hhs_archive) %>%
            select(geo_value, time_value, value) %>%
            daily_to_weekly() %>%
            mutate(version = time_value) %>%
            as_epi_archive(compactify = TRUE)
          google_symptoms_archive_min$DT %>%
            mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
            unnest(cols = "source") %>%
            filter(!is.na(value)) %>%
            relocate(source, geo_value, time_value, version, value) %>%
            as_epi_archive(other_keys = "source", compactify = TRUE)
        })
        all_of_them[[1]]$DT %<>% rename(google_symptoms_1_cough = value)
        all_of_them[[2]]$DT %<>% rename(google_symptoms_3_fever = value)
        all_of_them[[3]]$DT %<>% rename(google_symptoms_4_bronchitis = value)
        google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]]) %>% epix_merge(all_of_them[[3]])
        google_symptoms_archive <- google_symptoms_archive$DT %>%
          mutate(google_symptoms = google_symptoms_1_cough + google_symptoms_3_fever + google_symptoms_4_bronchitis) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        pre_pipeline <- google_symptoms_archive %>%
          epix_as_of(as.Date("2023-10-04"))
        colnames <- c("google_symptoms_1_cough", "google_symptoms_3_fever", "google_symptoms_4_bronchitis", "google_symptoms")
        for (colname in colnames) {
          learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
          google_symptoms_archive$DT %<>% data_whitening(colname = colname, learned_params, join_cols = c("geo_value", "source"))
        }
        google_symptoms_archive
      }
    ),
    tar_target(
      name = nwss_coarse,
      command = {
        files <- file.info(list.files(here::here("aux_data/nwss_flu_data/"), full.names = TRUE))
        most_recent <- rownames(files)[which.max(files$mtime)]
        nwss <- readr::read_csv(most_recent)
        state_code <- readr::read_csv(here::here("aux_data", "flusion_data", "state_codes_table.csv"), show_col_types = FALSE)
        hhs_codes <- readr::read_csv(here::here("aux_data", "flusion_data", "state_code_hhs_table.csv"), show_col_types = FALSE)
        state_to_hhs <- hhs_codes %>%
          left_join(state_code, by = "state_code") %>%
          select(hhs_region = hhs, geo_value = state_id)
        pop_data <- gen_pop_and_density_data()
        nwss %<>%
          mutate(agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse("us" == geo_value, "nation", "state"))) %>%
          add_pop_and_density() %>%
          mutate(
            nwss_rate = value / population * 100000,
            nwss_region_rate = region_value / population * 100000,
            nwss_national_rate = national_value / population * 100000
          ) %>%
          drop_na() %>%
          select(-agg_level, -year, -agg_level, -population, -density)
        nwss_hhs_region <-
          nwss %>%
          left_join(state_to_hhs, by = "geo_value") %>%
          mutate(year = year(time_value)) %>%
          left_join(pop_data, by = join_by(geo_value, year)) %>%
          select(-year, density) %>%
          group_by(time_value, hhs_region) %>%
          summarize(
            value = sum(value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            nwss_rate = sum(nwss_rate * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
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
            nwss_rate,
            nwss_region = region_value,
            nwss_national = national_value
          ) %>%
          mutate(time_value = time_value - 3, version = time_value) %>%
          arrange(geo_value, time_value) %>%
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          as_epi_archive(other_keys = "source")
      }
    ),
    tar_target(
      name = hhs_region,
      command = {
        hhs_region <- readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv")
        state_id <- readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv")
        hhs_region %>%
          left_join(state_id, by = "state_code") %>%
          select(hhs_region = hhs, geo_value = state_id) %>%
          mutate(hhs_region = as.character(hhs_region))
      }
    ),
    tar_target(
      name = hhs_evaluation_data,
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
          )
      }
    ),
    tar_target(
      name = joined_archive_data,
      command = {
        joined_archive_data <-
          flusion_data_archive %>%
          epix_merge(google_symptoms_archive, sync = "locf") %>%
          epix_merge(nwss_coarse, sync = "locf") %>%
          epix_merge(nssp_archive, sync = "locf") %>%
          `$`("DT") %>%
          drop_na(agg_level) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)

        # Jank adding of hhs_region column to the data
        joined_archive_data <- joined_archive_data$DT %>%
          add_hhs_region_sum(hhs_region) %>%
          as_epi_archive(other_keys = "source")
        # make the data cache via the dummy forecaster not expected to be returned
        slide_forecaster(
          epi_archive = joined_archive_data,
          outcome = "hhs",
          ahead = 0,
          forecaster = dummy_forecaster,
          n_training_pad = 30L,
          forecaster_args = list(),
          forecaster_args_names = list(),
          ref_time_values = ref_time_values,
          start_date = as.Date("2023-10-04"),
          end_date = as.Date("2024-04-24"),
          date_range_step_size = 7L,
          cache_key = "joined_archive_data"
        )
        joined_archive_data
      }
    )
  )
}
