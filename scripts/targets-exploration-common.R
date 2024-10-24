#' Make common targets for fetching data
#'
#' Relies on the following globals:
#' - `hhs_signal`
#' - `chng_signal`
#' - `fetch_args`
#' - `eval_time`
#' - `training_time`
make_data_targets <- function() {
  list(
    tar_target(
      name = hhs_latest_data,
      command = {
        epidatr::pub_covidcast(
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
        epidatr::pub_covidcast(
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
      name = hhs_archive_data_2022,
      command = {
        epidatr::pub_covidcast(
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
        first_half <- epidatr::pub_covidcast(
          source = "chng",
          signals = chng_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = training_time,
          issues = epidatr::epirange(from = start_time, to = start_time + half),
          fetch_args = fetch_args
        )
        second_half <- epidatr::pub_covidcast(
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
        epix_merge(hhs_archive_data_2022, chng_archive_data_2022, sync = "locf")$DT %>%
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
  tar_map(
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
          extra_sources = "",
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
          mutate(ahead = as.numeric(target_end_date - forecast_date))
        gc()
        return(slid)
      },
      pattern = map(aheads)
    ),
    tar_target(
      name = score,
      command = {
        evaluate_predictions(predictions_cards = forecast, truth_data = hhs_evaluation_data)
      }
    )
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
