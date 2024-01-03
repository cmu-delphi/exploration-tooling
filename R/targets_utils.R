#' Make common targets for fetching data
#'
#' Relies on the following globals:
#' - `hhs_signal`
#' - `chng_signal`
#' - `fetch_args`
#' - `eval_time`
#' - `traing_time`
#' @export
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
            actual = value,
            target_end_date = time_value
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
        epidatr::pub_covidcast(
          source = "chng",
          signals = chng_signal,
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
        epix_merge(hhs_archive_data_2022, chng_archive_data_2022, sync = "locf")$DT %>%
          drop_na() %>%
          filter(!geo_value %in% c("as", "pr", "vi", "gu", "mp")) %>%
          epiprocess::as_epi_archive()
      }
    )
  )
}

#' Make list of common forecasters for forecasting experiments across projects
#' @export
make_shared_grids <- function() {
  list(
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = c("linreg", "quantreg"),
      ahead = c(1:7, 14, 21, 28),
      pop_scaling = FALSE
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = c("linreg", "quantreg"),
      ahead = c(1:7, 14, 21, 28),
      lags = list(c(0, 3, 5, 7, 14), c(0, 7, 14), c(0,7,14,24)),
      pop_scaling = FALSE
    ),
    tidyr::expand_grid(
      forecaster = "flatline_fc",
      ahead = c(1:7, 14, 21, 28)
    )
  )
}
#' Make list of common ensembles for forecasting experiments across projects
#' @export
make_shared_ensembles <- function() {
  ex_forecaster <- list(
    forecaster = "scaled_pop",
    trainer = "linreg",
    pop_scaling = FALSE,
    lags = c(0, 3, 5, 7, 14)
  )
  # ensembles don't lend themselves to expand grid (inherently needs a list for sub-forecasters)
  tribble(
    ~ensemble, ~ensemble_params, ~forecasters,
    # mean forecaster
    "ensemble_average",
    list(average_type = "mean"),
    list(
      ex_forecaster,
      list(forecaster = "flatline_fc")
    ),
    # median forecaster
    "ensemble_average",
    list(average_type = "median"),
    list(
      ex_forecaster,
      list(forecaster = "flatline_fc")
    ),
  )
}

#' Make forecasts and scores by ahead targets
#' @description
#' globals this depends on:
#' Relies on the following globals:
#' - `date_step`
#' @export
make_forecasts_and_scores_by_ahead <- function() {
  tar_map(
    values = targets_param_grid,
    names = id,
    unlist = FALSE,
    tar_target_raw(
      name = ONE_AHEAD_FORECAST_NAME,
      command = expression(
        slide_forecaster(
          data = joined_archive_data_2022,
          outcome = "hhs",
          extra_sources = "",
          forecaster = forecaster,
          n_training_pad = 30L,
          forecaster_args = params,
          forecaster_args_names = param_names,
          date_range_step_size = date_step
        )
      )
    ),
    tar_target_raw(
      name = ONE_AHEAD_SCORE_NAME,
      command = expression(
        run_evaluation_measure(
          data = forecast_by_ahead,
          evaluation_data = hhs_evaluation_data,
          measure = list(
            wis = weighted_interval_score,
            ae = absolute_error,
            cov_80 = interval_coverage(0.8)
          )
        )
      )
    )
  )
}

#' Make forecasts and scores targets
#' @export
make_forecasts_and_scores <- function() {
  tar_map(
    values = forecaster_parent_id_map,
    names = parent_id,
    tar_target(
      name = forecast,
      command = {
        bind_rows(forecast_component_ids) %>%
          mutate(parent_forecaster = parent_id)
      }
    ),
    tar_target(
      name = score,
      command = {
        bind_rows(score_component_ids) %>%
          mutate(parent_forecaster = parent_id)
      }
    )
  )
}

#' Make ensemble targets
#' @export
make_ensemble_targets_and_scores <- function() {
  ensembles_and_scores <- tar_map(
    values = ensemble_parent_id_map,
    names = parent_id,
    tar_target(
      name = ensemble,
      command = {
        bind_rows(ensemble_component_ids) %>%
          mutate(parent_ensemble = parent_id)
      }
    ),
    tar_target(
      name = ensemble_score,
      command = {
        bind_rows(score_component_ids) %>%
          mutate(parent_ensemble = parent_id)
      }
    )
  )
}


#' Make external names and scores targets
#' @importFrom targets tar_target tar_group
#' @export
make_external_names_and_scores <- function() {
  external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
  if (external_scores_path != "") {
    external_names_and_scores <- list(
      tar_target(
        name = external_scores_df,
        command = {
          readRDS(external_scores_path) %>%
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
