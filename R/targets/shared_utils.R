#' Shared utility functions for targets
#'
#' This file contains utility functions that can be used by both COVID and flu
#' forecasting pipelines.


#' Get partially applied forecaster function
#'
#' params and param_names are defined by the values of the
#' tar_map. params is a list of lists, and param_names is the
#' names of parameters in each list. These are separate because
#' targets::tar_map strips the names from lists in a tibble.
#' Defining this function inside the target causes scope issues.
#'
#' @param id Forecaster ID
#' @return A partially applied forecaster function
#' @export
get_partially_applied_forecaster <- function(forecaster, ahead, params, param_names) {
  function(epi_data) rlang::inject(forecaster(epi_data, ahead = ahead, !!!(set_names(params, param_names))))
}


#' Create parameter targets for forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' @return A list of targets for parameters
#' @export
create_parameter_targets <- function() {
  list2(
    tar_target(name = aheads, command = g_aheads),
    tar_target(name = forecast_dates, command = g_forecast_dates),
    # This is for forecaster_lookup.
    tar_target(name = forecaster_params_grid, command = g_forecaster_params_grid),
    # This is for notebook generation.
    tar_target(name = forecaster_parameter_combinations, command = g_forecaster_parameter_combinations)
  )
}

#' Create forecast and score targets
#'
#' @return A list of targets for forecasts and scores
#' @export
create_forecast_targets <- function() {
  forecasts_and_scores <- tar_map(
    values = g_forecaster_params_grid,
    names = id,
    unlist = FALSE,
    tar_target(
      name = forecast,
      command = {
        out <- epix_slide_simple(
          joined_archive_data,
          get_partially_applied_forecaster(forecaster, aheads, params, param_names),
          forecast_dates,
          cache_key = "joined_archive_data"
        )
        if (g_disease == "flu") {
          # TODO: Hack fix because whitening has edge cases. Remove when fixed.
          out %<>% sort_by_quantile()
        }
        out %<>% rename(prediction = value) %>%
          mutate(ahead = as.numeric(target_end_date - forecast_date)) %>%
          mutate(id = id)
        out
      },
      pattern = map(aheads)
    ),
    tar_target(
      name = score,
      command = {
        forecasts <- forecast %>%
          # Push the Wednesday markers to Saturday, to match targets with truth data.
          mutate(forecast_date = forecast_date + 3, target_end_date = target_end_date + 3) %>%
          rename("model" = "id")
        # browser()
        evaluate_predictions(forecasts = forecasts, truth_data = hhs_evaluation_data) %>%
          rename("id" = "model")
      }
    )
  )

  combined_forecasts_and_scores <- rlang::list2(
    forecasts_and_scores,
    tar_combine(
      delphi_forecasts,
      forecasts_and_scores[["forecast"]],
      command = {
        dplyr::bind_rows(!!!.x) %>%
          rename(forecaster = id) %>%
          filter(geo_value %in% state_geo_values) %>%
          # Push the Wednesday markers to Saturday, to match targets with truth data.
          mutate(
            forecast_date = forecast_date + g_time_value_adjust,
            target_end_date = target_end_date + g_time_value_adjust
          )
      }
    ),
    tar_combine(
      delphi_scores,
      forecasts_and_scores[["score"]],
      command = {
        dplyr::bind_rows(!!!.x) %>%
          rename(forecaster = id) %>%
          filter(geo_value %in% state_geo_values)
      }
    )
  )

  combined_forecasts_and_scores
}

#' Create joined forecast and score targets
#'
#' Assumes the global `config` object is set.
#'
#' @param disease Disease name (e.g., "covid" or "flu")
#' @return A list of targets for joined forecasts and scores
#' @export
create_joined_targets <- function() {
  rlang::list2(
    tar_target(joined_forecasts, command = {
      if (g_disease == "flu") {
        rescaled_delphi_forecasts %>% bind_rows(external_forecasts)
      } else {
        delphi_forecasts %>% bind_rows(external_forecasts)
      }
    }),
    tar_target(joined_scores, command = delphi_scores %>% bind_rows(external_scores)),
    tar_map(
      values = list(forecaster_family = unique(g_forecaster_params_grid$family)),
      tar_target(
        name = notebook,
        command = {
          params_subset <- g_forecaster_parameter_combinations[[forecaster_family]]
          filtered_forecasts <- joined_forecasts %>%
            filter(forecaster %in% c(params_subset$id, outside_forecaster_subset))
          filtered_scores <- joined_scores %>%
            filter(forecaster %in% c(params_subset$id, outside_forecaster_subset))

          rmarkdown::render(
            "scripts/reports/comparison-notebook.Rmd",
            params = list(
              forecaster_parameters = params_subset,
              forecaster_family = forecaster_family,
              forecasts = filtered_forecasts,
              scores = filtered_scores,
              truth_data = hhs_evaluation_data,
              disease = g_disease
            ),
            output_file = here::here(g_reports_dir, paste0(g_disease, "-notebook-", forecaster_family, ".html"))
          )
        }
      )
    ),
    tar_target(
      overall_notebook,
      command = {
        rmarkdown::render(
          "scripts/reports/overall-comparison-notebook.Rmd",
          params = list(
            forecaster_parameters = g_forecaster_parameter_combinations,
            forecasts = joined_forecasts,
            scores = joined_scores,
            truth_data = hhs_evaluation_data,
            disease = g_disease
          ),
          output_file = here::here(g_reports_dir, paste0(g_disease, "-overall-notebook.html"))
        )
      }
    ),
    # TODO: Fix notebook, it's missing process_nhsn_data() function.
    # tar_target(
    #   new_data_notebook,
    #   command = {
    #     rmarkdown::render("scripts/reports/new_data.Rmd", output_file = here::here("reports", "new_data.html"))
    #   }
    # )
  )
}

set_targets_config <- function() {
  # On tanka, we have 64 cores, but we leave some free to try to reduce thrashing
  # and to allow for other users.
  if (parallel::detectCores() == 64) {
    num_workers <- 30L
  } else {
    num_workers <- max(parallel::detectCores() - 4L, 1L)
  }

  tar_option_set(
    format = "qs", # Optionally set the default storage format. qs is fast.
    controller = crew_controller_local(
      workers = num_workers,
      garbage_collection = TRUE,
      options_local = crew_options_local(log_directory = "local_logs")
    ),
    # Set default crew controller.
    # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
    memory = "transient",
    error = "stop",
    garbage_collection = TRUE,
    storage = "worker",
    retrieval = "worker" # this may need to go back to main
  )

  # Readr options.
  options(readr.show_progress = FALSE)
  options(readr.show_col_types = FALSE)
}
