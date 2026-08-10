#' Shared utility functions for targets
#'
#' This file contains utility functions that can be used by both COVID and flu
#' forecasting pipelines.

#' Get partially applied forecaster function
#'
#' params is defined by the values of the tar_map: a named list of forecaster
#' arguments (tar_map substitutes list-column values as literals, names intact).
#' Defining this function inside the target causes scope issues.
#'
#' @param id Forecaster ID
#' @return A partially applied forecaster function
#' @export
get_partially_applied_forecaster <- function(forecaster, ahead, params) {
  function(epi_data, ...) rlang::inject(forecaster(epi_data, ..., ahead = ahead, !!!params))
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
#' Variables with 'g_' prefix are globals defined in the calling script.
#' implicit global dependencies:
#' - g_forecaster_params_grid (needs `sort_quantiles` and `output_scale` columns)
#' - g_time_value_adjust
#' implicit target dependencies:
#' - joined_archive_data
#' - hhs_evaluation_data
#' - aheads
#' - forecast_dates
#' - state_geo_values
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
        # Same runner as prod: snapshot the archive as-of each forecast date and
        # forecast. Explore is the degenerate case of run_forecaster's wrapping
        # (asof policy, no source filter/extra join/geo exclusion). Explore's
        # aheads are already in days and its forecasters are day-native, so
        # ahead_multiplier is 1 (the make_forecaster_grid default). `sort_quantiles`
        # is a spec column (was a `g_disease == "flu"` grep). target_end_date stays
        # Wednesday here; the Wednesday->Saturday shift is applied downstream
        # (score/combine).
        archive_hash <- rlang::hash(joined_archive_data)
        map(forecast_dates, function(fdate) {
          # Revision-aware forecasters (needs_archive) train on past vintages, so
          # they get the truncated archive; everyone else gets the as-of snapshot.
          input <- if (needs_archive) {
            make_forecast_archive_snapshot(joined_archive_data, forecast_date = fdate, generation_date = fdate)
          } else {
            make_forecast_snapshot(
              joined_archive_data,
              forecast_date = fdate,
              generation_date = fdate,
              cache_key = "joined_archive_data",
              archive_hash = archive_hash
            )
          }
          run_forecaster(
            snapshot = input,
            forecaster = forecaster,
            aheads = aheads * ahead_multiplier,
            params = params,
            id = id,
            sort_quantiles = sort_quantiles
          )
        }) %>%
          bind_rows() %>%
          mutate(ahead = as.numeric(target_end_date - forecast_date))
      },
      pattern = map(aheads)
    ),
    tar_target(
      name = score,
      command = {
        # `output_scale` (spec column) replaces sniffing hhs_evaluation_data for a
        # `population` column: "per100k" forecasts are rescaled to counts to match
        # the truth data; "count" forecasts are compared as-is.
        forecast_scaled <- forecast
        if (output_scale == "per100k") {
          forecast_scaled <- forecast_scaled %>%
            left_join(
              hhs_evaluation_data %>% distinct(geo_value, population),
              by = "geo_value"
            ) %>%
            mutate(value = value * population / 10L**5)
        }
        forecast_scaled <- forecast_scaled %>%
          # Push the Wednesday markers to Saturday, to match targets with truth data.
          mutate(
            forecast_date = forecast_date + g_time_value_adjust,
            target_end_date = target_end_date + g_time_value_adjust
          ) %>%
          # `model` is the forecaster id. It cannot be written as
          # `rename(model = forecaster)`: `forecaster` is a tar_map grid column,
          # so tar_map substitutes the bare symbol with the forecaster *function*
          # name, renaming a column that doesn't exist. `id` is the same grid's id
          # literal (== the stamped `forecaster` column), and `any_of("forecaster")`
          # is a string tar_map won't touch.
          rename(prediction = value) %>%
          mutate(model = id) %>%
          select(-any_of("forecaster"))
        evaluate_predictions(forecasts = forecast_scaled, truth_data = hhs_evaluation_data) %>%
          rename(forecaster = model)
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
          rename(prediction = value) %>%
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
          filter(geo_value %in% state_geo_values)
      }
    )
  )

  combined_forecasts_and_scores
}

#' Create joined forecast and score targets
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#' Target dependencies:
#' - delphi_forecasts
#' - external_forecasts
#' - hhs_evaluation_data
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
            output_file = here::here(g_reports_dir, paste0(g_disease, "-notebook-", forecaster_family, "-", g_season, ".html"))
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
          output_file = here::here(g_reports_dir, paste0(g_disease, "-overall-notebook-", g_season, ".html"))
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
  # Leave at least 4 cores free for other processes, but cap at 30 workers.
  num_workers <- min(max(parallel::detectCores() - 4L, 1L), 30L)

  tar_option_set(
    # qs is a fast serialization format for R objects.
    format = "qs",
    # More or less default crew settings for local parallelization.
    # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
    controller = crew_controller_local(
      workers = num_workers,
      garbage_collection = TRUE,
      options_local = crew_options_local(log_directory = "local_logs")
    ),
    error = "stop",
    # Run every n tasks.
    garbage_collection = 5,
  )

  # Readr options.
  options(readr.show_progress = FALSE)
  options(readr.show_col_types = FALSE)
}
