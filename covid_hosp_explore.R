suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes) # Load other packages as needed.

  library(crew)
  library(dplyr)
  library(epipredict)
  library(epieval)
  library(lubridate)
  library(parsnip)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(rlang)
})

# The external scores processing causes the pipeline to exit with an error,
# apparently due to running out of memory. Set up a non-parallel `crew`
# controller to avoid.
# https://books.ropensci.org/targets/crew.html#heterogeneous-workers
main_controller <- crew_controller_local(
    name = "main_controller",
    workers = parallel::detectCores() - 5
  )
serial_controller <- crew_controller_local(
    name = "serial_controller",
    workers = 1L
  )

tar_option_set(
  packages = c(
    "assertthat",
    "dplyr",
    "epieval",
    "epipredict",
    "ggplot2",
    "lubridate",
    "parsnip",
    "tibble",
    "tidyr",
    "epidatr"
  ), # packages that your targets need to run
  imports = c("epieval", "parsnip"),
  format = "qs", # Optionally set the default storage format. qs is fast.
  controller = crew_controller_group(main_controller, serial_controller),
  # Set default crew controller.
  # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
  resources = tar_resources(
      crew = tar_resources_crew(controller = "main_controller")
    )
  )
# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# where the forecasters and parameters are joined; see either the variable param_grid or `tar_read(forecasters)`
source("covid_hosp_explore/forecaster_instantiation.R")
source("covid_hosp_explore/data_targets.R")
source("covid_hosp_explore/dynamic_constants.R")

forecasts_and_scores_by_ahead <- tar_map(
  values = forecaster_param_grids,
  names = id,
  unlist = FALSE,
  tar_target_raw(
    name = ONE_AHEAD_FORECAST_NAME,
    command = expression(
      forecaster_pred(
        data = joined_archive_data_2022,
        outcome = "hhs",
        extra_sources = "",
        forecaster = forecaster,
        slide_training = Inf,
        n_training_pad = 30L,
        forecaster_args = params,
        forecaster_args_names = param_names
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
          ic80 = interval_coverage(0.8)
        )
      )
    )
  )
)

forecasts_and_scores <- tar_map(
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

ensemble_keys <- list(a = c(300, 15))
ensembles <- tar_target(
  name = ensembles,
  command = {
    ensemble_keys
  }
)

# The combine approach below is taken from the manual:
#   https://books.ropensci.org/targets/static.html#combine
# The key is that the map above has unlist = FALSE.
ensemble_forecast <- tar_map(
  values = ensemble_keys,
  tar_combine(
    name = ensemble_forecast,
    # TODO: Needs a lookup table to select the right forecasters
    list(
      forecasts_and_scores_by_ahead[["forecast_by_ahead"]][[1]],
      forecasts_and_scores_by_ahead[["forecast_by_ahead"]][[2]]
    ),
    command = {
      bind_rows(!!!.x, .id = "forecaster") %>%
        pivot_wider(
          names_prefix = "forecaster",
          names_from = forecaster,
          values_from = value
        ) %>%
        mutate(
          value = a + rowMeans(across(starts_with("forecaster")))
        ) %>%
        select(-starts_with("forecaster"))
    }
  ),
  tar_target(
    name = ensemble_score,
    command = {
      run_evaluation_measure(
        data = ensemble_forecast,
        evaluation_data = hhs_evaluation_data,
        measures = list(
          wis = weighted_interval_score,
          ae = absolute_error,
          ic80 = interval_coverage(0.8)
        )
      )
    }
  )
)

if (LOAD_EXTERNAL_SCORES) {
  external_names_and_scores <- list(
    tar_target(
      name = external_scores_df,
      command = {
        readRDS(external_scores_path)
      }
    ),
    tar_target(
      name = external_names,
      command = {
        external_scores_df %>%
          group_by(forecaster) %>%
          group_keys() %>%
          pull(forecaster)
      }
    ),
    tar_target(
      name = group_dfs,
      command = {
        df_list <- external_scores_df %>%
        group_by(forecaster) %>%
        group_split()

        names(df_list) <- external_names
        df_list
      }
    ),
    tar_target(
      name = external_scores,
      pattern = map(external_names),
      command = {
       group_dfs[[external_names]]
      },
      # This step causes the pipeline to exit with an error, apparently due to
      # running out of memory. Run this in series on a non-parallel `crew`
      # controller to avoid.
      # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
      resources = tar_resources(
        crew = tar_resources_crew(controller = "serial_controller")
      )
    )
  )
} else {
  external_names_and_scores <- tar_target(
    name = external_names,
    command = {
      c()
    }
  )
}


list(
  data,
  forecasters,
  forecasts_and_scores_by_ahead,
  forecasts_and_scores,
  ensembles,
  ensemble_forecast,
  external_names_and_scores
)
