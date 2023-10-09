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
  controller = crew::crew_controller_local(workers = parallel::detectCores() - 5),
  )
# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# where the forecasters and parameters are joined; see either the variable param_grid or `tar_read(forecasters)`
source("covid_hosp_explore/forecaster_instantiation.R")
source("covid_hosp_explore/data_targets.R")

forecasts_and_scores <- tar_map(
  values = forecaster_param_grids,
  names = id,
  unlist = FALSE,
  tar_target(
    name = forecast,
    command = {
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
    }
  ),
  tar_target(
    name = score,
    command = {
      run_evaluation_measure(
        data = forecast,
        evaluation_data = hhs_evaluation_data,
        measure = list(
          wis = weighted_interval_score,
          ae = absolute_error,
          ic80 = interval_coverage(0.8)
        )
      )
    }
  )
)

# The combine approach below is taken from the manual:
#   https://books.ropensci.org/targets/static.html#combine
# The key is that the map above has unlist = FALSE.
ensemble_forecast <- tar_map(
  values = list(a = c(300, 15)),
  tar_combine(
    name = ensemble_forecast,
    # TODO: Needs a lookup table to select the right forecasters
    list(
      forecasts_and_scores[["forecast"]][[1]],
      forecasts_and_scores[["forecast"]][[2]]
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
notebooks <- list(
  tar_render(
    name = report,
    path = "extras/report.Rmd",
    params = list(
      exclude_geos = c("as", "gu", "mp", "vi")
    )
  )
)

list(
  data,
  forecasters,
  forecasts_and_scores,
  ensemble_forecast,
  notebooks
)
