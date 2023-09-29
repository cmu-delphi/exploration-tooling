# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

library(dplyr)
library(epipredict)
library(epieval)
library(parsnip)
library(purrr)
library(tibble)
library(tidyr)
library(rlang)

# Set target options:
tar_option_set(
  packages = c(
    "assertthat",
    "dplyr",
    "epieval",
    "epipredict",
    "lubridate",
    "parsnip",
    "tibble",
    "tidyr",
    "epidatr"
  ), # packages that your targets need to run
  imports = c("epieval", "parsnip"),
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  # controller = crew::crew_controller_local(workers = 8),
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()

grids <- list(
  list(
    forecaster = rlang::syms(c("scaled_pop")),
    params = tidyr::expand_grid(
      trainer = rlang::syms(c("linreg", "quantreg")),
      ahead = 1:4,
      pop_scaling = c(TRUE, FALSE)
    )
  ),
  list(
    forecaster = rlang::syms(c("scaled_pop")),
    params = tidyr::expand_grid(
      trainer = rlang::syms(c("linreg", "quantreg")),
      ahead = 5:7,
      pop_scaling = c(TRUE, FALSE)
    )
  )
)
make_target_param_grid <- function(grids) {
  purrr::map(grids, function(grid) {
    tibble(
      forecaster = grid$forecaster,
      params = transpose(grid$params),
      param_names = list(names(grid$params))
    )
  }) %>%
    bind_rows() %>%
    mutate(id = row_number())
}
forecaster_param_grids <- make_target_param_grid(grids)

data <- list(
  tar_target(
    name = hhs_evaluation_data,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "2020-01-01", to = "2024-01-01"),
      ) %>%
        rename(
          actual = value,
          target_end_date = time_value
        )
    }
  ),
  tar_target(
    name = hhs_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        issues = "*",
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = chng_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_covid",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        issues = "*",
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = data_archive_2022,
    command = {
      hhs_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("hhs" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      chng_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("chng" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      epix_merge(hhs_data_2022, chng_data_2022, sync = "locf")
    }
  )
)
forecasts_and_scores <- tar_map(
  values = forecaster_param_grids,
  names = id,
  unlist = FALSE,
  tar_target(
    name = forecast,
    command = {
      forecaster_pred(
        data = data_archive_2022,
        outcome = "hhs",
        extra_sources = "",
        forecaster = forecaster,
        slide_training = Inf,
        slide_training_pad = 30L,
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
list(
  data,
  forecasts_and_scores,
  ensemble_forecast
)
