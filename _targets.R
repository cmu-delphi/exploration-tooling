# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

library(epipredict)
library(epieval)
library(parsnip)
library(tibble)
library(tidyr)

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
  controller = crew::crew_controller_local(workers = 8),
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
quantreg <- quantile_reg()

forecaster_param_grids <- rbind(
  # Define a separate parameter grid per forecaster
  tibble(
    forecaster = rlang::syms(c("scaled_pop")),
    expand_grid(
      trainer = rlang::syms(c("linreg", "quantreg")),
      ahead = 1:4,
      pop_scaling = c(TRUE, FALSE)
    )
  )
)

# Replace the target list below with your own:
list(
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
        fetch_args = fetch_args_list(return_empty = TRUE, timeout_seconds = 100)
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
        fetch_args = fetch_args_list(return_empty = TRUE, timeout_seconds = 100)
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
  ),
  tar_map(
    values = forecaster_param_grids,
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
          ahead = ahead,
          trainer = trainer
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
  ),
  tar_map(
    values = list(a = c(300, 15)),
    tar_target(
      name = ensemble_forecast,
      command = {
        Reduce(function(x, y) {
          full_join(x, y, by = c("geo_value", "forecast_date", "target_end_date", "quantile")) %>%
            mutate(value = (value.x + value.y + a) / 2) %>%
            select(-value.x, -value.y)
        }, list(
          forecast_scaled_pop_linreg_3_FALSE,
          forecast_scaled_pop_linreg_3_TRUE,
          forecast_scaled_pop_quantreg_3_FALSE,
          forecast_scaled_pop_quantreg_3_TRUE
        ))
      }
    ),
    tar_target(
      name = ensemble_score,
      command = {
        run_evaluation_measure(
          data = ensemble_forecast,
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
)
