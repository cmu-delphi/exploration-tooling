library(targets)
library(tarchetypes) # Load other packages as needed.

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
  # controller = crew::crew_controller_local(workers = parallel::detectCores() - 1),
)



linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()

grids <- list(
  tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("linreg", "quantreg"),
    ahead = 1:4,
    pop_scaling = c(TRUE, FALSE)
  ),
  tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("linreg", "quantreg"),
    ahead = 5:7,
    lags = list(c(0, 3, 5, 7, 14), c(0, 7, 14)),
    pop_scaling = c(TRUE, FALSE)
  )
)


param_grid <- bind_rows(map(grids, add_id)) %>% relocate(id, .after = last_col())

forecaster_param_grids <- make_target_param_grid(param_grid)

big_example <- tar_read(joined_archive_data_2022)
big_example$DT
tribble(~geo_value, ~time_value, ~version, ~a, ~b, )
synth_mean <- 25
synth_sd <- 2
set.seed(12345)
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = 50)
# flatline data in a single state with no versioning confusion
constant <- as_epi_archive(tibble(
  geo_value = "g1",
  time_value = simple_dates,
  version = simple_dates,
  a = synth_mean
))

forecaster_pred(
  data = constant,
  outcome = "hhs",
  extra_sources = "",
  forecaster = forecaster_param_grids$forecaster[[1]],
  slide_training_pad = 30L,
  forecaster_args = forecaster_param_grids$params[[1]],
  forecaster_args_names = forecaster_param_grids$param_namess[[1]]
)

# flatline data in a single state, but with white noise
white_noise <- as_epi_archive(tibble(
  geo_value = "g1",
  time_value = simple_dates,
  version = simple_dates,
  a = rnorm(length(simple_dates), mean = synth_mean, sd = synth_sd)
))
# flatline data in a single state, but with poisson noise
poisson_noise <- as_epi_archive(tibble(
  geo_value = "g1",
  time_value = simple_dates,
  version = simple_dates,
  a = rpois(length(simple_dates), synth_mean)
))
x1 <- as_epi_archive(
  data.table::as.data.table(
    tibble::tribble(
      ~geo_value, ~time_value, ~version, ~x_value,
      "g1", simple_dates, simple_dates, 15,
    ) %>%
      tidyr::unchop(c(version, x_value)) %>%
      dplyr::mutate(dplyr::across(c(x_value), ~ dplyr::if_else(grepl("NA", .x), NA_character_, .x)))
  )
)
# a missing state (the last version is at the very end)
missing_state <- as_epi_archive(rbind(
  constant$DT,
  tibble(
    geo_value = "g2",
    time_value = simple_dates,
    version = max(simple_dates) + 3,
    a = synth_mean
  )
))
# side data has randomly NA values that don't get updated
missing_side_data <- as_epi_archive(mutate(
  constant$DT,
  b = sample(c(synth_mean / 5, NA),
    nrow(constant$DT),
    replace = TRUE
  )
))
# constant, but version is delayed
delayed <- as_epi_archive(tibble(
  geo_value = "g1",
  time_value = simple_dates,
  version = 5 + simple_dates,
  a = synth_mean
))

data_grid <- tibble(ex_dataset = list(constant, white_noise, poisson_noise, missing_state, missing_side_data, delayed))
expand_grid(forecaster_param_grids, data_grid)
data <- tar_map(
  values = forecaster_param_grids,
  names = id,
  unlist = FALSE,
  tar_target(
    name = test_missing_state,
    command = {
      missing_state
    }
  ),
  tar_target(
    name = test_missing_side_data,
    command = {
      missing_side_data
    }
  ),
  tar_target(
    name = test_delayed,
    command = {
      delayed
    }
  ),
  tar_target(
    name = test_poisson_noise,
    command = {
      poisson_noise
    }
  ),
  tar_target(
    name = test_poisson_noise,
    command = {
      poisson_noise
    }
  ),
  tar_target(
    name = test_white_noise,
    command = {
      white_noise
    }
  ),
  tar_target(
    name = test_constant,
    command = {
      constant
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
        data = joined_archive_data_2022,
        outcome = "hhs",
        extra_sources = "",
        forecaster = forecaster,
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
          cov_80 = interval_coverage(0.8)
        )
      )
    }
  )
)

list(
  data,
  forecasters,
  forecasts_and_scores,
  ensemble_forecast,
  notebooks
)
