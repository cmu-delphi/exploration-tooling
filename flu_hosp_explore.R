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
  library(epidatr)
})

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
# bind them together and give static ids; if you add a new field to a given
# expand_grid, everything will get a new id, so it's better to add a new
# expand_grid instead
param_grid <- bind_rows(map(grids, add_id)) %>% relocate(id, .after = last_col())

forecaster_param_grids <- make_target_param_grid(param_grid) %>%
  ## TODO This forecaster is hanging. Filter it out for now.
  filter(id != "necessary endless 5")

# not actually used downstream, this is for lookup during plotting and human evaluation
forecasters <- list(
  tar_target(
    name = forecasters,
    command = {
      param_grid
    }
  )
)

response_signal <- "confirmed_admissions_influenza_1d_prop_7dav"
target_range <- epirange(from = "20211001", to = "20220401")
data <- list(
  tar_target(
    name = hhs_evaluation_data,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = response_signal,
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
    name = hhs_archive_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = response_signal,
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = target_range,
        issues = "*",
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = chng_archive_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_flu",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = target_range,
        issues = "*",
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
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
      epix_merge(hhs_archive_data_2022, chng_archive_data_2022, sync = "locf")
    }
  ),
  tar_target(
    name = hhs_latest_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
    }
  ),
  tar_target(
    name = chng_latest_data_2022,
    command = {
      epidatr::pub_covidcast(
        source = "chng",
        signals = "smoothed_adj_outpatient_covid",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = epirange(from = "20220101", to = "20220401"),
        fetch_params = fetch_params_list(return_empty = TRUE, timeout_seconds = 100)
      )
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

ensemble_keys <- list(a = c(300, 15))
ensembles <- list(
  tar_target(
    name = ensembles,
    command = {
      ensemble_keys
    }
  )
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
  ensembles,
  ensemble_forecast
)
