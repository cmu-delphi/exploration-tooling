source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# Debug mode will replace all forecasters with a fast dummy forecaster. Helps
# with prototyping the pipeline.
dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))

# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations_ <- rlang::list2(
  scaled_pop_main = tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("linreg", "quantreg"),
    lags = list(
      c(0, 7),
      c(0, 7, 14),
      c(0, 7, 14, 21)
    ),
    pop_scaling = c(TRUE, FALSE),
    n_training = c(6, Inf)
  ),
  smoothed_scaled_main = tidyr::expand_grid(
    forecaster = "smoothed_scaled",
    trainer = c("quantreg"),
    lags = rlang::list2(
      # list(smoothed, sd)
      list(c(0, 7, 14, 21), c(0)),
      list(c(0, 7, 14), c(0)),
      list(c(0, 7, 14), c(0, 7)),
    ),
    smooth_width = as.difftime(8, units = "weeks"),
    sd_width = as.difftime(c(NA, 4, 12), units = "weeks"),
    sd_mean_width = as.difftime(8, units = "weeks"),
    pop_scaling = c(TRUE, FALSE),
    n_training = c(6, Inf)
  ),
  tidyr::expand_grid(
    forecaster = "flatline_fc",
  ),
  tidyr::expand_grid(
    forecaster = "scaled_pop_seasonal",
    trainer = "quantreg",
    lags = list(c(0, 7, 14, 21)),
    pop_scaling = TRUE,
    n_training = Inf,
    seasonal_pca = c("covid", "indicator"),
  )
) %>%
  map(function(x) {
    if (dummy_mode) {
      x$forecaster <- "dummy_forecaster"
    }
    x
  }) %>%
  map(add_id)
s3save(forecaster_parameter_combinations_, object = "covid_2023_forecaster_parameter_combinations.rds", bucket = "forecasting-team-data")

# Make sure all ids are unique.
stopifnot(length(forecaster_parameter_combinations_$id %>% unique()) == length(forecaster_parameter_combinations_$id))
# Build targets-internal tibble to map over.
forecaster_grid <- forecaster_parameter_combinations_ %>%
  map(make_forecaster_grid) %>%
  bind_rows()

scaled_pop_not_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "quantreg",
  lags = c(0, 7, 14, 28),
  pop_scaling = FALSE,
  n_training = Inf
)
scaled_pop_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "quantreg",
  lags = c(0, 7, 14, 28),
  pop_scaling = TRUE,
  n_training = Inf
)
smooth_scaled <- list(
  forecaster = "smoothed_scaled",
  trainer = "quantreg",
  lags =
  # list(smoothed, sd)
    list(c(0, 7, 14, 21, 28), c(0)),
  smooth_width = as.difftime(2, units = "weeks"),
  sd_width = as.difftime(4, units = "weeks"),
  sd_mean_width = as.difftime(2, units = "weeks"),
  pop_scaling = TRUE,
  n_training = Inf
)
# Human-readable object to be used for inspecting the ensembles in the pipeline.
ensemble_parameter_combinations_ <- tribble(
  ~ensemble, ~ensemble_args, ~forecasters,
  # mean forecaster
  "ensemble_average",
  list(average_type = "mean"),
  list(
    scaled_pop_scaled,
    list(forecaster = "flatline_fc")
  ),
  # median forecaster
  "ensemble_average",
  list(average_type = "median"),
  list(
    scaled_pop_scaled,
    scaled_pop_not_scaled,
    smooth_scaled
  ),
  # mean forecaster with baseline
  "ensemble_average",
  list(average_type = "mean"),
  list(
    scaled_pop_not_scaled,
    list(forecaster = "flatline_fc")
  ),
  # median forecaster with baseline
  "ensemble_average",
  list(average_type = "median"),
  list(
    scaled_pop_not_scaled,
    list(forecaster = "flatline_fc")
  )
) %>%
  {
    if (dummy_mode) {
      .$forecasters <- map(.$forecasters, function(x) {
        map(x, function(y) {
          y$forecaster <- "dummy_forecaster"
          y
        })
      })
    }
    .
  } %>%
  mutate(
    children_ids = map(.$forecasters, function(x) {
      map_chr(x, function(y) {
        get_single_id(y)
      })
    })
  ) %>%
  add_id(exclude = "forecasters")
# Check that every ensemble dependent is actually included.
missing_forecasters <- setdiff(
  ensemble_parameter_combinations_ %>% pull(children_ids) %>% unlist() %>% unique(),
  forecaster_grid$id
)
if (length(missing_forecasters) > 0) {
  cli_abort("Ensemble depends on forecasters not included in pipeline: {missing_forecasters}.")
}
# Build targets-internal tibble to map over.
ensemble_grid <- make_ensemble_grid(ensemble_parameter_combinations_)

# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
hhs_signal <- "confirmed_admissions_covid_1d"
date_step <- 7L
eval_time <- epidatr::epirange(from = "2020-01-01", to = "2024-01-01")
training_time <- epidatr::epirange(from = "2020-08-01", to = "2023-12-18")
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
data_targets <- rlang::list2(
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
    name = hhs_evaluation_data,
    command = {
      hhs_latest_data %>%
        select(
          signal,
          geo_value,
          time_value,
          value
        ) %>%
        daily_to_weekly(keys = c("geo_value", "signal")) %>%
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
    name = hhs_archive_data,
    command = {
      res <- epidatr::pub_covidcast(
        source = "hhs",
        signals = hhs_signal,
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = training_time,
        issues = "*",
        fetch_args = fetch_args
      ) %>%
        select(
          geo_value,
          time_value,
          value,
          issue
        ) %>%
        as_epi_archive(compactify = TRUE) %>%
        daily_to_weekly_archive("value")
    }
  ),
  tar_target(
    name = joined_archive_data,
    command = {
      hhs_archive_data$DT %>%
        select(geo_value, time_value, value, version) %>%
        rename("hhs" := value) %>%
        filter(!geo_value %in% c("as", "pr", "vi", "gu", "mp")) %>%
        as_epi_archive(
          compactify = TRUE
        )
    }
  )
)


# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
date_step <- 7L
if (!exists("ref_time_values")) {
  start_date <- as.Date("2023-10-04")
  end_date <- as.Date("2024-04-24")
  ref_time_values <- NULL
}
forecasts_and_scores <- make_forecasts_and_scores()

ensembles_and_scores <- make_ensembles_and_scores()
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
project_path <- Sys.getenv("TAR_PROJECT", "")
external_names_and_scores <- make_external_names_and_scores()


rlang::list2(
  list(
    tar_target(
      name = forecaster_parameter_combinations,
      command = {
        forecaster_parameter_combinations_
      },
      priority = 0.99
    ),
    tar_target(
      name = ensemble_forecasters,
      command = {
        ensemble_parameter_combinations_
      },
      priority = 0.99
    )
  ),
  tar_target(
    name = aheads,
    command = {
      c(7, 14, 21, 28)
    }
  ),
  data_targets,
  forecasts_and_scores,
  ensembles_and_scores,
  external_names_and_scores
)
