source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# Debug mode will replace all forecasters with a fast dummy forecaster. Helps
# with prototyping the pipeline.
debug <- TRUE

# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations_ <- list(
  tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("linreg", "quantreg"),
    lags = list(c(0, 3, 5, 7, 14), c(0, 7, 14), c(0, 7, 14, 24)),
    pop_scaling = c(TRUE, FALSE)
  ),
  tidyr::expand_grid(
    forecaster = "smoothed_scaled",
    trainer = c("quantreg"),
    lags = list(
      # list(smoothed, sd)
      list(c(0, 3, 5, 7, 14), c(0)),
      list(c(0, 7, 14, 21, 28), c(0)),
      list(c(0, 2, 4, 7, 14, 21, 28), c(0))
    ),
    pop_scaling = c(TRUE, FALSE)
  ),
  tidyr::expand_grid(
    forecaster = "flatline_fc",
  )
) %>%
  map(function(x) {
    if (debug) {
      x$forecaster <- "dummy_forecaster"
    }
    x
  }) %>%
  map(add_id)

# Make sure all ids are unique.
stopifnot(length(forecaster_parameter_combinations_$id %>% unique()) == length(forecaster_parameter_combinations_$id))
# Build targets-internal tibble to map over.
forecaster_grid <- forecaster_parameter_combinations_ %>%
  map(make_forecaster_grid) %>%
  bind_rows()

scaled_pop_not_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "linreg",
  pop_scaling = FALSE,
  lags = list(c(0, 3, 5, 7, 14))
)
scaled_pop_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "linreg",
  pop_scaling = FALSE,
  lags = list(c(0, 3, 5, 7, 14))
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
    scaled_pop_not_scaled
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
    if (debug) {
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
chng_signal <- "smoothed_adj_outpatient_covid"
eval_time <- epidatr::epirange(from = "2020-01-01", to = "2024-01-01")
training_time <- epidatr::epirange(from = "2020-08-01", to = "2023-12-18")
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
data_targets <- make_data_targets()


# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
date_step <- 7L
forecasts_and_scores <- make_forecasts_and_scores()

ensembles_and_scores <- make_ensembles_and_scores()
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
      c(1:7, 14, 21, 28)
    }
  ),
  data_targets,
  forecasts_and_scores,
  ensembles_and_scores,
  external_names_and_scores
)
