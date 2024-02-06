# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# where the forecasters and parameters are joined; see either the variable param_grid or `tar_read(forecasters)`
source("scripts/targets-common.R")

# Add custom parameter combinations in the list below.
make_unique_grids <- function() {
  list(
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = c("linreg", "quantreg"),
      ahead = c(1:7, 14, 21, 28),
      pop_scaling = TRUE
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = c("linreg", "quantreg"),
      ahead = c(1:7, 14, 21, 28),
      lags = list(c(0, 3, 5, 7, 14), c(0, 7, 14)),
      pop_scaling = TRUE
    ),
    tidyr::expand_grid(
      forecaster = "smoothed_scaled",
      trainer = c("quantreg"),
      ahead = c(1:7, 14, 21, 28),
      #
      lags = list(
        #        smoothed,      sd,          smoothed,   sd
        list(c(0, 3, 5, 7, 14), c(0)),
        list(c(0, 7, 14), c(0)),
        list(c(0, 2, 4, 7, 14, 21, 28), c(0))
      ),
      pop_scaling = TRUE
    )
  )
}
#
make_unique_ensemble_grid <- function() {
  # median forecaster averaging a pop scaled and not pop scaled
  tribble(
    ~ensemble, ~ensemble_params, ~forecasters,
    # mean forecaster
    "ensemble_average",
    list(average_type = "mean"),
    list(
      list(
        forecaster = "scaled_pop",
        trainer = "linreg",
        pop_scaling = TRUE,
        lags = c(0L, 3L, 5L, 7L, 14L)
      ),
      list(forecaster = "flatline_fc")
    ),
    # median forecaster
    "ensemble_average",
    list(average_type = "median"),
    list(
      list(
        forecaster = "scaled_pop",
        trainer = "linreg",
        pop_scaling = TRUE,
        lags = c(0, 3, 5, 7, 14)
      ),
      list(
        forecaster = "scaled_pop",
        trainer = "linreg",
        pop_scaling = FALSE,
        lags = c(0, 3, 5, 7, 14)
      )
    ),
  )
}

# TODO: Find a way to clean all this stuff about param grids up.
param_grid <- append(
  make_shared_grids(),
  make_unique_grids()
) %>%
  map(add_id) %>%
  bind_rows() %>%
  relocate(parent_id, id, .after = last_col())
if (length(param_grid$id %>% unique()) < length(param_grid$id)) {
  abort("there are non-unique forecasters")
}
forecaster_parent_id_map <- param_grid %>%
  group_by(parent_id) %>%
  summarize(
    forecast_component_ids = list(syms(paste0(ONE_AHEAD_FORECAST_NAME, "_", gsub(" ", ".", id, fixed = TRUE)))),
    score_component_ids = list(syms(paste0(ONE_AHEAD_SCORE_NAME, "_", gsub(" ", ".", id, fixed = TRUE))))
  )
targets_param_grid <- make_target_param_grid(param_grid) %>%
  ## TODO This forecaster is hanging. Filter it out for now.
  filter(id != "necessary endless 5")

# not actually used downstream, this is for lookup during plotting and human evaluation
forecaster_params_grid_target <- list(
  tar_target(
    name = forecaster_params_grid,
    command = {
      param_grid
    },
    priority = 0.99
  )
)

# moving on to the ensemble
AHEADS <- 1:4
ensemble_grid <- add_row(
  make_shared_ensembles(),
  make_unique_ensemble_grid()
) %>% id_ahead_ensemble_grid(AHEADS)
# bind them together and give static ids
target_ensemble_grid <- make_target_ensemble_grid(ensemble_grid)
ensemble_parent_id_map <- ensemble_grid %>%
  group_by(parent_id) %>%
  summarize(
    ensemble_component_ids = list(syms(paste0(ONE_AHEAD_ENSEMBLE_NAME, "_", gsub(" ", ".", id, fixed = TRUE)))),
    score_component_ids = list(syms(paste0(ONE_AHEAD_SCORE_NAME, "_", gsub(" ", ".", id, fixed = TRUE))))
  )
# check that every ensemble dependent is actually included
missing_forecasters <- ensemble_missing_forecasters_details(ensemble_grid, param_grid)
if (length(missing_forecasters) > 0) {
  print("missing forecasters:")
  print(glue::glue("{missing_forecasters}"))
  rlang::abort(c("ensemble missing forecasters"))
}
# not actually used downstream, this is for lookup during plotting and human evaluation
ensembles_params_grid_target <- list(
  tar_target(
    name = ensemble_forecasters,
    command = {
      ensemble_grid
    },
    priority = 0.99
  )
)

# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
hhs_signal <- "confirmed_admissions_covid_1d"
chng_signal <- "smoothed_adj_outpatient_covid"
eval_time <- epidatr::epirange(from = "2020-01-01", to = "2024-01-01")
training_time <- epidatr::epirange(from = "2021-01-01", to = "2023-06-01")
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
data_targets <- make_data_targets()


# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
date_step <- 1L
forecasts_and_scores_by_ahead <- make_forecasts_and_scores_by_ahead()
forecasts_and_scores <- make_forecasts_and_scores()
# ensembles

# do a tar_map or tar_target to asdf
ensembles_and_scores_by_ahead <- tar_map(
  values = target_ensemble_grid,
  names = id,
  tar_target(
    name = ensemble_by_ahead,
    command = {
      ensemble(joined_archive_data_2022,
        forecaster_ids,
        "hhs",
        extra_sources = "chng",
        ensemble_params,
        ensemble_params_names
      )
    },
    priority = .9999
  ),
  tar_target(
    name = score_by_ahead,
    command = {
      run_evaluation_measure(
        data = ensemble_by_ahead,
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
ensembles_and_scores <- make_ensemble_targets_and_scores()
# other sources
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
external_names_and_scores <- make_external_names_and_scores()

list(
  data_targets,
  forecaster_params_grid_target,
  forecasts_and_scores_by_ahead,
  forecasts_and_scores,
  ensembles_params_grid_target,
  ensembles_and_scores_by_ahead,
  ensembles_and_scores,
  external_names_and_scores
)
