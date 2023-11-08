# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# where the forecasters and parameters are joined; see either the variable param_grid or `tar_read(forecasters)`
source("extras/targets-common.R")

# Add custom parameter combinations in the list below.
make_unique_grids <- function() {
  list(
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = c("linreg", "quantreg"),
      ahead = 1:4,
      pop_scaling = c(TRUE)
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = c("linreg", "quantreg"),
      ahead = 5:7,
      lags = list(c(0, 3, 5, 7, 14), c(0, 7, 14)),
      pop_scaling = c(TRUE)
    )
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
    }
  )
)


# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
hhs_signal <- "confirmed_admissions_covid_1d"
chng_signal <- "smoothed_adj_outpatient_covid"
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 200)
data_targets <- make_data_targets()


forecasts_and_scores_by_ahead <- make_forecasts_and_scores_by_ahead()
forecasts_and_scores <- make_forecasts_and_scores()
ensemble_targets <- make_ensemble_targets()
external_names_and_scores <- make_external_names_and_scores()


list(
  data_targets,
  forecaster_params_grid_target,
  forecasts_and_scores_by_ahead,
  forecasts_and_scores,
  ensemble_targets,
  external_names_and_scores
)
