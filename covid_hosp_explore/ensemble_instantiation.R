ex_forecaster <-list(
      forecaster = "scaled_pop",
      trainer = "linreg",
      pop_scaling = FALSE,
      lags = c(0, 3, 5, 7, 14)
    )
# ensembles don't lend themselves to expand grid (inherently needs a list for sub-forecasters)
ensemble_grid <- tribble(
  ~ensemble, ~forecasters, ~ensemble_params,
  # mean forecaster
  "ensemble_average", list(
    ex_forecaster,
    list(forecaster = "flatline_fc")
  ),
  list(average_type = "mean"),
  # median forecaster
  "ensemble_average", list(
    ex_forecaster,
    list(forecaster = "flatline_fc")
  ),
  list(average_type = "median"),
)
# add aheads and ids (both forecasters and ensemble)
ensemble_grid <- id_ahead_ensemble_grid(ensemble_grid, 1:4)
ensemble_grid

# bind them together and give static ids
target_ensemble_grid <- make_target_ensemble_grid(ensemble_grid)

ONE_AHEAD_ENSEMBLE_NAME <- "ensemble_by_ahead"
ONE_AHEAD_SCORE_NAME <- "score_by_ahead"
ensemble_parent_id_map <- ensemble_grid %>%
  group_by(parent_id) %>%
  summarize(
    ensemble_component_ids = list(syms(paste0(ONE_AHEAD_ENSEMBLE_NAME, "_", gsub(" ", ".", id, fixed = TRUE)))),
    score_component_ids = list(syms(paste0(ONE_AHEAD_SCORE_NAME, "_", gsub(" ", ".", id, fixed = TRUE))))
  )

ensemble_list <- list(
  tar_target(
    name = ensemble_forecasters,
    command = {
      ensemble_grid
    }
  )
)
# TODO add method that guarantees the dependent forecasts are defined
