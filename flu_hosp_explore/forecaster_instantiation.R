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
param_grid <- bind_rows(map(grids, add_id)) %>%
  relocate(parent_id, id, .after = last_col())

forecaster_param_grids <- make_target_param_grid(param_grid) %>%
  ## TODO This forecaster is hanging. Filter it out for now.
  filter(id != "necessary endless 5")

ONE_AHEAD_FORECAST_NAME <- "forecast_by_ahead"
ONE_AHEAD_SCORE_NAME <- "score_by_ahead"
forecaster_parent_id_map <- param_grid %>%
  group_by(parent_id) %>%
  summarize(
    forecast_component_ids = list(syms(paste0(ONE_AHEAD_FORECAST_NAME, "_", gsub(" ", ".", id, fixed = TRUE)))),
    score_component_ids = list(syms(paste0(ONE_AHEAD_SCORE_NAME, "_", gsub(" ", ".", id, fixed = TRUE))))
  )

forecaster_param_grids <- make_target_param_grid(select(param_grid, -parent_id))

# not actually used downstream, this is for lookup during plotting and human evaluation
forecaster_targets <- list(
  tar_target(
    name = forecasters,
    command = {
      param_grid
    }
  )
)
