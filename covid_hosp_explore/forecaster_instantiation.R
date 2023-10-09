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

forecaster_param_grids <- make_target_param_grid(param_grid)

# not actually used downstream, this is for lookup during plotting and human evaluation
forecasters <- list(
  tar_target(
    name = forecasters,
    command = {
      param_grid
    }
  )
)
