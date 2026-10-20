test_that("make_forecaster_grid gives each param the config cell value", {
  latent_locations <- list(list(c("source"), c("flusurv", "ILI+")))
  config <- tidyr::expand_grid(
    forecaster = "scaled_pop_seasonal",
    trainer = "quantreg",
    lags = list(list(c(0, 7), c(0, 7, 14))),
    extra_sources = list(c("nssp", "inpatient_beds_ew")),
    seasonal_method = list(list("window", "climatological")),
    aheads = list(c(0, 7, 14)),
    keys_to_ignore = latent_locations,
    pop_scaling = FALSE
  ) %>%
    add_id()

  params <- make_forecaster_grid(config, "family")$params[[1]]

  expect_identical(params$trainer, rlang::sym("quantreg"))
  expect_identical(params$lags, list(c(0, 7), c(0, 7, 14)))
  expect_identical(params$extra_sources, c("nssp", "inpatient_beds_ew"))
  expect_identical(params$seasonal_method, list("window", "climatological"))
  expect_identical(params$aheads, c(0, 7, 14))
  expect_identical(params$keys_to_ignore, latent_locations[[1]])
  expect_identical(params$pop_scaling, FALSE)
})

test_that("default_args_list decodes keys_to_ignore from the grid param", {
  args <- default_args_list(keys_to_ignore = list(c("source"), c("flusurv", "ILI+")))
  expect_identical(args$keys_to_ignore, list(source = c("flusurv", "ILI+")))
})
