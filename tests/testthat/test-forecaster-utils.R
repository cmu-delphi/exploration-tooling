source(here::here("R", "load_all.R"))

test_that("sanitize_args_predictors_trainer", {
  epi_data <- epipredict::case_death_rate_subset
  # don't need to test validate_forecaster_inputs as that's inherited
  # testing args_list inheritance
  ex_args <- arx_args_list()
  expect_error(sanitize_args_predictors_trainer(epi_data, "case_rate", c("case_rate"), 5, ex_args))
  argsPredictors <- sanitize_args_predictors_trainer(
    epi_data, "case_rate", c("case_rate", ""), parsnip::linear_reg(), ex_args
  )
  args_list <- argsPredictors[[1]]
  predictors <- argsPredictors[[2]]
  expect_equal(predictors, c("case_rate"))
})

test_that("id generation works", {
  # Same arguments but scrambled.
  simple_ex <- list(
    dplyr::tribble(
      ~forecaster, ~trainer, ~pop_scaling, ~lags,
      "scaled_pop", "linreg", TRUE, c(1, 2)
    ), dplyr::tribble(
      ~forecaster, ~pop_scaling, ~trainer, ~lags,
      "scaled_pop", TRUE, "linreg", c(1, 2)
    ), dplyr::tribble(
      ~trainer, ~forecaster, ~pop_scaling,
      "linreg", "scaled_pop", TRUE,
    ), dplyr::tribble(
      ~trainer, ~pop_scaling, ~forecaster,
      "linreg", TRUE, "scaled_pop",
    )
  )
  same_ids <- map(simple_ex, add_id)
  expect_equal(same_ids[[1]]$id, same_ids[[2]]$id)
  expect_equal(same_ids[[3]]$id, same_ids[[4]]$id)
})

test_that("forecaster lookup selects the right rows", {
  param_grid_ex <- tibble(
    forecaster = rep("scaled_pop", 5),
    ahead = c(1, 2, 3, 4, 5),
    pop_scale = c(rep(FALSE, 4), TRUE),
    lags = list(NULL, NULL, NULL, NULL, c(0, 7, 14)),
    id = c("unexpected.criminological.1", "unexpected.criminological.3", "unexpected.criminological.2", "unexpected.criminological.4", "vain.intrapsychic.5")
  )
  expect_equal(forecaster_lookup("score_unexpected.criminological", param_grid_ex), tibble(
    forecaster = rep("scaled_pop", 4),
    ahead = c(1, 2, 3, 4),
    pop_scale = rep(FALSE, 4),
    lags = list(NULL, NULL, NULL, NULL),
    id = c("unexpected.criminological.1", "unexpected.criminological.3", "unexpected.criminological.2", "unexpected.criminological.4")
  ))
  expect_equal(forecaster_lookup("score_unexpected.criminological.1", param_grid_ex), tibble(
    forecaster = c("scaled_pop"),
    ahead = c(1),
    pop_scale = c(FALSE),
    lags = list(NULL),
    id = c("unexpected.criminological.1")
  ))
})

test_that("strip_underscored drops 1 or several", {
  expect_identical(strip_underscored("forecast_by_ahead_unexpected.criminological.1"), "unexpected.criminological.1")
  expect_identical(strip_underscored("score_unexpected.criminological"), "unexpected.criminological")
})
