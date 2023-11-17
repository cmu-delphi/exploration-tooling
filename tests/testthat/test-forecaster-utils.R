test_that("perform_sanity_checks", {
  epi_data <- epipredict::case_death_rate_subset
  # don't need to test validate_forecaster_inputs as that's inherited
  # testing args_list inheritance
  ex_args <- arx_args_list()
  expect_error(perform_sanity_checks(epi_data, "case_rate", c("case_rate"), 5, ex_args))
  argsPredictors <- perform_sanity_checks(
    epi_data, "case_rate", c("case_rate", ""), parsnip::linear_reg(), ex_args
  )
  args_list <- argsPredictors[[1]]
  predictors <- argsPredictors[[2]]
  expect_equal(predictors, c("case_rate"))
})

test_that("id generation works", {
  simple_ex <- list(dplyr::tribble(
    ~forecaster, ~trainer, ~ahead, ~pop_scaling, ~lags,
    "scaled_pop", "linreg", 1, TRUE, c(1,2)
  ), dplyr::tribble(
    ~forecaster, ~ahead, ~pop_scaling, ~trainer, ~lags,
    "scaled_pop", 1, TRUE, "linreg", c(1,2)
  ))
  same_ids <- map(simple_ex, add_id)
  # the order shouldn't matter for the names
  expect_equal(same_ids[[1]]$id, same_ids[[2]]$id)
  # the list version and the
  list_ahead_ex <- list(forecaster = "scaled_pop", trainer = "linreg", ahead = 1, pop_scaling = TRUE, lags = c(1,2))
  expect_equal(single_id(list_ahead_ex), same_ids[[1]]$id)
  # order shouldn't matter for the list either
  list_ahead_scrambled <- list(lags = c(1,2), trainer = "linreg", ahead = 1, pop_scaling = TRUE, forecaster = "scaled_pop")
  expect_equal(single_id(list_ahead_scrambled), single_id(list_ahead_ex))
  # the list version returns the same whether ahead is included
  list_ex <- list(forecaster = "scaled_pop", trainer = "linreg", pop_scaling = TRUE, lags = c(1,2))
  expect_equal(single_id(list_ahead_ex), single_id(list_ex, 1))
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

test_that("ensemble_check catches missing examples", {
  param_grid_ex <- tibble(
    forecaster = rep("scaled_pop", 5),
    ahead = c(1, 2, 3, 4, 5),
    pop_scale = c(rep(FALSE, 4), TRUE),
    lags = list(NULL, NULL, NULL, NULL, c(0, 7, 14)),
    id = c("unexpected.criminological.1", "unexpected.criminological.3", "unexpected.criminological.2", "unexpected.criminological.4", "vain.intrapsychic.5")
  )
  ensemble_ex <- tibble(
    ensemble = c("present", "absent"),
    forecasters = list(list(forecaster = "scaled_pop", trainer = "linreg"), list(forecaster = "scaled_pop", trainer = "linreg")),
    ensemble_params = list(list(someThing = TRUE), list(someThing = TRUE)),
    ahead = c(1,3),
    forecaster_ids = list(list("unexpected.criminological.1", "vain.intrapsychic.5"), list("unexpected.criminological.5"))
  )
  expect_identical(ensemble_missing_forecasters(ensemble_ex, param_grid_ex), "unexpected.criminological.5")
})
