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
  simple_ex <- list(tribble(
    ~forecaster, ~trainer, ~ahead, ~pop_scaling, ~lags,
    "scaled_pop", "linreg", 1, TRUE, c(1,2)
  ), tribble(
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
  list_ahead_scrambled <- list(trainer = "linreg", ahead = 1, pop_scaling = TRUE, forecaster = "scaled_pop")
  expect_equal(single_id(list_ahead_scrambled), single_id(list_ahead_ex))
  # the list version returns the same whether ahead is included
  list_ex <- list(forecaster = "scaled_pop", trainer = "linreg", pop_scaling = TRUE)
  expect_equal(single_id(list_ahead_ex), single_id(list_ex, 1))
})
