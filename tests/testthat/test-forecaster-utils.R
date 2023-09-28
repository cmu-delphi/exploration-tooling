test_that("perform_sanity_checks", {
  epi_data <- case_death_rate_subset
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
