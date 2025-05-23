suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("sanitize_args_predictors_trainer", {
  epi_data <- epidatasets::covid_case_death_rates
  # don't need to test validate_forecaster_inputs as that's inherited
  # testing args_list inheritance
  ex_args <- default_args_list()
  expect_error(sanitize_args_predictors_trainer(epi_data, "case_rate", c("case_rate"), 5, ex_args))
  argsPredictors <- sanitize_args_predictors_trainer(
    epi_data,
    "case_rate",
    c("case_rate", ""),
    parsnip::linear_reg(),
    ex_args
  )
  args_list <- argsPredictors[[1]]
  predictors <- argsPredictors[[2]]
  expect_equal(predictors, c("case_rate"))
})

test_that("id generation works", {
  # Same arguments but scrambled.
  # fmt: skip
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
  # Same as above, but direct calls into get_single_id
  for (i in 1:4) {
    expect_equal(simple_ex[[i]] %>% purrr::transpose() %>% pluck(1) %>% get_single_id(), same_ids[[i]]$id)
  }
})

test_that("forecaster lookup selects the right rows", {
  param_grid_ex <- tibble(
    id = c("simian.irishsetter", "monarchist.thrip"),
    forecaster = rep("scaled_pop", 2),
    lags = list(NULL, c(0, 7, 14)),
    pop_scale = c(FALSE, TRUE),
  )
  # fmt: skip
  expect_equal(forecaster_lookup("monarchist", param_grid_ex), tribble(
    ~id, ~forecaster, ~lags, ~pop_scale,
    "monarchist.thrip", "scaled_pop", c(0, 7, 14), TRUE,
  ))
  # fmt: skip
  expect_equal(forecaster_lookup("irish", param_grid_ex), tribble(
    ~id, ~forecaster, ~lags, ~pop_scale,
    "simian.irishsetter", "scaled_pop", NULL, FALSE,
  ))
})
