test_that("target param generation works", {
  ex_frame <- tribble(
    ~forecaster, ~id, ~someNAs, ~someNULLs,
    "scaled_pop", "linreg", TRUE, 35,
    "scaled_pop", "linreg", NA, 35,
    "scaled_pop", "linreg", FALSE, NULL,
  )
  list_version <- list(
    list(someNAs = TRUE, someNULLs = 35),
    list(someNULLs = 35),
    list(someNAs = FALSE)
  )
  expect_equal(lists_of_real_values(ex_frame), list_version)
})
param_grid$pop_scaling
NA
lists_of_real_values(param_grid)
