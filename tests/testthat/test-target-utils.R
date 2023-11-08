test_that("target param generation works", {
  ex_frame <- tribble(
    ~forecaster, ~id, ~someNAs, ~someNULLs, ~someList, ~someYoDog,
    "scaled_pop", "linreg", TRUE, 35, c(1, 3, 5), list(c(1, 2), c(3, 4)),
    "scaled_pop", "linreg", NA, 35, c(1, 3, 5), list(c(1, 2), c(3, 4)),
    "scaled_pop", "linreg", FALSE, NULL, c(1, 3, 5), list(c(1, 2), c(3, 4))
  )
  list_version <- list(
    list(someNAs = TRUE, someNULLs = 35, someList = c(1, 3, 5), someYoDog = list(c(1, 2), c(3, 4))),
    list(someNULLs = 35, someList = c(1, 3, 5), someYoDog = list(c(1, 2), c(3, 4))),
    list(someNAs = FALSE, someList = c(1, 3, 5), someYoDog = list(c(1, 2), c(3, 4)))
  )
  expect_equal(lists_of_real_values(ex_frame), list_version)
})
