source(here::here("R", "load_all.R"))

test_that("extend_ahead", {
  # testing that POSIXct converts correctly (as well as basic types)
  expect_no_error(epidataAhead <- extend_ahead(epipredict::case_death_rate_subset, 1))
  epi_data <- epidataAhead[[1]]
  effective_ahead <- epidataAhead[[2]]
  expect_identical(epi_data, epipredict::case_death_rate_subset)
  expect_type(effective_ahead, "integer")

  # testing the date math works correctly
  jhu <- epipredict::case_death_rate_subset %>%
    dplyr::filter(time_value >= as.Date("2021-12-01"))
  # the as_of for this is wildly far in the future
  attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
  epidataAhead <- extend_ahead(jhu, 1)
  epi_data <- epidataAhead[[1]]
  effective_ahead <- epidataAhead[[2]]
  expect_identical(epi_data, jhu)
  # ahead = 1 and compensating for 3 days latency
  expect_identical(effective_ahead, 4L)
})
