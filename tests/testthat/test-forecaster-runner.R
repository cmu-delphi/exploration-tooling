suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

valid_forecast <- function() {
  tidyr::expand_grid(
    geo_value = c("ca", "tx"),
    target_end_date = as.Date(c("2024-11-23", "2024-11-30")),
    quantile = c(0.25, 0.5, 0.75)
  ) %>%
    mutate(
      forecast_date = as.Date("2024-11-20"),
      value = quantile * 100
    )
}

test_that("validate_forecast_output passes a well-formed forecast through", {
  fc <- valid_forecast()
  expect_identical(validate_forecast_output(fc, "test"), fc)
})

test_that("validate_forecast_output passes the empty insufficient-data result", {
  null_result <- tibble(
    geo_value = character(),
    forecast_date = lubridate::Date(),
    target_end_date = lubridate::Date(),
    quantile = numeric(),
    value = numeric()
  )
  expect_identical(validate_forecast_output(null_result, "test"), null_result)
})

test_that("validate_forecast_output catches missing columns", {
  expect_error(
    validate_forecast_output(valid_forecast() %>% select(-quantile), "test"),
    "missing column"
  )
})

test_that("validate_forecast_output catches NAs in required columns", {
  fc <- valid_forecast()
  fc$value[3] <- NA
  expect_error(validate_forecast_output(fc, "test"), "NA values")
})

test_that("validate_forecast_output catches negative values", {
  fc <- valid_forecast()
  fc$value[1] <- -1
  expect_error(validate_forecast_output(fc, "test"), "negative value")
})

test_that("validate_forecast_output catches crossing quantiles per task", {
  fc <- valid_forecast()
  # cross one task's median below its 25th percentile; other tasks stay valid
  fc$value[fc$geo_value == "ca" & fc$target_end_date == as.Date("2024-11-23") & fc$quantile == 0.5] <- 1
  expect_error(validate_forecast_output(fc, "test"), "quantiles cross in 1 task")
})

test_that("validate_forecast_output allows ties across quantiles", {
  fc <- valid_forecast() %>% mutate(value = 0)
  expect_identical(validate_forecast_output(fc, "test"), fc)
})
