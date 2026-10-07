suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("score_forecasts skips locations with no truth of their own", {
  fd <- as.Date("2026-02-07")
  forecasts <- tidyr::expand_grid(
    forecaster = "climate_linear", geo_value = c("ca", "ia"), forecast_date = fd,
    target_end_date = fd, quantile = c(0.05, 0.25, 0.5, 0.75, 0.95)
  ) %>%
    mutate(value = quantile)
  latest_data <- tibble(geo_value = "ca", time_value = fd, value = 0.5)
  scores <- score_forecasts(latest_data, forecasts, "wk inc flu prop ed visits")
  expect_equal(scores$geo_value, "ca")
})

test_that("score_forecasts scores a location only on dates where it has truth", {
  fd <- as.Date("2026-02-07")
  forecasts <- tidyr::expand_grid(
    forecaster = "climate_linear", geo_value = c("ca", "ia"), forecast_date = fd,
    target_end_date = fd + 7 * 0:2, quantile = c(0.05, 0.25, 0.5, 0.75, 0.95)
  ) %>%
    mutate(value = quantile)
  latest_data <- bind_rows(
    tibble(geo_value = "ca", time_value = fd + 7 * 0:2, value = 0.5),
    tibble(geo_value = "ia", time_value = fd + 7 * c(0, 2), value = 0.5)
  )
  scores <- score_forecasts(latest_data, forecasts, "wk inc flu prop ed visits")
  expect_equal(nrow(filter(scores, geo_value == "ca")), 3)
  expect_equal(sort(filter(scores, geo_value == "ia")$ahead), c(0L, 2L))
})
