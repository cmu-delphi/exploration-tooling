suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("ensemble_weighted renormalizes over the components present at each ahead", {
  fd <- as.Date("2026-04-01")
  mk <- function(f, ahead_wk, v) {
    tibble(
      forecaster = f, geo_value = "ca", forecast_date = fd,
      target_end_date = fd + 7 * ahead_wk + 3, quantile = 0.5, value = v
    )
  }
  # revision_aware carries h−1 but has no h−1 forecast this round (a reporting
  # gap); it still forecasts h0, so it is present for the geo overall.
  forecasts <- bind_rows(mk("climate_linear", c(-1, 0), 100), mk("revision_aware", 0, 50))
  weights <- tibble(
    forecast_date = fd, forecaster = c("climate_linear", "revision_aware"), geo_value = "ca",
    ahead = c(NA, -1), weight = c(0.001, 1)
  )
  out <- ensemble_weighted(forecasts, weights)
  expect_equal(out$value, c(100, 100))
})
