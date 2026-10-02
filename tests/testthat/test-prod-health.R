suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("prod_ensemble_coverage separates by-design drops from missing components", {
  fd <- as.Date("2026-04-01")
  mk <- function(f, geo, ahead_wk) {
    tibble(forecaster = f, geo_value = geo, forecast_date = fd, target_end_date = fd + 7 * ahead_wk + 3, quantile = 0.5, value = 1)
  }
  # windowed_seasonal is dropped at h−1 by design; revision_aware carries h−1 but
  # is missing for tx; tx's h−1 is still submitted (climate_linear carried it).
  forecasts <- bind_rows(mk("windowed_seasonal", c("ca", "tx"), -1), mk("windowed_seasonal", c("ca", "tx"), 0), mk("revision_aware", "ca", -1))
  clim_lin <- bind_rows(mk("climate_linear", c("ca", "tx"), -1), mk("climate_linear", c("ca", "tx"), 0))
  submitted <- bind_rows(mk("ensemble_mix", c("ca", "tx"), -1), mk("ensemble_mix", "ca", 0))
  weights <- tibble(
    forecast_date = fd, forecaster = rep(c("windowed_seasonal", "climate_linear", "revision_aware"), 2),
    geo_value = rep(c("ca", "tx"), each = 3), ahead = rep(c(NA, NA, -1), 2), weight = rep(c(1, 0.001, 1), 2)
  )
  spec <- list(
    components = list(nhsn = c("windowed_seasonal", "revision_aware")),
    drop_negative_aheads = list(nhsn = TRUE), drop_negative_aheads_exempt = list(nhsn = "revision_aware")
  )
  cov <- prod_ensemble_coverage(forecasts, clim_lin, submitted, weights, spec, "climate_linear", "nhsn", -1:0)
  missing <- cov$components %>% filter(!present)
  expect_equal(missing$forecaster, "revision_aware")
  expect_equal(missing$geo_value, "tx")
  expect_false(any(cov$components$forecaster == "windowed_seasonal" & cov$components$ahead == -1))
  expect_equal(cov$submission %>% filter(!submitted) %>% select(geo_value, ahead), tibble(geo_value = "tx", ahead = 0L))
  status <- prod_health_status(list(nhsn = cov))
  # tx h0 is half of h0's locations, over the 25% fail threshold.
  expect_equal(status$status, "fail")
})
