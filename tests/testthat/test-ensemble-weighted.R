suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

fd <- as.Date("2026-04-01")

mk_forecast <- function(forecaster, ahead_wk, value, geo_value = "ca", quantile = 0.5) {
  tidyr::expand_grid(
    forecaster = forecaster, geo_value = geo_value, ahead_wk = ahead_wk, quantile = quantile
  ) %>%
    transmute(
      forecaster, geo_value,
      forecast_date = fd, target_end_date = fd + 7 * ahead_wk + 3, quantile, value = value
    )
}

mk_weights <- function(forecaster, weight, geo_value = "ca", ahead = NA) {
  tibble(forecast_date = fd, forecaster = forecaster, geo_value = geo_value, ahead = ahead, weight = weight)
}

test_that("resolve_ensemble_weights sums to 1 over the components present at each (geo, ahead)", {
  forecasts <- bind_rows(
    mk_forecast("ar", 0:2, 10, geo_value = c("ca", "tx")),
    mk_forecast("climate", 0:2, 20, geo_value = c("ca", "tx")),
    # Missing at tx, ahead 2.
    mk_forecast("linear", 0:1, 30, geo_value = c("ca", "tx")),
    mk_forecast("linear", 2, 30, geo_value = "ca")
  )
  weights <- bind_rows(
    mk_weights(c("ar", "climate", "linear"), c(3, 1, 2), geo_value = "ca"),
    mk_weights(c("ar", "climate", "linear"), c(1, 1, 0), geo_value = "tx"),
    # Not among the forecasts.
    mk_weights("absent", 5, geo_value = c("ca", "tx"))
  )
  resolved <- resolve_ensemble_weights(forecasts, weights)

  present <- forecasts %>% add_week_ahead() %>% distinct(forecaster, geo_value, ahead)
  expect_equal(nrow(resolved), nrow(present))
  expect_equal(nrow(dplyr::anti_join(resolved, present, by = c("forecaster", "geo_value", "ahead"))), 0)
  mass <- resolved %>% group_by(geo_value, ahead) %>% summarize(mass = sum(weight), .groups = "drop")
  expect_equal(mass$mass, rep(1, nrow(mass)))
  expect_equal(
    resolved %>% filter(geo_value == "ca", ahead == 0) %>% arrange(forecaster) %>% pull(weight),
    c(3, 1, 2) / 6
  )
})

test_that("ensemble_weighted is the weighted mean of the components at each quantile", {
  forecasts <- bind_rows(
    mk_forecast("ar", 0:1, 10, quantile = c(0.25, 0.5, 0.75)),
    mk_forecast("climate", 0:1, 40, quantile = c(0.25, 0.5, 0.75))
  )
  weights <- mk_weights(c("ar", "climate"), c(3, 1))
  out <- ensemble_weighted(forecasts, weights)
  expect_equal(nrow(out), 6)
  expect_equal(out$value, rep((3 * 10 + 1 * 40) / 4, 6))
})

test_that("ensemble_weighted renormalizes over the components present at each ahead", {
  # revision_aware carries h−1 but has no h−1 forecast this round (a reporting
  # gap); it still forecasts h0, so it is present for the geo overall.
  forecasts <- bind_rows(mk_forecast("climate_linear", c(-1, 0), 100), mk_forecast("revision_aware", 0, 50))
  weights <- mk_weights(c("climate_linear", "revision_aware"), c(0.001, 1), ahead = c(NA, -1))
  out <- ensemble_weighted(forecasts, weights)
  expect_equal(out$value, c(100, 100))
})

test_that("ensemble_weighted renormalizes over the components present at each geo", {
  # linear forecasts ahead 1 for ca only, so it has a tx ahead-1 weight but no
  # tx ahead-1 forecast.
  forecasts <- bind_rows(
    mk_forecast("ar", 0:1, 10, geo_value = c("ca", "tx")),
    mk_forecast("linear", 0, 30, geo_value = "tx"),
    mk_forecast("linear", 0:1, 30, geo_value = "ca")
  )
  weights <- mk_weights(c("ar", "linear"), c(1, 1), geo_value = c("ca", "tx")) %>%
    tidyr::complete(forecaster, geo_value, fill = list(forecast_date = fd, weight = 1))
  out <- ensemble_weighted(forecasts, weights) %>% add_week_ahead()
  expect_equal(out$value[out$geo_value == "tx" & out$ahead == 1], 10)
  expect_equal(out$value[out$geo_value == "tx" & out$ahead == 0], 20)
  expect_equal(out$value[out$geo_value == "ca"], c(20, 20))
})

test_that("ensemble_weighted gives no weight to a component without a weights row", {
  forecasts <- bind_rows(mk_forecast("ar", 0, 10), mk_forecast("unweighted", 0, 1000))
  out <- ensemble_weighted(forecasts, mk_weights("ar", 2))
  expect_equal(out$value, 10)
})
