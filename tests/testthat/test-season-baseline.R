hub_forecasters <- c("RSVHub-baseline", "RSVHub-ensemble")

test_that("season_baseline_id keeps the hub baseline when the hub has scores", {
  scores <- tibble(forecaster = c("RSVHub-baseline", "some.forecaster"))
  expect_equal(season_baseline_id(scores, hub_forecasters, "flat.line"), "")
})

test_that("season_baseline_id falls back when the hub has no scores", {
  scores <- tibble(forecaster = c("some.forecaster", "flat.line"))
  expect_equal(season_baseline_id(scores, hub_forecasters, "flat.line"), "flat.line")
})

test_that("season_baseline_id ignores a hub ensemble without the hub baseline", {
  scores <- tibble(forecaster = c("RSVHub-ensemble", "flat.line"))
  expect_equal(season_baseline_id(scores, hub_forecasters, "flat.line"), "flat.line")
})

test_that("season_baseline_id keeps the default without a fallback", {
  scores <- tibble(forecaster = "some.forecaster")
  expect_equal(season_baseline_id(scores, hub_forecasters, NULL), "")
})
