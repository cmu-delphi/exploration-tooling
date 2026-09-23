suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# parse_prod_weights fetches all_states over the network; these tests exercise
# the two validation helpers it calls directly instead, so they run offline
# and pin down exactly what the schema check accepts/rejects.

forecaster_ids <- c("linear", "windowed_seasonal", "climate_base")
geos <- c("pa", "wy", "us", "usa")

valid_weights <- tibble::tribble(
  ~forecast_date, ~forecaster, ~geo_value, ~weight,
  as.Date("2026-01-07"), "linear", "all", 3,
  as.Date("2026-01-07"), "windowed_seasonal", "pa", 0.5,
  as.Date("2026-01-07"), "all", "wy", 0,
  as.Date("2026-01-07"), "linearlog", "all", 0, # legacy id, always inert
)

test_that("validate_prod_weights_values passes a well-formed weights table", {
  expect_no_error(validate_prod_weights_values(valid_weights, "test.csv", forecaster_ids, geos))
})

test_that("validate_prod_weights_values aborts on an unknown forecaster id", {
  bad <- valid_weights
  bad$forecaster[1] <- "linaer" # typo
  expect_error(
    validate_prod_weights_values(bad, "test.csv", forecaster_ids, geos),
    "unknown forecaster id"
  )
})

test_that("validate_prod_weights_values aborts on a negative weight", {
  bad <- valid_weights
  bad$weight[1] <- -1
  expect_error(
    validate_prod_weights_values(bad, "test.csv", forecaster_ids, geos),
    "negative `weight`"
  )
})

test_that("validate_prod_weights_values aborts on a non-finite weight", {
  bad <- valid_weights
  bad$weight[1] <- NA_real_
  expect_error(
    validate_prod_weights_values(bad, "test.csv", forecaster_ids, geos),
    "non-numeric or non-finite"
  )
})

test_that("validate_prod_weights_values aborts on an unknown geo_value", {
  bad <- valid_weights
  bad$geo_value[2] <- "zz"
  expect_error(
    validate_prod_weights_values(bad, "test.csv", forecaster_ids, geos),
    "unknown `geo_value`"
  )
})

test_that("validate_prod_weights_columns passes a well-formed raw table", {
  raw <- valid_weights
  expect_no_error(validate_prod_weights_columns(raw, "test.csv"))
})

test_that("validate_prod_weights_columns aborts on a missing column", {
  raw <- dplyr::select(valid_weights, -weight)
  expect_error(
    validate_prod_weights_columns(raw, "test.csv"),
    "missing required column"
  )
})

test_that("validate_prod_weights_columns aborts on an unparseable forecast_date", {
  raw <- valid_weights
  raw$forecast_date <- as.character(raw$forecast_date)
  raw$forecast_date[1] <- "not-a-date"
  expect_error(
    validate_prod_weights_columns(raw, "test.csv"),
    "unparseable `forecast_date`"
  )
})

test_that("all four hand-edited prod weights csvs pass schema validation", {
  flu_ids <- c(
    "cdc_baseline", "linear", "linear_no_population_scale", "windowed_seasonal",
    "windowed_seasonal_extra_sources", "climate_base", "climate_geo_agged", "seasonal_nssp_cheating"
  )
  covid_ids <- c(
    "cdc_baseline", "linear", "linear_no_population_scale", "windowed_seasonal",
    "windowed_seasonal_extra_sources", "climate_base", "climate_geo_agged",
    "windowed_seasonal_latest", "seasonal_nssp_latest"
  )
  date_int <- as.integer(as.Date("2026-01-07"))
  files <- list(
    list(f = here::here("scripts", "flu_geo_exclusions.csv"), ids = flu_ids),
    list(f = here::here("scripts", "flu_nssp_geo_exclusions.csv"), ids = flu_ids),
    list(f = here::here("scripts", "covid_geo_exclusions.csv"), ids = covid_ids),
    list(f = here::here("scripts", "covid_nssp_geo_exclusions.csv"), ids = covid_ids)
  )
  for (x in files) {
    expect_no_error(parse_prod_weights(x$f, date_int, x$ids))
  }
})
