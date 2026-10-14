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
    "windowed_seasonal_extra_sources", "climate_base", "climate_geo_agged",
    "seasonal_nssp_cheating", "revision_aware"
  )
  covid_ids <- c(
    "cdc_baseline", "linear", "linear_no_population_scale", "windowed_seasonal",
    "windowed_seasonal_extra_sources", "climate_base", "climate_geo_agged",
    "windowed_seasonal_latest", "seasonal_nssp_latest", "revision_aware"
  )
  date_int <- as.integer(as.Date("2026-01-07"))
  files <- list(
    list(f = here::here("pipelines", "flu_geo_exclusions.csv"), ids = flu_ids),
    list(f = here::here("pipelines", "flu_nssp_geo_exclusions.csv"), ids = flu_ids),
    list(f = here::here("pipelines", "covid_geo_exclusions.csv"), ids = covid_ids),
    list(f = here::here("pipelines", "covid_nssp_geo_exclusions.csv"), ids = covid_ids)
  )
  for (x in files) {
    expect_no_error(parse_prod_weights(x$f, date_int, x$ids))
  }
})

merge_date <- as.Date("2026-10-14")
merge_geos <- c("ca", "ny")
merge_ids <- c("windowed_seasonal", "revision_aware")
mk_block <- function(forecaster, geo_value, ahead, weight) {
  tibble::tibble(forecast_date = merge_date, forecaster, geo_value, ahead, weight)
}
merged_weight <- function(merged, forecaster, geo_value, ahead) {
  keep <- merged$forecaster == forecaster & merged$geo_value == geo_value &
    (if (is.na(ahead)) is.na(merged$ahead) else merged$ahead %in% ahead)
  merged$weight[keep]
}

test_that("merge_geo_weights: a geo row with no ahead replaces the ahead-specific all-geo rows", {
  block <- mk_block(
    forecaster = c("revision_aware", "revision_aware", "revision_aware"),
    geo_value = c("all", "all", "ca"),
    ahead = c(-1, 0, NA),
    weight = c(1, 0.1, 5000)
  )
  merged <- merge_geo_weights(block, merge_ids, merge_geos)
  ca_rows <- dplyr::filter(merged, geo_value == "ca")
  expect_equal(nrow(ca_rows), 1)
  expect_equal(merged_weight(merged, "revision_aware", "ca", NA), 5000)
  expect_equal(merged_weight(merged, "revision_aware", "ny", -1), 1)
  expect_equal(merged_weight(merged, "revision_aware", "ny", 0), 0.1)
})

test_that("merge_geo_weights: a geo row with an ahead replaces only that ahead", {
  block <- mk_block(
    forecaster = c("windowed_seasonal", "windowed_seasonal", "windowed_seasonal"),
    geo_value = c("all", "all", "ca"),
    ahead = c(NA, 1, 1),
    weight = c(1, 2, 7)
  )
  merged <- merge_geo_weights(block, merge_ids, merge_geos)
  expect_equal(merged_weight(merged, "windowed_seasonal", "ca", NA), 1)
  expect_equal(merged_weight(merged, "windowed_seasonal", "ca", 1), 7)
  expect_equal(merged_weight(merged, "windowed_seasonal", "ny", 1), 2)
})

test_that("merge_geo_weights: an `all` forecaster geo row zeroes every forecaster at every ahead", {
  block <- mk_block(
    forecaster = c("windowed_seasonal", "revision_aware", "all"),
    geo_value = c("all", "all", "ca"),
    ahead = c(NA, -1, NA),
    weight = c(1, 1, 0)
  )
  merged <- merge_geo_weights(block, merge_ids, merge_geos)
  ca_rows <- dplyr::filter(merged, geo_value == "ca")
  expect_setequal(ca_rows$forecaster, merge_ids)
  expect_true(all(is.na(ca_rows$ahead)))
  expect_true(all(ca_rows$weight == 0))
})

test_that("merge_geo_weights works on a block without an ahead column", {
  block <- dplyr::select(mk_block(c("windowed_seasonal", "windowed_seasonal"), c("all", "ca"), NA, c(1, 3)), -ahead)
  merged <- merge_geo_weights(block, merge_ids, merge_geos)
  expect_equal(merged$weight[merged$geo_value == "ca"], 3)
  expect_equal(merged$weight[merged$geo_value == "ny"], 1)
})

test_that("stamp_default_weights stamps a file that ends with the defaults block", {
  weights_file <- withr::local_tempfile(fileext = ".csv")
  defaults <- c(
    "forecast_date,forecaster,geo_value,ahead,weight",
    "# defaults",
    "2024-10-01, linear, all,, 3",
    "2024-10-01, climate_base, all,, 2"
  )
  writeLines(defaults, weights_file)

  stamp_default_weights(weights_file, as.Date("2026-10-14"))

  expect_identical(
    readLines(weights_file),
    c(
      defaults,
      "##################",
      "# Oct 14",
      "##################",
      "2026-10-14, linear, all,, 3",
      "2026-10-14, climate_base, all,, 2"
    )
  )
})
