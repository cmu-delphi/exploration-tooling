suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# fmt: skip
weekly_rows <- function(...) {
  tribble(~geo_value, ~time_value, ~version, ~value, ...) %>%
    mutate(across(c(time_value, version), as.Date))
}

test_that("spoof_backfill_versions moves only bulk-loaded first vintages", {
  # fmt: skip
  input <- weekly_rows(
    # Bulk-loaded years later, then revised once.
    "us", "2010-10-03", "2018-10-05", 1.0,
    "us", "2010-10-03", "2018-10-12", 1.1,
    # Real-time report six days after the week ends.
    "us", "2020-10-04", "2020-10-16", 2.0,
    "us", "2020-10-04", "2020-11-20", 2.1
  )
  result <- spoof_backfill_versions(input) %>% arrange(time_value, version)
  expect_equal(
    result$version,
    as.Date(c("2010-10-03", "2018-10-12", "2020-10-16", "2020-11-20"))
  )
  expect_equal(result$value, input$value)
})

test_that("spoof_backfill_versions uses the end of the week for the lag", {
  # Week of 2020-10-04 ends 2020-10-10; 28 days later is 2020-11-07.
  # fmt: skip
  input <- weekly_rows(
    "ca", "2020-10-04", "2020-11-07", 1,
    "tx", "2020-10-04", "2020-11-08", 1
  )
  result <- spoof_backfill_versions(input)
  expect_equal(result$version, as.Date(c("2020-11-07", "2020-10-04")))
})

test_that("use_ny_minus_nyc keeps only the region the lab data has", {
  # fmt: skip
  input <- weekly_rows(
    "ny", "2020-10-04", "2020-10-16", 1,
    "nyc", "2020-10-04", "2020-10-16", 2,
    "ny_minus_nyc", "2020-10-04", "2020-10-16", 3,
    "ca", "2020-10-04", "2020-10-16", 4
  )
  result <- use_ny_minus_nyc(input)
  expect_equal(result$geo_value, c("ny", "ca"))
  expect_equal(result$value, c(3, 4))
})

test_that("combine_ili_plus uses the latest vintage of each input at each version", {
  # fmt: skip
  wili <- weekly_rows(
    "us", "2020-10-04", "2020-10-16", 2,
    "us", "2020-10-04", "2020-10-30", 4
  )
  # fmt: skip
  positivity <- weekly_rows(
    "us", "2020-10-04", "2020-10-23", 50
  )
  result <- combine_ili_plus(wili, positivity) %>% arrange(version)
  expect_equal(result$version, as.Date(c("2020-10-23", "2020-10-30")))
  expect_equal(result$hhs, c(1, 2))
})

test_that("read_fluview_positivity_csv maps regions and stops before the cutover", {
  # The FluView CSVs are not in git, so CI does not have them.
  skip_on_ci()
  cutover <- as.Date("2016-10-02")
  result <- read_fluview_positivity_csv(before = cutover)
  expect_true(all(result$time_value < cutover))
  expect_true(all(result$version == result$time_value))
  expect_true(all(c("us", as.character(1:10), "ca", "ny", "dc", "pr") %in% result$geo_value))
  expect_false(any(is.na(result$geo_value)))
  expect_false(anyDuplicated(result[c("geo_value", "time_value")]) > 0)
  expect_true(all(weekdays(result$time_value) == "Sunday"))
})
