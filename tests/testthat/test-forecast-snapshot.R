suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# A small archive with honest versions (version >= time_value) plus one row
# published ahead of its observation date, the way faux-versioned augmentation
# rows can be if an archive-construction bug stamps them into the future.
# fmt: skip
snapshot_test_archive <- function(rows = NULL) {
  tribble(
    ~geo_value, ~time_value, ~version, ~value,
    "ca", "2024-11-06", "2024-11-08", 1,
    "ca", "2024-11-13", "2024-11-15", 2,
    "ca", "2024-11-20", "2024-11-22", 3,
    "tx", "2024-11-06", "2024-11-08", 4,
    "tx", "2024-11-13", "2024-11-15", 5,
    "tx", "2024-11-20", "2024-11-22", 6
  ) %>%
    bind_rows(rows) %>%
    mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
    as_epi_archive(versions_end = as.Date("2024-12-01"), compactify = TRUE)
}

test_that("asof snapshot passes version faithfulness on honest data", {
  snapshot <- make_forecast_snapshot(
    snapshot_test_archive(),
    forecast_date = as.Date("2024-11-16"),
    generation_date = as.Date("2024-11-16")
  )
  expect_equal(max(snapshot$time_value), as.Date("2024-11-13"))
  expect_equal(attributes(snapshot)$metadata$as_of, as.Date("2024-11-16"))
})

test_that("asof snapshot aborts when a future observation leaks through", {
  # Published (version) before the generation date but observed (time_value)
  # after it: epix_as_of keeps the row, the faithfulness assert must not.
  leaky <- tibble(
    geo_value = "ca",
    time_value = "2024-11-27",
    version = "2024-11-10",
    value = 7
  )
  expect_error(
    make_forecast_snapshot(
      snapshot_test_archive(leaky),
      forecast_date = as.Date("2024-11-16"),
      generation_date = as.Date("2024-11-16")
    ),
    "time_value after generation_date"
  )
})

test_that("cheating snapshot keeps finalized values but not future time_values", {
  leaky <- tibble(
    geo_value = "ca",
    time_value = "2024-11-27",
    version = "2024-11-10",
    value = 7
  )
  snapshot <- make_forecast_snapshot(
    snapshot_test_archive(leaky),
    forecast_date = as.Date("2024-11-16"),
    generation_date = as.Date("2024-11-16"),
    as_of_policy = "cheating"
  )
  # finalized values (version past generation_date) are the point of cheating...
  expect_equal(snapshot %>% filter(time_value == as.Date("2024-11-13")) %>% pull(value), c(2, 5))
  # ...but rows dated at/after the generation date still must not appear
  expect_lt(max(snapshot$time_value), as.Date("2024-11-16"))
})

test_that("forecast_date after generation_date is rejected", {
  expect_error(
    make_forecast_snapshot(
      snapshot_test_archive(),
      forecast_date = as.Date("2024-11-20"),
      generation_date = as.Date("2024-11-16")
    ),
    "never precede"
  )
})
