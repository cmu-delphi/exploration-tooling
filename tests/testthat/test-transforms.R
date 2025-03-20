source(here::here("R", "load_all.R"))

n_days <- 20
removed_date <- 10
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = n_days)
simple_dates <- simple_dates[-removed_date]
rand_vals <- rnorm(n_days - 1)

# Two states, with 2 variables. a is linear, going up in one state and down in the other
# b is just random
# note that day 10 is missing
epi_data <- epiprocess::as_epi_df(rbind(tibble(
  geo_value = "al",
  time_value = simple_dates,
  a = 1:(n_days - 1),
), tibble(
  geo_value = "ca",
  time_value = simple_dates,
  a = (n_days - 1):1,
)))

test_that("rolling_mean generates correct mean", {
  rolled <- rolling_mean(epi_data)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "slide_a_m7"))

  # hand specified rolling mean with a rear window of 7
  expected_mean <- c(
    rep(NA, 6), 4:6, rep(NA, 6), 13:16,
    rep(NA, 6), 16:14, rep(NA, 6), 7:4
  )
  expect_equal(rolled %>% pull(slide_a_m7), expected_mean)

  expect_true("epi_df" %in% class(rolled))
})

test_that("rolling_mean generates correct mean for several widths", {
  skip("TODO: fix broken test, rolling_mean can't take a vector of widths")
  rolled <- rolling_mean(epi_data, width = c(3, 7))
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "slide_a_m3", "slide_a_m7"))

  # hand specified rolling mean with a rear window of 7
  expected_mean_7 <- c(
    rep(NA, 6), 4:6, rep(NA, 6), 13:16,
    rep(NA, 6), 16:14, rep(NA, 6), 7:4
  )
  expect_equal(rolled %>% pull(slide_a_m7), expected_mean_7)
  expected_mean_3 <- c(
    rep(NA, 2), 2:8, rep(NA, 2), 11:18,
    rep(NA, 2), 18:12, rep(NA, 2), 9:2
  )
  expect_equal(rolled %>% pull(slide_a_m3), expected_mean_3)

  expect_true("epi_df" %in% class(rolled))
})

test_that("rolling_sd generates correct standard deviation", {
  rolled <- rolling_sd(epi_data, sd_width = 4)
  rolled
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "slide_a_sd4"))
  # hand specified rolling mean with a rear window of 7, noting that mean(1:14) = 7.5
  expected_sd <- c(
    rep(NA, 4), rep(0.5, 5), rep(NA, 4), rep(0.5, 6),
    rep(NA, 4), rep(0.5, 5), rep(NA, 4), rep(0.5, 6)
  )
  expect_equal(rolled %>% pull(slide_a_sd4), expected_sd)

  # doesn't break types
  expect_true("epi_df" %in% class(rolled))
})

test_that("get_trainable_names pulls out mean and sd columns", {
  rolled <- rolling_sd(epi_data, keep_mean = TRUE)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "slide_a_m15", "slide_a_sd29"))
  expect_equal(get_trainable_names(rolled, NULL), c("a"))
})
# TODO example with NA's, example with missing days, only one column, keep_mean

test_that("rolling_sd doesn't keep the mean columns by default", {
  rolled <- rolling_sd(epi_data)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "slide_a_sd29"))
})
