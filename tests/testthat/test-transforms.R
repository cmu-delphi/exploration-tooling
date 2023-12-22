library(dplyr)
n_days <- 40
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
  b = rand_vals
), tibble(
  geo_value = "ca",
  time_value = simple_dates,
  a = (n_days - 1):1,
  b = rand_vals + 10
)))
test_that("rolling_mean generates correct mean", {
  rolled <- rolling_mean(epi_data)
  rolled
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "b", "a_m7", "b_m7"))
  # hand specified rolling mean with a rear window of 7, noting that mean(1:7) = 4
  linear_roll_mean <- c(seq(from = 1, by = .5, length.out = 7), seq(from = 5, to = 36, by = 1))
  # day 10 is missing, so the average days 11-16 are thrown off, only using 6 values instead of 7
  gap_starts <- epi_data %>% filter(geo_value == "al" & time_value == as.Date("2012-01-11")) %>% pull(a)
  unusual_days <- map_vec(seq(from = 0, to = 5), \(d) mean(((gap_starts + d) - 0):((gap_starts + d) - 5)))
  # stitching the lag induced hiccup into the "normal" mean values
  expected_mean <- c(linear_roll_mean[1:9], unusual_days, linear_roll_mean[16:(n_days - 1)])
  expected_mean

  expect_equal(rolled %>% filter(geo_value == "al") %>% pull("a_m7"), expected_mean)
  # Doing the same for California
  # same, but "ca" is reversed, noting mean(40:(40-7)) =36.5
  linear_reverse_roll_mean <- c(seq(from = 39, by = -0.5, length.out = 7), seq(from = 35, to = 4, by = -1))
  # day 10 is missing, so days 11-16 are thrown off
  gap_starts <- epi_data %>% filter(geo_value == "ca" & time_value == as.Date("2012-01-11")) %>% pull(a)
  unusual_days <- map_vec(seq(from = 0, to = 5), \(d) mean(((gap_starts - d) + 0):((gap_starts - d) + 5)))
  # stitching the lag induced hiccup into the "normal" mean values
  expected_mean <- c(linear_reverse_roll_mean[1:9], unusual_days, linear_reverse_roll_mean[16:(n_days - 1)])
  # actually testing
  expect_equal(rolled %>% filter(geo_value == "ca") %>% pull("a_m7"), expected_mean)

  # making sure the type is maintained
  expect_true("epi_df" %in% class(rolled))
})

test_that("rolling_sd generates correct standard deviation", {
  rolled <- rolling_sd(epi_data, keep_mean = TRUE)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "b", "a_m14", "a_sd28", "b_m14", "b_sd28"))
  # hand specified rolling mean with a rear window of 7, noting that mean(1:14) = 7.5
  linear_roll_mean <- c(seq(from = 1, to = 7, by = .5), seq(from = 8, to = 16, by = 1), seq(from = 16.5, to = 32.5, by = 1))
##   linear_roll_mean <- c(seq(from = 1, by = .5, length.out = 14), seq(from = 8.5, to = 32.5, by = 1))
##   gap_starts <- epi_data %>% filter(geo_value == "al" & time_value == as.Date("2012-01-11")) %>% pull(a)
##   unusual_days <- map_vec(seq(from = 0, to = 5), \(d) mean(((gap_starts + d) - 0):max((gap_starts + d) - 14, 1)))
##   map(seq(from = 0, to = 5), \(d) mean(((gap_starts + d) - 0):max((gap_starts + d) - 13, 1)))
##   linear_roll_mean
## rolled %>% filter(geo_value == "al") %>% pull("a_m14")
  expect_equal(rolled %>% filter(geo_value == "al") %>% pull("a_m14"), linear_roll_mean)
  # and the standard deviation is
  linear_roll_mean <- append(linear_roll_mean, NA, after = removed_date - 1)
  linear_values <- 1:39
  linear_values <- append(linear_values, NA, after = removed_date - 1)
  linear_roll_sd <- sqrt(slider::slide_dbl((linear_values - linear_roll_mean)^2, \(x) mean(x, na.rm = TRUE), .before = 28 - 1))
  # drop the extra date caused by the inclusion of the NAs
  linear_roll_sd <- linear_roll_sd[-(removed_date)]
  expect_equal(rolled %>% filter(geo_value == "al") %>% pull("a_sd28"), linear_roll_sd)
  # even though ca is reversed, the changes are all the same, so the standard deviation is *exactly* the same values
  expect_equal(rolled %>% filter(geo_value == "ca") %>% pull("a_sd28"), linear_roll_sd)
  # doesn't break types
  expect_true("epi_df" %in% class(rolled))
})

test_that("get_trainable_names pulls out mean and sd columns", {
  rolled <- rolling_sd(epi_data, keep_mean = TRUE)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "b", "a_m14", "a_sd28", "b_m14", "b_sd28"))
  expect_equal(get_trainable_names(rolled, NULL), c("a", "b"))
})
# TODO example with NA's, example with missing days, only one column, keep_mean

test_that("update_predictors keeps unmodified predictors", {
  epi_data["c"] <- NaN
  epi_data["d"] <- NaN
  epi_data["b_m14"] <- NaN
  epi_data["b_sd28"] <- NaN
  predictors <- c("a", "b", "c") # everything but d
  modified <- c("b", "c") # we want to exclude b but not its modified versions
  expected_predictors <- c("a", "b_m14", "b_sd28")
  expect_equal(update_predictors(epi_data, modified, predictors), expected_predictors)
  expected_if_all_modified <- c("b_m14", "b_sd28")
  expect_equal(update_predictors(epi_data, NULL, predictors), expected_if_all_modified)
})

test_that("rolling_sd doesn't keep the mean columns by default", {
  rolled <- rolling_sd(epi_data)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "b", "a_sd28", "b_sd28"))
})
