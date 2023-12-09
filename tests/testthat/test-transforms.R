n_days <- 40
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = n_days)
rand_vals <- rnorm(n_days)
epi_data <- epiprocess::as_epi_df(rbind(tibble(
geo_value = "al",
time_value = simple_dates,
a = 1:n_days,
b = rand_vals
), tibble(
geo_value = "ca",
time_value = simple_dates,
a = n_days:1,
b = rand_vals + 10
)))
test_that("rolling_mean generates correct mean", {
  rolled <- rolling_mean(epi_data)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "b", "a7", "b7"))
  # hand specified rolling mean with a rear window of 7, noting that mean(1:7) = 4
  linear_roll_mean <- c(seq(from=1, to = 4, by = .5), seq(from = 4.5, to = 36.5, by = 1))
  expect_equal(rolled %>% filter(geo_value == "al") %>% pull("a7"), linear_roll_mean)
  # same, but "ca" is reversed, noting mean(40:(40-7)) =36.5
  linear_reverse_roll_mean <- c(seq(from=40, to = 36.5, by = -0.5), seq(from = 35.5, to = 4.5, by = -1))
  expect_equal(rolled %>% filter(geo_value == "ca") %>% pull("a7"), linear_reverse_roll_mean)
})

test_that("rolling_sd generates correct standard deviation", {
  rolled <- rolling_sd(epi_data)
  expect_equal(names(rolled), c("geo_value", "time_value", "a", "b", "a_SD28", "b_SD28"))
  # hand specified rolling mean with a rear window of 7, noting that mean(1:14) = 7.5
  linear_roll_mean <- c(seq(from=1, to = 7.5, by = .5), seq(from = 8, to = 33, by = 1))
  # and the standard deviation is
  linear_roll_sd <- sqrt(slider::slide_dbl((1:40 - linear_roll_mean)^2, mean, .before = 28))
  expect_equal(rolled %>% filter(geo_value == "al") %>% pull("a_SD28"), linear_roll_sd)
  # even though ca is reversed, the changes are all the same, so the standard deviation is *exactly* the same values
  expect_equal(rolled %>% filter(geo_value == "ca") %>% pull("a_SD28"), linear_roll_sd)
  })
# TODO example with NA's, example with missing days, only one column, keep_mean
