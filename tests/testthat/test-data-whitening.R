suppressPackageStartupMessages(source(here::here("R", "load_all.R")))
real_ex <- epidatasets::covid_case_death_rates %>%
  as_tibble() %>%
  mutate(source = "same") %>%
  as_epi_df(other_keys = "source")
real_params <- calculate_whitening_params(real_ex, c("case_rate", "death_rate"))
test_that("whitening and then coloring returns the original data", {
  whiten_color_diff <- real_ex %>%
    data_whitening(c("case_rate", "death_rate"), real_params) %>%
    data_coloring(c("case_rate", "death_rate"), real_params) %>%
    left_join(real_ex, by = join_by(geo_value, source, time_value)) %>%
    mutate(net_death = death_rate.x - death_rate.y, net_case = case_rate.x - case_rate.y) %>%
    summarise(err_death = sum(net_death, na.rm = TRUE), err_case = sum(net_case, na.rm = TRUE))
  expect_equal(whiten_color_diff %>% unlist(use.names = FALSE), c(0, 0))
})

test_that("whitening small specific example", {
  linear_small <- c(0, .5, 1, 1.5, 2)
  linear_large <- linear_small * 100 + 50
  ex_data <- tibble(
    geo_value = c(rep("ga", times = 5), rep("gb", times = 5)),
    source = c(rep("sa", times = 10)),
    value = c(linear_small, linear_large)
  )
  scaled_data <- ex_data %>% mutate(value = (value + 0.01)^0.25)
  center_small <- median((linear_small + 0.01)^0.25)
  center_large <- median((linear_large + 0.01)^0.25)
  scale_small <- quantile((linear_small + 0.01)^0.25, 0.95) -
    quantile((linear_small + 0.01)^0.25, 0.05) +
    0.01
  scale_large <- quantile((linear_large + 0.01)^0.25, 0.95) -
    quantile((linear_large + 0.01)^0.25, 0.05) +
    0.01
  params <- calculate_whitening_params(ex_data, "value")
  expect_equal(params$value_center, c(center_small, center_large))
  expect_equal(params$value_scale, c(scale_small, scale_large) %>% unname())
})
