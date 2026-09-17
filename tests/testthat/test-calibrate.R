suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# A tiny synthetic hub: two seasons of weekly Saturday rounds, two locations,
# horizons 0 and 1, all 23 hub levels. Location "B" runs near zero with a base
# that over-predicts, which is the regime where transformed-scale played values
# go negative.
make_synthetic_hub <- function(seed = 1) {
  set.seed(seed)
  rounds <- c(
    seq(as.Date("2023-10-07"), by = 7, length.out = 30),
    seq(as.Date("2024-10-05"), by = 7, length.out = 30)
  )
  dates <- seq(min(rounds) - 7, max(rounds) + 14, by = 7)
  truth <- bind_rows(
    tibble(location = "A", target_end_date = dates, truth = round(200 + 150 * sin(seq_along(dates) / 5) + rnorm(length(dates), 0, 20))),
    tibble(location = "B", target_end_date = dates, truth = rpois(length(dates), 0.5))
  ) %>% mutate(truth = pmax(truth, 0))
  spread <- stats::qnorm(HUB_QUANTILE_LEVELS)
  forecasts <- tidyr::expand_grid(reference_date = rounds, horizon = 0:1, location = c("A", "B")) %>%
    mutate(target_end_date = reference_date + 7L * horizon) %>%
    left_join(truth, by = c("location", "target_end_date")) %>%
    mutate(
      centre = if_else(location == "A", 0.8 * truth, truth + 3),
      sd = if_else(location == "A", 20, 1)
    ) %>%
    tidyr::expand_grid(level_index = seq_along(HUB_QUANTILE_LEVELS)) %>%
    mutate(
      level = HUB_QUANTILE_LEVELS[level_index],
      value = pmax(centre + sd * spread[level_index], 0)
    ) %>%
    select(reference_date, horizon, target_end_date, location, level_index, level, value)
  list(forecasts = forecasts, truth = truth)
}

test_that("sqrt-space calibration returns monotone, non-negative quantiles", {
  hub <- make_synthetic_hub()
  cal <- calibrate_hub_forecasts(
    hub$forecasts, hub$truth, burn_in_seasons = "2023-2024", transform = "sqrt",
    lr_args = list(mult = 0.1, floor = 1e-3), slow_init = "burn_in_quantile", progress = FALSE
  )
  fc <- cal$forecasts %>% filter(!is.na(value_base))
  # The regime the clamp exists for is actually exercised: some sqrt-scale
  # played values are negative before the inverse transform.
  expect_true(any(sqrt(fc$value_base) + fc$hidden < 0))
  expect_true(all(fc$value_cal >= 0))
  crossing <- fc %>%
    arrange(location, horizon, reference_date, level) %>%
    group_by(location, horizon, reference_date) %>%
    summarize(crossing = any(diff(value_cal) < -1e-9), .groups = "drop")
  expect_false(any(crossing$crossing))
})

test_that("burn-in outcomes revealed at live rounds do not move the offsets", {
  hub <- make_synthetic_hub()
  cal <- calibrate_hub_forecasts(hub$forecasts, hub$truth, burn_in_seasons = "2023-2024", progress = FALSE)
  first_live <- min(cal$rounds$round_index[!cal$rounds$is_burn_in])
  # Horizon 1 has a 3-round delay, so the first live round reveals the last
  # burn-in rounds and the first live-issued outcome lands only at round
  # first_live + 3, moving hidden from first_live + 4 on.
  h1 <- cal$forecasts %>% filter(horizon == 1, location == "A", level == 0.5) %>% arrange(round_index)
  expect_true(all(h1$hidden[h1$round_index <= first_live + 3L] == 0))
  expect_true(any(h1$hidden[h1$round_index > first_live + 3L] != 0))
  expect_true(all(cal$series$n_revealed[cal$series$round_index == first_live & cal$series$horizon == 1] > 0))
})

test_that("off_after and fast_decay cannot be combined", {
  hub <- make_synthetic_hub()
  expect_error(
    calibrate_hub_forecasts(hub$forecasts, hub$truth, off_after = "02-15", fast_decay = 0.1, progress = FALSE),
    "cannot be combined"
  )
})

test_that("hub_series_matrix rejects duplicate (round, level) rows", {
  hub <- make_synthetic_hub()
  series <- hub$forecasts %>% filter(location == "A", horizon == 0)
  rounds <- sort(unique(series$reference_date))
  expect_no_error(hub_series_matrix(series, rounds, 23L))
  expect_error(hub_series_matrix(bind_rows(series, series[1, ]), rounds, 23L), "more than one value")
})
