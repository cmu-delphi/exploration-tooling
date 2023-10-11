# if you're adding a forecaster, add it to the list to be tested
forecasters <- tribble(
  ~forecaster, ~extra_params, ~extra_params_names,
  scaled_pop, list(1, TRUE), list("ahead", "pop_scaling"),
  scaled_pop, list(1, FALSE), list("ahead", "pop_scaling"),
)
synth_mean <- 25
synth_sd <- 2
tiny_sd <- 1.0e-5
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = 40)
approx_zero <- rnorm(length(simple_dates), sd = tiny_sd)
# technically white noise, but with a variance that is miniscule
constant <- as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = synth_mean + approx_zero
))
# wrap a call that is made quite frequently
# n_training_pad is set to avoid warnings from the trainer
get_pred <- function(dataset,
                     ii, outcome = "a", extra_sources = "") {
  res <- forecaster_pred(
    data = dataset,
    outcome = outcome,
    extra_sources = extra_sources,
    n_training_pad = 5,
    forecaster = forecasters$forecaster[[ii]],
    forecaster_args = forecasters$extra_params[[ii]],
    forecaster_args_names = forecasters$extra_params_names[[ii]]
  )
  return(res)
}

test_that("constant", {
  different_constants <- as_epi_archive(rbind(
    constant$DT,
    tibble(
      geo_value = "ca",
      time_value = simple_dates,
      version = simple_dates,
      a = 4 * synth_mean + approx_zero
    )
  ))
  for (ii in 1:nrow(forecasters)) {
    res <- get_pred(different_constants, ii)

    rel_values <- res %>%
      group_by(geo_value) %>%
      filter(quantile == .5)
    sd_values <- rel_values %>%
      summarise(is_const = sd(value) < 0.001) %>%
      pull(is_const) %>%
      all()
    expect_true(sd_values)
    actual_value <- rel_values %>%
      mutate(is_right = near(value, true_value)) %>%
      pull(is_right) %>%
      all()
    expect_true(actual_value)
  }
})


test_that("white noise", {
  set.seed(12345)
  white_noise <- as_epi_archive(tibble(
    geo_value = "al",
    time_value = simple_dates,
    version = simple_dates,
    a = rnorm(length(simple_dates), mean = synth_mean, sd = synth_sd)
  ))
  for (ii in 1:nrow(forecasters)) {
    res <- get_pred(white_noise, ii)

    values <- res %>%
      filter(quantile == .5) %>%
      pull(value)
    expect_true(sd(values) < .01)
    # how much is each quantile off from the expected value?
    # should be fairly generous here, we just want the right order of magnitude
    quantile_deviation <- res %>%
      mutate(
        diff_from_exp =
          (value - qnorm(quantile, mean = synth_mean, sd = synth_sd)) /
            as.integer(target_end_date - forecast_date)
      ) %>%
      select(-true_value, -value) %>%
      group_by(quantile) %>%
      summarize(err = abs(mean(diff_from_exp)))
    expect_true(all(quantile_deviation$err < length(unique(res$target_end_date))))
  }
})


test_that("delayed state", {
  set.seed(12345)
  state_delay <- rpois(length(simple_dates), 0.5)
  missing_state <- as_epi_archive(rbind(
    constant$DT,
    tibble(
      geo_value = "ca",
      time_value = simple_dates,
      version = simple_dates + state_delay,
      a = synth_mean + approx_zero
    )
  ))
  for (ii in seq_len(nrow(forecasters))) {
    expect_no_error(res <- get_pred(missing_state, ii))
    expect_equal(length(unique(res$geo_value)), 2)
    counts <- res %>%
      filter(quantile == 0.5 & !is.na(value)) %>%
      group_by(geo_value) %>%
      count()
    counts_ca <- counts %>%
      filter(geo_value == "ca") %>%
      pull(n)
    counts_al <- counts %>%
      filter(geo_value == "al") %>%
      pull(n)
    expect_true(counts_al > counts_ca)
    expect_true(sum(state_delay == 0) > counts_ca)
    expect_true(counts_ca > 0)
  }
})

test_that("linear", {
  set.seed(12347)
  side_delay <- rpois(length(simple_dates), 0.5)
  side_delay
  start_date <- min(simple_dates)
  linear <- as_epi_archive(
    tibble(
      geo_value = "al",
      time_value = simple_dates,
      version = simple_dates,
      a = seq_len(length(simple_dates)) + approx_zero
    )
  )
  for (ii in seq_len(nrow(forecasters))) {
    res <- get_pred(linear, ii)
    # make sure that the median is on the sloped line
    median_err <- res %>%
      filter(quantile == .5) %>%
      mutate(err = value - as.integer(target_end_date - start_date + 1), .keep = "none") %>%
      mutate(is_right = near(err,0, tol=.0001), .keep = "none")
    expect_true(all(median_err))
  }
})
