library(dplyr)


# if you're adding a forecaster, add it to the list to be tested
forecasters <- tibble::tribble(
  ~forecaster, ~extra_params, ~extra_params_names, ~fc_name,
  scaled_pop, list(1, TRUE), list("ahead", "pop_scaling"), "scaled_pop",
  scaled_pop, list(1, FALSE), list("ahead", "pop_scaling"), "scaled_pop",
  flatline_fc, list(1), list("ahead"), "flatline_fc",
  smoothed_scaled, list(1), list("ahead"), "smoothed_scaled"
)
expects_nonequal <- c("scaled_pop", "smoothed_scaled")
synth_mean <- 25
synth_sd <- 2
tiny_sd <- 1.0e-5
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = 40)
# technically white noise, but with a variance that is miniscule
constant <- epiprocess::as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = synth_mean
))

# wrap a call that is made quite frequently
# n_training_pad is set to avoid warnings from the trainer
get_pred <- function(dataset,
                     ii, outcome = "a", extra_sources = "") {
  res <- slide_forecaster(
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

# a dataset that has a constant value
# (technically a mean 25 sd 1e-5, since otherwise linear regression gets annoyed)
different_constants <- epiprocess::as_epi_archive(rbind(
  constant$DT,
  tibble(
    geo_value = "ca",
    time_value = simple_dates,
    version = simple_dates,
    a = 4 * synth_mean
  )
))
for (ii in 1:nrow(forecasters)) {
  test_that(paste(forecasters$fc_name[[ii]], " predicts a constant median for constant data"), {
    if (any(forecasters$fc_name[[ii]] %in% expects_nonequal)) {
      suppressWarnings(expect_warning(res <- get_pred(different_constants, ii), regexp = "prediction from rank-deficient fit"))
    } else {
      res <- get_pred(different_constants, ii)
    }


    # only looking at the median, because the rest of the quantiles are going to be pretty weird on an actually constant input
    rel_values <- res %>%
      group_by(geo_value) %>%
      filter(quantile == .5)
    # the observed sd of the median should be approximately the ~zero `tiny_sd`
    sd_values <- rel_values %>%
      summarise(is_const = sd(value) < 2 * tiny_sd) %>%
      pull(is_const) %>%
      all()
    expect_true(sd_values)

    # the observed median should be approximately the actual median, to within a small amount of noise
    actual_value <- rel_values %>%
      mutate(is_right = near(value, true_value, tol = tiny_sd^.5)) %>%
      pull(is_right) %>%
      all()
    expect_true(actual_value)
  })
}


# a dataset that is mean 25, with a standard deviation of 2
set.seed(12345)
white_noise <- epiprocess::as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = rnorm(length(simple_dates), mean = synth_mean, sd = synth_sd)
))
for (ii in 1:nrow(forecasters)) {
  test_that(paste(forecasters$fc_name[[ii]], " predicts the median and the right quantiles for Gaussian data"), {
    expect_no_error(res <- get_pred(white_noise, ii))

    # shouldn't expect the sample sd to actually match the true sd exactly, so giving it some leeway
    values <- res %>%
      filter(quantile == .5) %>%
      pull(value)
    expect_true(sd(values) < 2 * synth_sd)

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
    expect_true(all(quantile_deviation$err < 2 * synth_sd))
  })
}


# a dataset where one state is just the constant above, but the other has data that is delayed by various amounts
# we check that the undelayed is predicted, while the delayed is predicted whenever there's data a the lags (this could use work, its not that precise)
set.seed(12345)
state_delay <- rpois(length(simple_dates), 0.5)
missing_state <- epiprocess::as_epi_archive(rbind(
  constant$DT,
  tibble(
    geo_value = "ca",
    time_value = simple_dates,
    version = simple_dates + state_delay,
    a = synth_mean
  )
))
for (ii in seq_len(nrow(forecasters))) {
  test_that(paste(forecasters$fc_name[[ii]], "predicts well in the presence of only one state with variably delayed data"), {
    if (any(forecasters$fc_name[[ii]] %in% expects_nonequal)) {
      suppressWarnings(expect_warning(res <- get_pred(missing_state, ii), regexp = "prediction from rank-deficient fit"))
    } else {
      res <- get_pred(missing_state, ii)
    }
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
    res %>% filter(geo_value == "ca" & quantile == .5)
    # flatline is more aggressive about forecasting, so it will always have something if there's past data at all
    if (identical(forecasters$forecaster[[ii]], flatline_fc)) {
      expect_true(counts_al == counts_ca)
    } else {
      expect_true(counts_al > counts_ca)
    }
    # the number of days which have data on the day of is an upper bound
    expect_true(sum(state_delay == 0) > counts_ca)
    # at least one day could be predicted
    expect_true(counts_ca > 0)
  })
}

# a dataset that increases linearly at a rate of 1/day, plus the same noise as in the constant case (mean 0 sd 1e-5)
set.seed(12347)
start_date <- min(simple_dates)
linear <- epiprocess::as_epi_archive(
  tibble(
    geo_value = "al",
    time_value = simple_dates,
    version = simple_dates,
    a = seq_len(length(simple_dates))
  )
)
for (ii in seq_len(nrow(forecasters))) {
  test_that(paste(forecasters$fc_name[[ii]], "predicts a linear increasing slope correctly"), {
    # flatline will definitely fail this, so it's exempt
    if (!identical(forecasters$forecaster[[ii]], flatline_fc)) {
      if (any(forecasters$fc_name[[ii]] %in% expects_nonequal)) {
        suppressWarnings(expect_warning(res <- get_pred(linear, ii), regexp = "prediction from rank-deficient fit"))
      } else {
        res <- get_pred(linear, ii)
      }
      # make sure that the median is on the sloped line
      median_err <- res %>%
        filter(quantile == .5) %>%
        mutate(err = value - as.integer(target_end_date - start_date + 1), .keep = "none") %>%
        mutate(is_right = near(err, 0, tol = tiny_sd^0.5), .keep = "none")
      expect_true(all(median_err))
    } else {
      # flatline gets a cookie for existing
      expect_true(TRUE)
    }
  })
}
