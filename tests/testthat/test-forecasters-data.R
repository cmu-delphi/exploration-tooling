library(dplyr)


# if you're adding a forecaster, add it to the list to be tested
forecasters <- tibble::tribble(
  ~forecaster, ~extra_params, ~extra_params_names, ~fc_name,
  scaled_pop, list(1, TRUE), list("ahead", "pop_scaling"), "scaled_pop",
  scaled_pop, list(1, FALSE), list("ahead", "pop_scaling"), "scaled_pop",
  flatline_fc, list(1), list("ahead"), "flatline_fc",
  smoothed_scaled, list(1, list(c(0, 7, 14), c(0))), list("ahead", "lags"), "smoothed_scaled"
)
expects_nonequal <- c("scaled_pop", "smoothed_scaled")
# wrap a call that is made quite frequently
# n_training_pad is set to avoid warnings from the trainer
default_slide_forecaster <- function(dataset,
                                     ii, outcome = "a", extra_sources = "", linreg_warnings = TRUE) {
  # linear_reg really doesn't like exactly equal data, and throws a warning.
  # wrapperfun is to suppress that for forecasters using linear_reg
  if (any(forecasters$fc_name[[ii]] %in% expects_nonequal) && linereg_warnings) {
    wrapperfun <- function(x) {
      suppressWarnings(expect_warning(x,
        regexp =
          "prediction from rank-deficient fit"
      ))
    }
  } else {
    wrapperfun <- identity
  }
  wrapperfun(res <- slide_forecaster(
    data = dataset,
    outcome = outcome,
    extra_sources = extra_sources,
    n_training_pad = 5,
    forecaster = forecasters$forecaster[[ii]],
    forecaster_args = forecasters$extra_params[[ii]],
    forecaster_args_names = forecasters$extra_params_names[[ii]]
  ))
  return(res)
}

# setting up some otherwise magic numbers
synth_mean <- 25
synth_sd <- 2
tiny_sd <- 1.0e-5
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = 40)
# create an archive that contains a single version for each of the 40 days in
# `simple_dates`, for the single geo-value "al" (arbitrary choice)
constant <- epiprocess::as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = synth_mean
))

################################################################################
################################ Constant data #################################
################################################################################
# a dataset that has 2 different constant values at 2 different locations
# Meant to check
different_constants <- rbind(
  constant$DT,
  tibble(
    geo_value = "ca",
    time_value = simple_dates,
    version = simple_dates,
    a = 4 * synth_mean
  )
) %>%
  arrange(version, time_value) %>%
  epiprocess::as_epi_archive()
for (ii in 1:nrow(forecasters)) {
  test_that(paste(
    forecasters$fc_name[[ii]],
    " predicts a constant median for constant data"
  ), {
    res <- default_slide_forecaster(different_constants, ii)


    # only looking at the median, because the rest of the quantiles are going to
    # be pretty weird on an actually constant input
    rel_values <- res %>%
      group_by(geo_value) %>%
      filter(quantile == .5)

    # the observed median should be approximately the actual median, to within a
    # small amount of noise
    actual_value <- rel_values %>%
      mutate(is_right = near(value, true_value)) %>%
      pull(is_right) %>%
      all()
    expect_true(actual_value)

    # the observed sd of the median should be within floating point error of
    # zero
    sd_values <- rel_values %>%
      summarise(is_const = near(sd(value), 0)) %>%
      pull(is_const) %>%
      all()
    expect_true(sd_values)
  })
}


################################################################################
################################ White Noise ###################################
################################################################################
# A dataset that white noise with mean 25, and standard deviation of 2.
# Meant to check for sane handling of the simplest realistic case
set.seed(12345)
white_noise <- epiprocess::as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = rnorm(length(simple_dates), mean = synth_mean, sd = synth_sd)
))
for (ii in 1:nrow(forecasters)) {
  test_that(paste(
    forecasters$fc_name[[ii]],
    " predicts the median and the right quantiles for Gaussian data"
  ), {
    expect_no_error(res <- default_slide_forecaster(white_noise, ii, linreg_warnings = FALSE))

    # Make sure that the sample standard deviation of the mean is within the
    # true standard deviation, with some leeway
    values <- res %>%
      filter(quantile == .5) %>%
      pull(value)
    expect_true(sd(values) < 2 * synth_sd)

    # Make sure that each quantile doesn't deviate too much from the value that
    # would be predicted by the generating gaussian.
    quantile_deviation <- res %>%
      mutate(
        diff_from_exp =
          value - qnorm(quantile, mean = synth_mean, sd = synth_sd)
      ) %>%
      select(-true_value, -value) %>%
      group_by(quantile) %>%
      summarize(err = abs(mean(diff_from_exp)))
    expect_true(all(quantile_deviation$err < synth_sd))
  })
}


################################################################################
############################### Simple Latency #################################
################################################################################
# A dataset where one state is just the constant above, but the other has data
# that is delayed by various amounts. A check for latency behavior
# We check that the undelayed is predicted every day, while the delayed is
# predicted whenever there's data at the appropriate lags (this could use work,
# its not that precise)
constant <- epiprocess::as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = synth_mean
))
set.seed(12345)
# delay is set to a small poisson
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
  test_that(paste(
    forecasters$fc_name[[ii]],
    "predicts well in the presence of only one state with variably delayed data"
  ), {
    res <- default_slide_forecaster(missing_state, ii)

    # some predictions exist for both states
    expect_equal(length(unique(res$geo_value)), 2)
    # get the number of predictions by state
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
    # flatline is more aggressive about forecasting, so it will always have a
    # prediction if there's past data at all
    if (forecasters$fc_name[[ii]] == "flatline_fc") {
      expect_true(counts_al == counts_ca)
    } else {
      expect_true(counts_al > counts_ca)
    }
    # the number of days which have data on the day of is an upper bound
    expect_true(sum(state_delay == 0) > counts_ca)
    # at least one day could be predicted
    expect_true(counts_ca > 0)
    # ideally we would figure out which days actually have data available at
    # exactly the given lag
  })
}

################################################################################
################################ Simple sloped #################################
################################################################################
# a dataset that increases linearly at a rate of 1/day
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
  test_that(paste(
    forecasters$fc_name[[ii]],
    " predicts a linear increasing slope correctly"
  ), {
    # flatline will definitely fail this, so it's exempt
    if (forecasters$fc_name[[ii]] == "flatline_fc") {
      # flatline gets a cookie for existing
      expect_true(TRUE)
    } else {
      res <- default_slide_forecaster(linear, ii)

      # make sure that the median is on the line
      median_err <- res %>%
        filter(quantile == .5) %>%
        mutate(err = value - as.integer(target_end_date - start_date + 1), .keep = "none") %>%
        mutate(is_right = near(err, 0), .keep = "none")
      expect_true(all(median_err))
    }
  })
}
