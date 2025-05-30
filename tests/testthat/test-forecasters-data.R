suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

testthat::skip("Optional, long-running tests skipped.")

# A list of forecasters to be tested. Add here to test new forecasters.
# fmt: skip
forecasters <- tibble::tribble(
  ~forecaster, ~forecaster_args, ~forecaster_args_names, ~fc_name, ~outcome, ~extra_sources, ~ahead,
  scaled_pop, list(TRUE), list("pop_scaling"), "scaled_pop", "a", "", 1,
  scaled_pop, list(FALSE), list("pop_scaling"), "scaled_pop", "a", "", 1,
  flatline_fc, list(), list(), "flatline_fc", "a", "", 1,
  smoothed_scaled, list(list(c(0, 7, 14), c(0)), 14, 7), list("lags", "sd_width", "sd_mean_width"), "smoothed_scaled", "a", "", 1,
)
# Which forecasters expect the data to be non-identical?
expects_nonequal <- c("scaled_pop", "smoothed_scaled")

#' A wrapper for a common call to slide a forecaster over a dataset.
#'
#' @param dataset The dataset to be used for the forecast.
#' @param ii The row of the forecasters table to be used.
#' @param outcome The name of the target column in the dataset.
#' @param extra_sources Any extra columns used for prediction that aren't
#' default.
#' @param expect_linreg_warnings Whether to expect and then suppress warnings
#' from linear_reg.
#'
#' Notes:
#' - n_training_pad is set to avoid warnings from the trainer.
#' - linear_reg doesn't like exactly equal data when training and throws a
#'   warning. wrapperfun is used to suppress that.
default_slide_forecaster <- function(dataset, ii, expect_linreg_warnings = TRUE) {
  if (any(forecasters$fc_name[[ii]] %in% expects_nonequal) && expect_linreg_warnings) {
    wrapperfun <- function(x) {
      suppressWarnings(expect_warning(x, regexp = "prediction from rank-deficient fit"))
    }
  } else {
    wrapperfun <- identity
  }
  args <- forecasters %>%
    select(-fc_name) %>%
    slice(ii) %>%
    purrr::transpose() %>%
    pluck(1)
  wrapperfun(res <- inject(slide_forecaster(epi_archive = dataset, n_training_pad = 30, !!!args)))
  return(res)
}

# Some arbitrary magic numbers used to generate data.
synth_mean <- 25
synth_sd <- 2
tiny_sd <- 1.0e-5
simple_dates <- seq(as.Date("2012-01-01"), by = "day", length.out = 40)

# Create an archive that contains a single version for each of the 40 days in
# `simple_dates`, for the single geo-value "al" (arbitrary choice).
constant <- tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = synth_mean
) %>%
  epiprocess::as_epi_archive()

################################################################################
################################ Constant data #################################
################################################################################
# A dataset that has the constant value `synth_mean` for all dates for "al" and
# 4 * `synth_mean` for all dates for "ca". Meant to check for sane handling of
# constant data.
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
different_constants_truth <- different_constants$DT %>%
  tibble() %>%
  rename("true_value" = "a", "target_end_date" = "time_value") %>%
  select(-version)
for (ii in seq_len(nrow(forecasters))) {
  test_that(
    paste(
      forecasters$fc_name[[ii]],
      " predicts a constant median for constant data"
    ),
    {
      res <- default_slide_forecaster(different_constants, ii)

      # Here we compare only the median, because the rest of the quantiles are
      # going to be pretty weird on a constant input.
      rel_values <- res %>%
        group_by(geo_value) %>%
        filter(quantile == .5)

      # We expect the forecaster median to be equal to the actual median plus
      # small noise noise
      actual_value <- rel_values %>%
        inner_join(
          different_constants_truth,
          by = c("geo_value", "target_end_date")
        ) %>%
        mutate(is_right = near(value, true_value)) %>%
        pull(is_right) %>%
        all()
      expect_true(actual_value)

      # We expect the forecasted standard deviation of the medians to be zero up
      # to numerical error
      sd_values <- rel_values %>%
        summarise(is_const = near(sd(value), 0)) %>%
        pull(is_const) %>%
        all()
      expect_true(sd_values)
    }
  )
}


################################################################################
################################ White Noise ###################################
################################################################################
# A white noise dataset with a single geo, mean 25 and standard deviation of 2.
# Meant to check for sane handling of a simple realistic case.
set.seed(12345)
white_noise <- epiprocess::as_epi_archive(tibble(
  geo_value = "al",
  time_value = simple_dates,
  version = simple_dates,
  a = rnorm(length(simple_dates), mean = synth_mean, sd = synth_sd)
))
for (ii in seq_len(nrow(forecasters))) {
  test_that(
    paste(
      forecasters$fc_name[[ii]],
      " predicts the median and the right quantiles for Gaussian data"
    ),
    {
      expect_no_error(res <- default_slide_forecaster(white_noise, ii, expect_linreg_warnings = FALSE))

      # We expect the standard deviation of the forecasted median values to be
      # within two true standard deviations of the true data mean.
      expect_true(
        (res %>%
          filter(quantile == .5) %>%
          pull(value) %>%
          sd()) <
          2 * synth_sd
      )

      # Make sure that each quantile doesn't deviate too much from the value that
      # would be predicted by the generating Gaussian. Dmitry: this bound seems
      # loose, is it worth improving?
      quantile_deviation <- res %>%
        mutate(
          diff_from_expected = value - qnorm(quantile, mean = synth_mean, sd = synth_sd)
        ) %>%
        select(-any_of(c("true_value", "value"))) %>%
        group_by(quantile) %>%
        summarize(err = abs(mean(diff_from_expected)))
      expect_true(all(quantile_deviation$err < 4 * synth_sd))
    }
  )
}


################################################################################
############################### Simple Latency #################################
################################################################################
# A dataset where one state is just the constant above, but the other has data
# that is delayed by various amounts, so this functions as a check for latency
# behavior. We check that the undelayed is predicted every day, while the
# delayed is predicted whenever there's data at the appropriate lags (this could
# use work, its not that precise).
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
  test_that(
    paste(
      forecasters$fc_name[[ii]],
      "predicts well in the presence of only one state with variably delayed data"
    ),
    {
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
    }
  )
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
  test_that(
    paste(
      forecasters$fc_name[[ii]],
      " predicts a linear increasing slope correctly"
    ),
    {
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
    }
  )
}
