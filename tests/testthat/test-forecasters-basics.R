suppressPackageStartupMessages(source(here::here("R", "load_all.R")))
testthat::local_edition(3)
# TODO better way to do this than copypasta
forecasters <- list(
  list("scaled_pop", scaled_pop),
  list("flatline_fc", flatline_fc),
  list("smoothed_scaled", smoothed_scaled, lags = list(c(0, 2, 5), c(0)))
  # TODO: flusion is broken?
  # list("flusion", flusion),
  # TODOO: no_recent_outcome cannot be run without aux_data/apportionment.csv present
  # list("no_recent_outcome", no_recent_outcome)
)
for (forecaster in forecasters) {
  test_that(paste(forecaster[[1]], "gets the date and columns right"), {
    jhu <- epidatasets::covid_case_death_rates %>%
      dplyr::filter(time_value >= as.Date("2021-11-01"))
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3

    res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L)

    expect_equal(
      names(res),
      c("geo_value", "forecast_date", "target_end_date", "quantile", "value")
    )
    expect_true(all(
      res$target_end_date == as.Date("2022-01-01")
    ))
  })

  test_that(paste(forecaster[[1]], "handles only using 1 column correctly"), {
    skip("TODO: fix broken test, no_recent_outcome has an error")
    jhu <- epidatasets::covid_case_death_rates %>%
      dplyr::filter(time_value >= as.Date("2021-11-01"))
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    if (forecaster[[1]] == "smoothed_scaled") {
      expect_no_error(res <- forecaster[[2]](jhu, "case_rate", "", -2L, lags = forecaster$lags))
    } else {
      expect_no_error(suppressWarnings(res <- forecaster[[2]](jhu, "case_rate", "", -2L)))
    }
  })

  test_that(paste(forecaster[[1]], "deals with no as_of"), {
    skip("TODO: fix broken test, smoothed_scaled has an error")
    jhu <- epidatasets::covid_case_death_rates %>%
      dplyr::filter(time_value >= as.Date("2021-11-01"))
    # what if we have no as_of date? assume they mean the last available data
    attributes(jhu)$metadata$as_of <- NULL
    expect_snapshot(error = FALSE, res <- forecaster[[2]](jhu, "case_rate", extra_sources = "death_rate", ahead = 2L))
  })

  test_that(paste(forecaster[[1]], "handles last second NA's"), {
    # if the last entries are NA, we should still predict
    # TODO: currently this checks that we DON'T predict
    jhu <- epidatasets::covid_case_death_rates %>%
      dplyr::filter(time_value >= as.Date("2021-11-01"))
    geo_values <- jhu$geo_value %>% unique()
    one_day_nas <- tibble(
      geo_value = geo_values,
      time_value = as.Date("2022-01-01"),
      case_rate = NA,
      death_rate = runif(length(geo_values))
    )
    second_day_nas <- one_day_nas %>%
      mutate(time_value = as.Date("2022-01-02"))
    jhu_nad <- jhu %>%
      as_tibble() %>%
      bind_rows(one_day_nas, second_day_nas) %>%
      epiprocess::as_epi_df()
    attributes(jhu_nad)$metadata$as_of <- max(jhu_nad$time_value) + 3
    expect_no_error(nas_forecast <- forecaster[[2]](jhu_nad, "case_rate", c("death_rate"), ahead = 1))
    # TODO: this shouldn't actually be null, it should be a bit further delayed
    # predicting from 3 days later
    expect_equal(nas_forecast$forecast_date %>% unique(), as.Date("2022-01-05"))
    # predicting 1 day into the future
    expect_equal(nas_forecast$target_end_date %>% unique(), as.Date("2022-01-06"))
    # (nearly) every state and quantile has a prediction
    # as, vi and mp don't currently have populations for flusion, so they're not getting forecast
    max_n_geos <- length(jhu$geo_value %>% unique())
    if (forecaster[[1]] != "flatline_fc") {
      # flatline_fc is a little bit broken on last-minute `NA`'s right now
      expect_true(any(nrow(nas_forecast) == length(covidhub_probs()) * (max_n_geos - c(0, 1, 3))))
    }
  })

  test_that(paste(forecaster[[1]], "handles unused extra sources with NAs"), {
    # if there is an extra source we aren't using, we should ignore any NA's it has
    jhu <- epidatasets::covid_case_death_rates %>%
      dplyr::filter(time_value >= as.Date("2021-11-01"))
    jhu_nad <- jhu %>%
      as_tibble() %>%
      mutate(some_other_predictor = rep(c(NA, 3), times = 1708)) %>%
      epiprocess::as_epi_df()
    attributes(jhu_nad)$metadata$as_of <- max(jhu$time_value) + 3
    # should run fine
    expect_no_error(nas_forecast <- forecaster[[2]](jhu_nad, "case_rate", c("death_rate")))
    expect_equal(nas_forecast$forecast_date %>% unique(), max(jhu$time_value) + 3)
    # there's an actual full set of predictions
    max_n_geos <- length(jhu$geo_value %>% unique())
    expect_true(any(nrow(nas_forecast) == length(covidhub_probs()) * (max_n_geos - c(0, 1, 3))))
  })

  #################################
  # any forecaster specific tests
  if (forecaster[[1]] == "scaled_pop" || forecaster[[1]] == "smoothed_scaled") {
    test_that(paste(forecaster[[1]], "scaled and unscaled don't make the same predictions"), {
      jhu <- epidatasets::covid_case_death_rates %>%
        dplyr::filter(time_value >= as.Date("2021-11-01"))
      # the as_of for this is wildly far in the future
      attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
      res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L)
      # confirm scaling produces different results
      res_unscaled <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L, pop_scaling = FALSE, )
      expect_false(
        res_unscaled %>%
          full_join(
            res,
            by = join_by(geo_value, forecast_date, target_end_date, quantile),
            suffix = c(".unscaled", ".scaled")
          ) %>%
          mutate(equal = value.unscaled == value.scaled) %>%
          summarize(all(equal), .groups = "drop") %>%
          pull(`all(equal)`)
      )
    })
  } else if (forecaster[[1]] == "smoothed_scaled") {
    testthat("smoothed_scaled handles variable lags correctly", {
      jhu <- epidatasets::covid_case_death_rates %>%
        dplyr::filter(time_value >= as.Date("2021-11-01"))
      # the as_of for this is wildly far in the future
      attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
      expect_no_error(
        res <- forecaster[[2]](
          jhu,
          "case_rate",
          c("death_rate"),
          -2L,
          lags = list(c(0, 3, 5, 7), c(0), c(0, 3, 5, 7), c(0))
        )
      )
    })
  }
  # TODO confirming that it produces exactly the same result as arx_forecaster
  # test case where extra_sources is "empty"
  # test case where the epi_df is empty
  test_that(paste(forecaster[[1]], "empty epi_df predicts nothing"), {
    jhu <- epidatasets::covid_case_death_rates %>%
      dplyr::filter(time_value >= as.Date("2021-11-01"))
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L)
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    null_jhu <- jhu %>% filter(time_value < as.Date("0009-01-01"))
    expect_no_error(null_res <- forecaster[[2]](null_jhu, "case_rate", c("death_rate")))
    expect_identical(names(null_res), names(res))
    expect_equal(nrow(null_res), 0)
    expect_identical(
      null_res,
      tibble(
        geo_value = character(),
        forecast_date = lubridate::Date(),
        target_end_date = lubridate::Date(),
        quantile = numeric(),
        value = numeric()
      )
    )
  })
}

# unique tests
test_that("flatline_fc same across aheads", {
  jhu <- epidatasets::covid_case_death_rates %>%
    dplyr::filter(time_value >= as.Date("2021-11-01"))
  attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
  resM2 <- flatline_fc(jhu, "case_rate", c("death_rate"), -2L) %>%
    filter(quantile == 0.5) %>%
    select(-target_end_date)
  resM1 <- flatline_fc(jhu, "case_rate", c("death_rate"), -1L) %>%
    filter(quantile == 0.5) %>%
    select(-target_end_date)
  res1 <- flatline_fc(jhu, "case_rate", c("death_rate"), 1L) %>%
    filter(quantile == 0.5) %>%
    select(-target_end_date)
  expect_equal(resM2, resM1)
  expect_equal(resM2, res1)
})
