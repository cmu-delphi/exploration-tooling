library(dplyr)
# TODO better way to do this than copypasta
forecasters <- list(
  list("scaled_pop", scaled_pop),
  list("flatline_fc", flatline_fc),
  list("smoothed_scaled", smoothed_scaled, lags = list(c(0, 2, 5), c(0)))
)
for (forecaster in forecasters) {
  test_that(paste(forecaster[[1]], "gets the date and columns right"), {
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L)
    expect_equal(
      names(res),
      c("geo_value", "forecast_date", "target_end_date", "quantile", "value")
    )
    expect_true(all(
      res$target_end_date ==
        as.Date("2022-01-01")
    ))
  })

  test_that(paste(forecaster[[1]], "handles only using 1 column correctly"), {
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    if (forecaster[[1]] == "smoothed_scaled") {
      expect_no_error(res <- forecaster[[2]](jhu, "case_rate", "", -2L, lags = forecaster$lags))
    } else {
      expect_no_error(res <- forecaster[[2]](jhu, "case_rate", "", -2L))
    }
  })

  test_that(paste(forecaster[[1]], "deals with no as_of"), {
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    # what if we have no as_of date? assume they mean the last available data
    attributes(jhu)$metadata$as_of <- NULL
    expect_no_error(res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), 2L))
    expect_equal(res$target_end_date %>% unique(), max(jhu$time_value) + 2)
  })

  test_that(paste(forecaster[[1]], "handles last second NA's"), {
    # if the last entries are NA, we should still predict
    # TODO: currently this checks that we DON'T predict
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
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
    expect_no_error(nas_forecast <- forecaster[[2]](jhu_nad, "case_rate", c("death_rate")))
    # TODO: this shouldn't actually be null, it should be a bit further delayed
    # predicting from 3 days later
    expect_equal(nas_forecast$forecast_date %>% unique(), as.Date("2022-01-05"))
    # predicting 1 day into the future
    expect_equal(nas_forecast$target_end_date %>% unique(), as.Date("2022-01-06"))
    # every state and quantile has a prediction
    expect_equal(nrow(nas_forecast), length(covidhub_probs()) * length(jhu$geo_value %>% unique()))
  })

  test_that(paste(forecaster[[1]], "handles unused extra sources with NAs"), {
    # if there is an extra source we aren't using, we should ignore any NA's it has
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    jhu_nad <- jhu %>%
      as_tibble() %>%
      mutate(some_other_predictor = NA) %>%
      epiprocess::as_epi_df()
    attributes(jhu_nad)$metadata$as_of <- max(jhu$time_value) + 3
    # should run fine
    expect_no_error(nas_forecast <- forecaster[[2]](jhu_nad, "case_rate", c("death_rate")))
    expect_equal(nas_forecast$forecast_date %>% unique(), max(jhu$time_value) + 3)
    # there's an actual full set of predictions
    expect_equal(nrow(nas_forecast), 1288)
  })

  #################################
  # any forecaster specific tests
  if (forecaster[[1]] == "scaled_pop" || forecaster[[1]] == "smoothed_scaled") {
    test_that(paste(forecaster[[1]], "scaled and unscaled don't make the same predictions"), {
      jhu <- epipredict::case_death_rate_subset %>%
        dplyr::filter(time_value >= as.Date("2021-12-01"))
      # the as_of for this is wildly far in the future
      attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
      res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L)
      # confirm scaling produces different results
      res_unscaled <- forecaster[[2]](jhu,
        "case_rate",
        c("death_rate"),
        -2L,
        pop_scaling = FALSE,
      )
      expect_false(res_unscaled %>%
        full_join(res,
          by = join_by(geo_value, forecast_date, target_end_date, quantile),
          suffix = c(".unscaled", ".scaled")
        ) %>%
        mutate(equal = value.unscaled == value.scaled) %>%
        summarize(all(equal)) %>% pull(`all(equal)`))
    })
  } else if (forecaster[[1]] == "smoothed_scaled") {
    testthat("smoothed_scaled handles variable lags correctly", {
      jhu <- epipredict::case_death_rate_subset %>%
        dplyr::filter(time_value >= as.Date("2021-12-01"))
      # the as_of for this is wildly far in the future
      attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
      expect_no_error(res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L, lags = list(c(0, 3, 5, 7), c(0), c(0, 3, 5, 7), c(0))))
    })
  }
  # TODO confirming that it produces exactly the same result as arx_forecaster
  # test case where extra_sources is "empty"
  # test case where the epi_df is empty
  test_that(paste(forecaster[[1]], "empty epi_df predicts nothing"), {
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), -2L)
    # the as_of for this is wildly far in the future
    attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
    null_jhu <- jhu %>% filter(time_value < as.Date("0009-01-01"))
    expect_no_error(null_res <- forecaster[[2]](null_jhu, "case_rate", c("death_rate")))
    expect_identical(names(null_res), names(res))
    expect_equal(nrow(null_res), 0)
    expect_identical(null_res, tibble(geo_value = character(), forecast_date = lubridate::Date(), target_end_date = lubridate::Date(), quantile = numeric(), value = numeric()))
  })
}

# unique tests
test_that("flatline_fc same across aheads", {
  jhu <- epipredict::case_death_rate_subset %>%
    dplyr::filter(time_value >= as.Date("2021-12-01"))
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

test_that("ensemble_average", {
  jhu <- epipredict::case_death_rate_subset %>%
    dplyr::filter(time_value >= as.Date("2021-12-01"))
  # the as_of for this is wildly far in the future
  attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
  # create some forecasts to ensemble
  meta_res <- list(
    scaled_pop(jhu, "case_rate", c("death_rate"), -2L),
    scaled_pop(jhu, "case_rate", c("death_rate"), -2L, lags = c(0, 1, 2, 3, 5, 7, 11, 13, 17)),
    scaled_pop(jhu, "case_rate", c("death_rate"), -2L, pop_scaling = FALSE),
    flatline_fc(jhu, "case_rate", ahead = -2L)
  )
  ave_ens <- ensemble_average(jhu, meta_res, "case_rate")
  # target date correct
  expect_true(all(
    ave_ens$target_end_date ==
      as.Date("2022-01-01")
  ))
  expect_equal(
    names(ave_ens),
    c("geo_value", "forecast_date", "target_end_date", "quantile", "value")
  )
  expect_true(all(
    ave_ens$target_end_date ==
      as.Date("2022-01-01")
  ))
  # make sure that key direction doesn't matter when generating ensembles
  ave_ens_reversed <- ensemble_average(jhu, meta_res, "case_rate")
  expect_true(all.equal(ave_ens_reversed, ave_ens))
  # make sure it produces the expected median of a random row
  sampled_rows_all <- purrr::map_vec(
    meta_res,
    ~ filter(.x, geo_value == "ca" & forecast_date == "2022-01-03" & quantile == .3)
  )
  sampled_row_by_hand <- sampled_rows_all %>%
    summarize(value = median(value), .by = c("geo_value", "forecast_date", "target_end_date", "quantile"))
  sampled_row_by_function <- ave_ens %>% filter(geo_value == "ca" & forecast_date == "2022-01-03" & quantile == .3)
  expect_equal(sampled_row_by_function, sampled_row_by_hand)

  mean_ens <- ensemble_average(jhu, meta_res, "case_rate", ensemble_args = list(average_type = mean))
  are_equal <- mean_ens %>%
    full_join(ave_ens,
      by = join_by(geo_value, forecast_date, target_end_date, quantile)
    ) %>%
    mutate(eq = value.x != value.y) %>%
    pull(eq)
  # expect the mean and median to be generally not equal
  expect_true(mean(are_equal) > 0.9)
  # any ensemble specific tests
  # test case where extra_sources is "empty"
  # test case where the epi_df is empty
  null_jhu <- jhu %>% filter(time_value < as.Date("0009-01-01"))
  expect_no_error(null_ave_ens <- ensemble_average(null_jhu, meta_res, "case_rate"))
  # ensemble_average doesn't actually depend on the input data
  expect_true(all.equal(ave_ens, null_ave_ens))

  # carry on as if nothing was missing if one of the forecasters is missing entries
  one_partially_missing <- rlang::list2(meta_res[[1]][1:900, 1:5], !!!meta_res[2:4])
  expect_no_error(partial_ave_ens <- ensemble_average(jhu, one_partially_missing))
  # the entries that are present for all of them are the same
  left_join(partial_ave_ens, ave_ens, by = c("geo_value", "forecast_date", "target_end_date", "quantile")) %>%
    summarize(all.equal(value.x, value.y))
  left_join(partial_ave_ens, ave_ens, by = c("geo_value", "forecast_date", "target_end_date", "quantile")) %>%
    mutate(eq = value.x == value.y) %>%
    pull(eq)
  expect_true(all.equal(partial_ave_ens[1:900, 1:5], ave_ens[1:900, 1:5]))
  expect_identical(names(null_ave_ens), names(ave_ens))
})
