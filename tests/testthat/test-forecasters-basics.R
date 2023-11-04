library(dplyr)
# TODO better way to do this than copypasta
forecasters <- list(
  c("scaled_pop", scaled_pop),
  c("flatline_fc", flatline_fc)
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

  test_that(paste(forecaster[[1]], "deals with no as_of"), {
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    # what if we have no as_of date? assume they mean the last available data
    attributes(jhu)$metadata$as_of <- NULL
    expect_no_error(res <- forecaster[[2]](jhu, "case_rate", c("death_rate"), 2L))
    expect_equal(res$target_end_date %>% unique, max(jhu$time_value)+2)
    })

  test_that(paste(forecaster[[1]], "handles last second NA's"), {
    # if the last entries are NA, we should still predict
    # TODO: currently this checks that we DON'T predict
    jhu <- epipredict::case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-12-01"))
    geo_values <-jhu$geo_value %>% unique()
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
      as_epi_df()
    attributes(jhu_nad)$metadata$as_of <- max(jhu_nad$time_value) + 3
    expect_no_error(nas_forecast <- forecaster[[2]](jhu_nad, "case_rate", c("death_rate")))
    # TODO: this shouldn't actually be null, it should be a bit further delayed
    expect_equal(nrow(nas_forecast), 0)
    })

    #################################
    # any forecaster specific tests
    if (forecaster[[1]] == "scaled_pop") {
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
    }
    # TODO confirming that it produces exactly the same result as arx_forecaster
    # test case where extra_sources is "empty"
    # test case where the epi_df is empty
  test_that(paste(forecaster[[1]], "scaled and unscaled don't make the same predictions"), {
      jhu <- epipredict::case_death_rate_subset %>%
        dplyr::filter(time_value >= as.Date("2021-12-01"))
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
