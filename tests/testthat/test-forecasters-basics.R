library(dplyr)
# TODO better way to do this than copypasta
forecasters <- list(
  c("scaled_pop", scaled_pop),
  c("flatline_fc", flatline_fc)
)
for (forecaster in forecasters) {
  test_that(forecaster[[1]], {
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
    # any forecaster specific tests
    if (forecaster[[1]] == "scaled_pop") {
      # confirm scaling produces different results
      res_unscaled <- forecaster[[2]](jhu,
        "case_rate",
        c("death_rate"),
        -2L,
        pop_scaling = FALSE
      )
      expect_false(res_unscaled %>%
        full_join(res,
          by = join_by(geo_value, forecast_date, target_end_date, quantile),
          suffix = c(".unscaled", ".scaled")
        ) %>%
        mutate(equal = value.unscaled == value.scaled) %>%
        summarize(all(equal)) %>% pull(`all(equal)`))
    }
    # TODO confirming that it produces exactly the same result as arx_forecaster
    # test case where extra_sources is "empty"
    # test case where the epi_df is empty
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