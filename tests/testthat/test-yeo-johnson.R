suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("Yeo-Johnson transformation inverts correctly", {
  expect_true(
    map_lgl(seq(-5, 5, 0.1), function(lambda) {
      map_lgl(seq(-10, 10, 0.1), \(x) abs(yj_inverse(yj_transform(x, lambda), lambda) - x) < 0.00001) %>% all()
    }) %>%
      all()
  )
})

test_that("Yeo-Johnson steps and layers invert each other", {
  jhu <- cases_deaths_subset %>%
    filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
    select(geo_value, time_value, cases)
  filtered_data <- jhu

  # Get some lambda values
  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(cases) %>%
    step_epi_lag(cases, lag = 0) %>%
    step_epi_ahead(cases, ahead = 0, role = "outcome") %>%
    step_epi_naomit()
  tr <- r %>% prep(filtered_data)

  # Check general lambda values tibble structure
  expect_true(".lambda_cases" %in% names(tr$steps[[1]]$lambdas))
  expect_true(is.numeric(tr$steps[[1]]$lambdas$.lambda_cases))
  # Still works on a tibble
  expect_equal(
    tr %>% bake(filtered_data %>% as_tibble()),
    tr %>% bake(filtered_data)
  )

  # Make sure that the inverse transformation works
  f <- frosting() %>%
    layer_predict() %>%
    layer_epi_YeoJohnson(.pred)
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>% as_tibble() %>% slice_max(time_value, by = geo_value)
  out2 <- forecast(wf) %>% rename(cases = .pred)
  expect_equal(out1, out2)

  # Make sure it works when there are multiple predictors and outcomes
  jhu_multi <- epidatasets::covid_case_death_rates_extended %>%
    filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
    select(geo_value, time_value, case_rate, death_rate)
  filtered_data <- jhu_multi
  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(case_rate, death_rate) %>%
    step_epi_lag(case_rate, death_rate, lag = 0) %>%
    step_epi_ahead(case_rate, death_rate, ahead = 0, role = "outcome") %>%
    step_epi_naomit()
  tr <- r %>% prep(filtered_data)

  # Check general lambda values tibble structure
  expect_true(".lambda_case_rate" %in% names(tr$steps[[1]]$lambdas))
  expect_true(".lambda_death_rate" %in% names(tr$steps[[1]]$lambdas))
  expect_true(is.numeric(tr$steps[[1]]$lambdas$.lambda_case_rate))
  expect_true(is.numeric(tr$steps[[1]]$lambdas$.lambda_death_rate))

  # TODO: Make sure that the inverse transformation works
  skip("TODO")
  f <- frosting() %>%
    layer_predict() %>%
    layer_epi_YeoJohnson(.pred_ahead_0_case_rate)
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>% as_tibble() %>% slice_max(time_value, by = geo_value)
  # debugonce(slather.layer_epi_YeoJohnson)
  out2 <- forecast(wf) %>% rename(case_rate = .pred)
  expect_equal(out1, out2)
})

test_that("Yeo-Johnson steps and layers invert each other when other_keys are present", {
  skip("TODO")
  jhu <- cases_deaths_subset %>%
    filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
    select(geo_value, time_value, cases)
  filtered_data <- jhu

  # Get some lambda values
  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(cases) %>%
    step_epi_lag(cases, lag = 0) %>%
    step_epi_ahead(cases, ahead = 0, role = "outcome") %>%
    step_epi_naomit()
  tr <- r %>% prep(filtered_data)
  # Check for fixed lambda values
  expect_true(all(near(tr$steps[[1]]$lambdas$.lambda_cases, c(0.856, 0.207), tol = 0.001)))

  # Make sure that the inverse transformation works
  f <- frosting() %>%
    layer_predict() %>%
    layer_epi_YeoJohnson(.pred)
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>% as_tibble() %>% slice_max(time_value, by = geo_value)
  out2 <- forecast(wf) %>% rename(cases = .pred)
  expect_equal(out1, out2)
})
