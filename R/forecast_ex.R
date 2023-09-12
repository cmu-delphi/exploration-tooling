smoothfc <- function(x, degree = 3L) {
  mtv <- max(x$time_value)
  fd <- attributes(x)$metadata$as_of
  x <- filter(x, !(geo_value %in% c("as", "gu", "mp", "vi")))

  rec <- epi_recipe(x) %>%
    step_epi_lag(cases7dav, lag = c(0:7, 14, 21)) %>%
    step_epi_lag(hosp7dav, lag = c(0, 7, 14)) %>%
    step_epi_ahead(hosp7dav, ahead = 1:28)
  f <- frosting() %>%
    layer_predict() %>%
    layer_unnest(.pred) %>%
    layer_naomit(distn) %>%
    layer_add_forecast_date(fd) %>%
    layer_population_scaling(
      distn,
      df = epipredict::state_census %>% select(geo_value = abbr, pop),
      df_pop_col = "pop",
      rate_rescaling = 1e5,
      by = c("geo_value" = "geo_value"),
      create_new = FALSE
    ) %>%
    layer_threshold(distn)
  ee <- smooth_quantile_reg(
    tau = c(.01, .025, 1:19 / 20, .975, .99),
    outcome_locations = 1:28,
    degree = degree
  )
  ewf <- epi_workflow(rec, ee, f)
  the_fit <- ewf %>% fit(x)

  latest <- get_test_data(rec, x, fill_locf = TRUE)
  preds <- predict(the_fit, new_data = latest) %>%
    mutate(forecast_date = fd, target_date = fd + ahead)
  preds
}

unloadNamespace("epipredict")
unloadNamespace("workflows")
unloadNamespace("parsnip")
library(epipredict)

jhu <- case_death_rate_subset %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

# wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
wf <- epi_workflow(r, quantile_reg()) %>% fit(jhu)

latest <- get_test_data(recipe = r, x = jhu)

f <- frosting() %>%
  layer_predict() %>%
  layer_residual_quantiles(probs = c(0.0275, 0.975), symmetrize = FALSE) %>%
  layer_naomit(.pred)
wf1 <- wf %>% add_frosting(f)

wf1
p <- predict(wf1, latest)
# column name for linear_reg is .pred
# actually it's the column *type* that matters

class(components$predictions$.pred)
inherits(components$predictions$.pred, "distribution")
components$mold$blueprint
