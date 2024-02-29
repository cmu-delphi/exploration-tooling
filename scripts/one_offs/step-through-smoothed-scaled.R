# this script runs exactly one smoothed_scaled forecaster at the timepoint `forecast_as_of`.
# the goal was to have a simple point comparison with our legacy forecasters
library(dplyr)
library(tidyr)
library(parallel)
library(ggplot2)
library(epidatr)
library(epiprocess)
devtools::load_all(export_all = FALSE)

forecast_as_of <- as.Date("2021-11-08")

data_start_date <- as.Date("2020-08-01")
target_geo_values <- c(tolower(state.abb), "dc", "pr", "vi") # TODO consider national-level
aheads <- c(0:28)
## aheads <- c(0L, 7L, 14L, 21L, 28L)

cce <- covidcast_epidata()

hhs_snapshot <-
  cce$signals$`hhs:confirmed_admissions_covid_1d`$call(
    "state", target_geo_values,
    epirange(data_start_date, 34560101),
    as_of = as.character(forecast_as_of)
  )

hhs_snapshot_edf <- hhs_snapshot %>%
  pivot_wider(
    id_cols = c("geo_value", "time_value"),
    names_from = c("source", "signal"),
    values_from = "value"
  ) %>%
  as_epi_df(as_of = forecast_as_of)

# get the real data
hhs_up_to_date <-
  cce$signals$`hhs:confirmed_admissions_covid_1d`$call(
    "state", target_geo_values,
    epirange(data_start_date, forecast_as_of + max(aheads)),
  )

hhs_up_to_date %<>%
  pivot_wider(
    id_cols = c("geo_value", "time_value"),
    names_from = c("source", "signal"),
    values_from = "value"
  ) %>%
  as_epi_df() %>%
  filter((geo_value == "az"), (time_value %in% (forecast_as_of + aheads)))
## debugonce(quantreg::rq.fit)

## smoothed_scaled(
##   hhs_snapshot_edf,
##   outcome = "hhs_confirmed_admissions_covid_1d",
##   trainer = epipredict::quantile_reg(),
##   ahead = 0L,
##   lags = list(
##     # smoothed
##     7L * (0:4),
##     # sd
##     0L
##   ),
##   pop_scaling = TRUE
## )

system.time({
  fcst <-
    aheads %>%
    mclapply(function(ahead) {
      smoothed_scaled(
        hhs_snapshot_edf,
        outcome = "hhs_confirmed_admissions_covid_1d",
        trainer = epipredict::quantile_reg(),
        ahead = ahead,
        lags = list(
          # smoothed
          7L * (0:4),
          # sd
          0L
        ),
        pop_scaling = TRUE
      )
    }, mc.cores = 6L) %>%
    bind_rows()
})

fcst %>%
  filter(geo_value == "az") %>%
  ggplot(aes(target_end_date, value, group = quantile)) %>%
  `+`(geom_line()) %>%
  `+`(geom_line(data = hhs_up_to_date, aes(x = time_value, y = hhs_confirmed_admissions_covid_1d, group = NULL))) %>%
  `+`(labs(title = "smoothed_scaling"))

# plotting a "relative to truth" comparison
zeroed <- fcst %>%
  filter(geo_value == "az") %>%
  left_join(hhs_up_to_date, by = join_by(target_end_date == time_value, geo_value == geo_value)) %>%
  mutate(value = value - hhs_confirmed_admissions_covid_1d)
ggplot(zeroed, aes(target_end_date, value, group = quantile)) +
  geom_line() +
  labs(title = "smoothed_scaled")

fcst %>%
  filter(
    geo_value == "az",
    target_end_date %in% range(target_end_date),
    quantile %in% range(quantile)
  )




####################################################
# the same, but only using the more recent training data
####################################################
short_start_date <- as.Date("2021-01-01")
hhs_snapshot_short <-
  cce$signals$`hhs:confirmed_admissions_covid_1d`$call(
    "state", target_geo_values,
    epirange(short_start_date, 34560101),
    as_of = as.character(forecast_as_of)
  )

hhs_snapshot_short_edf <- hhs_snapshot_short %>%
  pivot_wider(
    id_cols = c("geo_value", "time_value"),
    names_from = c("source", "signal"),
    values_from = "value"
  ) %>%
  as_epi_df(as_of = forecast_as_of)

system.time({
  fcst_short <-
    aheads %>%
    mclapply(function(ahead) {
      smoothed_scaled(
        hhs_snapshot_short_edf,
        outcome = "hhs_confirmed_admissions_covid_1d",
        trainer = epipredict::quantile_reg(),
        ahead = ahead,
        lags = list(
          # smoothed
          7L * (0:4),
          # sd
          0L
        ),
        pop_scaling = TRUE
      )
    }, mc.cores = 6L) %>%
    bind_rows()
})

zeroed_short <- fcst_short %>%
  filter(geo_value == "az") %>%
  left_join(hhs_up_to_date, by = join_by(target_end_date == time_value, geo_value == geo_value)) %>%
  mutate(value = value - hhs_confirmed_admissions_covid_1d)
ggplot(zeroed_short, aes(target_end_date, value, group = quantile)) +
  geom_line() +
  labs(title = "smoothed_scaled short")
