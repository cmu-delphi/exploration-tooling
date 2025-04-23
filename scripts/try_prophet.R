# Testing the Prophet forecaster
# https://facebook.github.io/prophet/docs/handling_shocks.html
library(prophet)
library(targets)
library(epiprocess)


prophet_forecaster <- function(epi_data, ahead, extra_data, ...) {
  df <- epi_data %>%
    filter(source == "nhsn") %>%
    select(geo_value, time_value, hhs) %>%
    rename(ds = time_value, y = hhs)
  # Use interval width to get quantiles
  # Use MCMC to get uncertainty in seasonality
  m <- prophet(df, interval.width = .80, mcmc.samples = 300)
  future <- make_future_dataframe(m, periods = ahead)
  forecast <- predict(m, future)
  return(forecast)
}

forecast <- tar_read(joined_archive_data) %>%
  epix_as_of_current() %>%
  filter(geo_value == "fl", source == "nhsn") %>%
  prophet_forecaster(1)
