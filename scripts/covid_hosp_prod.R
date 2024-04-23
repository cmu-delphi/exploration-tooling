# The COVID Hospitalization Production Forecasting Pipeline.
#
# Ran into some issues with targets:
#   https://github.com/ropensci/targets/discussions/666#discussioncomment-9050772
#

source("scripts/targets-common.R")

insufficient_data_geos <- c("as", "gu", "mp", "vi")
forecast_generation_date <- as.character(seq.Date(as.Date("2024-04-22"), Sys.Date(), by = "1 week"))
# forecast_generation_date <- as.character(seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "1 week"))
bad_forecast_exclusions <- Vectorize(epieval::get_exclusions)(forecast_generation_date)
forecaster_fns <- list(
  function(...) {
    smoothed_scaled(
      ...,
      outcome = "hhs",
      pop_scaling = TRUE,
      trainer = epipredict::quantile_reg(),
      lags = list(c(0, 7, 14, 21, 28), c(0))
    )
  }
)

rlang::list2(
  tar_target(
    aheads,
    command = {
      7 * 1:4
    }
  ),
  tar_target(
    forecasters,
    command = {
      seq_along(forecaster_fns)
    }
  ),
  tar_map(
    values = tidyr::expand_grid(
      tibble(
        forecast_generation_date = forecast_generation_date,
        bad_forecast_exclusions = bad_forecast_exclusions
      )
    ),
    names = "forecast_generation_date",
    tar_target(
      hhs_latest_data,
      command = {
        epidatr::pub_covidcast(
          source = "hhs",
          signals = "confirmed_admissions_covid_1d",
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = epidatr::epirange(from = "2020-01-01", to = forecast_generation_date),
          as_of = forecast_generation_date,
          fetch_args = epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
        ) %>%
          select(geo_value, time_value, value, issue) %>%
          rename("hhs" := value) %>%
          rename(version = issue) %>%
          filter(!geo_value %in% insufficient_data_geos)
      }
    ),
    tar_target(
      forecast,
      command = {
        hhs_latest_data %>%
          as_epi_df() %>%
          forecaster_fns[[forecasters]](ahead = aheads) %>%
          mutate(
            forecaster = sprintf("epipredict_%s", forecasters),
            geo_value = as.factor(geo_value)
          )
      },
      pattern = cross(aheads, forecasters)
    ),
    tar_target(
      notebook,
      command = {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          "scripts/covid_hosp_prod.Rmd",
          output_file = here::here(
            "reports",
            sprintf("covid_hosp_prod_%s.html", forecast_generation_date)
          ),
          params = list(
            forecast = forecast,
            bad_forecast_exclusions = bad_forecast_exclusions
          )
        )
      }
    )
  )
)
