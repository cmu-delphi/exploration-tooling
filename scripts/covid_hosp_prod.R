# The COVID Hospitalization Production Forecasting Pipeline.
#
# Ran into some issues with targets:
#   https://github.com/ropensci/targets/discussions/666#discussioncomment-9050772
#

source("scripts/targets-common.R")

submission_directory <- Sys.getenv("COVID_SUBMISSION_DIRECTORY", "cache")
insufficient_data_geos <- c("as", "mp", "vi", "gu")
forecast_generation_date <- Sys.Date()
bad_forecast_exclusions <- map(forecast_generation_date, get_exclusions)
forecaster_fns <- list2(
  linear = function(...) {
    forecaster_baseline_linear(...)
  },
  climate_base = function(...) {
    climatological_model(
      ...,
    )
  },
  climate_geo_agged = function(...) {
    climatological_model(
      ...,
      geo_agg = TRUE
    )
  },
  climate_quantile_extrapolated = function(...) {
    climatological_model(
      ...,
      quantile_method = "epipredict"
    )
  },
)


rlang::list2(
  tar_target(
    aheads,
    command = {
      -1:3
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
      nhsn_latest_data,
      command = {
        nhsn_archive <- s3readRDS(object = "nhsn_archive.rds", bucket = "forecasting-team-data")
        nhsn_archive %>%
          epix_as_of(nhsn_archive$versions_end) %>%
          filter(disease == "nhsn_covid") %>%
          select(-disease)
      }
    ),
    tar_target(
      forecast_res,
      command = {
        nhsn_latest_data %>%
          as_epi_df(as_of = as.Date(forecast_generation_date)) %>%
          forecaster_fns[[forecasters]](ahead = aheads) %>%
          mutate(
            forecaster = names(forecaster_fns[forecasters]),
            geo_value = as.factor(geo_value)
          )
      },
      pattern = cross(aheads, forecasters)
    ),
    tar_target(
      name = ensemble_res,
      command = {
        forecast_res %>%
          group_by(geo_value, quantile, forecast_date, target_end_date) %>%
          mutate(quantile = round(quantile, digits = 2)) %>%
          summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
      }
    ),
    tar_target(
      name = make_submission_csv,
      command = {
        res <- ensemble_res
        browser()
        res
        debugonce(write_submission_file)
        res %>%
          filter(geo_value %nin% bad_forecast_exclusions) %>%
          format_flusight(disease = "covid") %>%
          write_submission_file(get_forecast_reference_date(as.Date(forecast_generation_date)), submission_directory)
      }
    ),
    tar_target(
      notebook,
      command = {
        4
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          "scripts/forecast_report.Rmd",
          output_file = here::here(
            "reports",
            sprintf("covid_forecast_report_%s.html", as.Date(forecast_generation_date))
          ),
          params = list(
            disease = "covid",
            forecast_res = forecast_res,
            ensemble_res = ensemble_res,
            forecast_generation_date = as.Date(forecast_generation_date),
            truth_data = nhsn_latest_data
          )
        )
      }
    )
  ),
)
