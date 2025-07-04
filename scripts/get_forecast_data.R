# Get Forecast Data
#
# This is a basic script to download the forecast data from a forecasting hub.
# This script runs via cron on mentat.forecaster and gets the latest forecasts.
#
# Crontab:
# 0 9 *  *  4 source ~/.zshrc && cd /home/forecaster/prod/exploration-tooling-2024 && R -s -q -f scripts/get_forecast_data.r >> cache/get_forecast_data.log 2>&1
#
# The goal is to:
# 1. Download some select forecasts from a forecasting hub
# 2. Either save locally to a parquet file or upload to an S3 bucket
# 3. Optionally, read the parquet file back in and use it in a target pipeline
# 4. TODO: Add a companion metadata file that describes the forecasts in the parquet file
# 5. TODO: Add the metadata file to the S3 bucket
# 6. TODO: Allow incremental updates to the forecasts, by downloading only the new files
print("-------------------------------------------------------")
print(glue::glue("starting at {Sys.time()}"))
suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(lubridate)
  library(progress)
})

options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)

# Script run time
run_time <- with_tz(Sys.time(), tzone = "UTC")
run_time_local <- with_tz(run_time)


# Configuration
covid_config <- list(
  base_url = "https://raw.githubusercontent.com/cdcgov/covid19-forecast-hub/main/model-output",
  forecasters = c(
    "CMU-TimeSeries",
    "CovidHub-baseline",
    "CovidHub-ensemble",
    "UMass-ar6_pooled",
    "UMass-gbqr",
    "CEPH-Rtrend_covid",
    "Metaculus-cp"
  ),
  s3_bucket = "forecasting-team-data",
  s3_key = "exploration/2024-2025_covid_hosp_forecasts.parquet",
  disease = "covid"
)
# same but for flu
flu_config <- list(
  base_url = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/model-output",
  forecasters = c(
    "FluSight-baseline",
    "FluSight-ensemble",
    "CMU-TimeSeries",
    "PSI-PROF",
    "FluSight-lop_norm",
    "UMass-flusion",
    "NIH-Flu_ARIMA",
    "Metaculus-cp"
  ),
  s3_bucket = "forecasting-team-data",
  s3_key = "exploration/2024-2025_flu_hosp_forecasts.parquet",
  disease = "flu"
)


# Function to check if file exists on GitHub
check_github_file <- function(forecaster, filename, disease) {
  if (disease == "covid") {
    config <- covid_config
  } else if (disease == "flu") {
    config <- flu_config
  } else {
    stop("Invalid disease")
  }
  url <- paste0(config$base_url, "/", forecaster, "/", filename)
  response <- GET(url)
  return(status_code(response) == 200)
}

# Function to download and read a single file
download_forecast_file <- function(forecaster, filename, disease) {
  if (disease == "covid") {
    config <- covid_config
  } else if (disease == "flu") {
    config <- flu_config
  } else {
    stop("Invalid disease")
  }
  url <- paste0(config$base_url, "/", forecaster, "/", filename)
  tryCatch(
    {
      # Download directly to memory and parse
      df <- readr::read_csv(url) %>%
        mutate(
          forecaster = forecaster,
          forecast_date = as.Date(str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")),
          output_type_id = as.numeric(output_type_id)
        ) %>%
        filter(output_type == "quantile")
      return(df)
    },
    error = function(e) {
      warning(sprintf("Failed to download %s: %s", filename, e$message))
      return(NULL)
    }
  )
}

# Main function to fetch and combine forecast files
fetch_forecast_files <- function(sync_to_s3 = TRUE, disease) {
  if (disease == "covid") {
    config <- covid_config
  } else if (disease == "flu") {
    config <- flu_config
  } else {
    stop("Invalid disease")
  }

  # Generate date range
  # First get the nearest Saturday
  dates <- seq(as.Date("2023-10-14"), round_date(Sys.Date(), "week", 6), by = "week")

  # Initialize list to store all forecasts
  all_forecasts <- list()

  pb_forecasters <- progress_bar$new(
    format = "Downloading forecasts from :forecaster [:bar] :percent :eta",
    total = length(config$forecasters),
    clear = FALSE,
    width = 60
  )

  for (forecaster in config$forecasters) {
    pb_forecasters$tick(tokens = list(forecaster = forecaster))

    # Generate filenames for date range
    filenames <- paste0(format(dates, "%Y-%m-%d"), "-", forecaster, ".csv")

    # Create nested progress bar for files
    pb_files <- progress_bar$new(
      format = "  Downloading files [:bar] :current/:total :filename",
      total = length(filenames)
    )

    for (filename in filenames) {
      pb_files$tick(tokens = list(filename = filename))

      if (check_github_file(forecaster, filename, disease)) {
        forecast_data <- download_forecast_file(forecaster, filename, disease)
        if (!is.null(forecast_data)) {
          all_forecasts[[length(all_forecasts) + 1]] <- forecast_data
        }
      }
    }
  }

  # Combine all forecasts
  combined_forecasts <- bind_rows(all_forecasts)

  if (sync_to_s3) {
    combined_forecasts %>% aws.s3::s3write_using(nanoparquet::write_parquet, object = config$s3_key, bucket = config$s3_bucket)
  }

  return(combined_forecasts)
}

cli::cli_alert_info("Fetching COVID forecasts {run_time_local} (UTC: {run_time})")
covid_forecasts <- fetch_forecast_files(disease = "covid")
cli::cli_alert_info("Fetching FLU forecasts {run_time_local} (UTC: {run_time})")
flu_forecasts <- fetch_forecast_files(disease = "flu")
print(glue::glue("Run successfully finished at {Sys.time()}"))
print("-------------------------------------------------------")
print("-------------------------------------------------------")
