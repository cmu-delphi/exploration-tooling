# Get Forecast Data
#
# This is a basic script to download the forecast data from a forecasting hub.
#
# The goal is to:
# 1. Download some select forecasts from a forecasting hub
# 2. Either save locally to a parquet file or upload to an S3 bucket
# 3. Optionally, read the parquet file back in and use it in a target pipeline
# 4. TODO: Add a companion metadata file that describes the forecasts in the parquet file
# 5. TODO: Add the metadata file to the S3 bucket
# 6. TODO: Allow incremental updates to the forecasts, by downloading only the new files

library(tidyverse)
library(httr)
library(lubridate)
library(progress)

options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)


# Configuration
## config <- list(
##   base_url = "https://raw.githubusercontent.com/cdcgov/covid19-forecast-hub/main/model-output",
##   forecasters = c("CMU-TimeSeries", "CovidHub-baseline", "CovidHub-ensemble"),
##   s3_bucket = "forecasting-team-data",
##   s3_key = "covid/covid_hosp_forecasts.parquet",
##   disease = "covid"
## )
# same but for flu
config <- list(
  base_url = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/model-output",
  forecasters = c("FluSight-baseline", "FluSight-ensemble", "CMU-TimeSeries"),
  s3_bucket = "forecasting-team-data",
  s3_key = "flu/flu_hosp_forecasts.parquet",
  disease = "flu"
)


# Function to check if file exists on GitHub
check_github_file <- function(forecaster, filename) {
  url <- paste0(config$base_url, "/", forecaster, "/", filename)
  response <- GET(url)
  return(status_code(response) == 200)
}

# Function to download and read a single file
download_forecast_file <- function(forecaster, filename) {
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
fetch_forecast_files <- function(days_back = 7 * 4 * 5, sync_to_s3 = TRUE) {
  # Generate date range
  # First get the nearest Saturday
  end_date <- round_date(Sys.Date(), "week", 6)
  start_date <- end_date - days_back

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
    dates <- seq(start_date, end_date, by = "week")
    filenames <- paste0(format(dates, "%Y-%m-%d"), "-", forecaster, ".csv")

    # Create nested progress bar for files
    pb_files <- progress_bar$new(
      format = "  Downloading files [:bar] :current/:total :filename",
      total = length(filenames)
    )

    for (filename in filenames) {
      pb_files$tick(tokens = list(filename = filename))

      if (check_github_file(forecaster, filename)) {
        forecast_data <- download_forecast_file(forecaster, filename)
        if (!is.null(forecast_data)) {
          all_forecasts[[length(all_forecasts) + 1]] <- forecast_data
        }
      }
    }
  }

  # Combine all forecasts
  combined_forecasts <- bind_rows(all_forecasts)

  if (sync_to_s3) {
    # Write to Parquet and upload to S3
    temp_file <- tempfile(fileext = ".parquet")
    write_parquet(combined_forecasts, temp_file)
    put_object(
      file = temp_file,
      object = config$s3_key,
      bucket = config$s3_bucket
    )
    unlink(temp_file)
  }

  return(combined_forecasts)
}

df <- fetch_forecast_files(sync_to_s3 = FALSE)
arrow::write_parquet(df, here::here(glue::glue("data/forecasts/{config$disease}_hosp_forecasts.parquet")))
