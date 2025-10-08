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
  git_root_url = "https://api.github.com/repos/cdcgov/covid19-forecast-hub/git/trees/main?recursive=1",
  forecasters = NULL, # pulled from API
  s3_bucket = "forecasting-team-data",
  s3_key = "exploration/2024-2025_covid_hosp_forecasts.parquet",
  s3_prefix = "exploration",
  disease = "covid",
  start_date = as.Date("2024-11-23")
)
# same but for flu
flu_config <- list(
  base_url = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/model-output",
  git_root_url = "https://api.github.com/repos/cdcepi/FluSight-forecast-hub/git/trees/main?recursive=1",
  forecasters = NULL, # pulled from API
  s3_bucket = "forecasting-team-data",
  s3_key = "exploration/2024-2025_flu_hosp_forecasts.parquet",
  s3_prefix = "exploration",
  disease = "flu",
  start_date = as.Date("2023-10-14")
)
get_all_forecasters <- function(git_url) {
  all_files <- GET(paste0(git_url))
  flat_file_list <- unlist(lapply(content(all_files)$tree, "[", "path"), use.names = FALSE)
  all_models <-
    grep("model-metadata", flat_file_list, value = TRUE, fixed = TRUE) %>%
    str_split_i("/", 2) %>%
    str_split_i(stringr::fixed("."), 1) %>%
    `[`(-(1:2))
}

covid_config$forecasters <- get_all_forecasters(covid_config$git_root_url)
flu_config$forecasters <- get_all_forecasters(flu_config$git_root_url)

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
          output_type_id = as.numeric(output_type_id),
          location = as.character(location)
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
fetch_forecast_files <- function(sync_to_s3 = TRUE, disease, redownload = FALSE) {
  if (disease == "covid") {
    config <- covid_config
  } else if (disease == "flu") {
    config <- flu_config
  } else {
    stop("Invalid disease")
  }

  # Generate date range
  # First get the nearest Saturday, and exclude this week if it's Wednesday or earlier
  dates <- seq(config$start_date, round_date(Sys.Date() - 3, "week", 6), by = "week")
  # filter out dates we've already downloaded
  if (!redownload) {
    file_date_present <- map_lgl(
      dates,
      \(forecast_date) {
        suppressMessages( # because verbose doesn't supress the 404 error.
          aws.s3::object_exists(
            paste0(config$s3_prefix, "/", forecast_date, "/", config$disease, "_forecasts.parquet"),
            config$s3_bucket,
            verbose = FALSE
          )
        )
      }
    )
    dates <- dates[!file_date_present]
  }

  pb_forecast_date <- progress_bar$new(
    format = "Downloading forecasts from :date [:bar] :percent :elapsedfull eta :eta",
    total = length(dates),
    clear = FALSE,
    width = 80
  )

  # Initialize list to store all forecasts
  all_forecasts <- list()
  for (forecast_date in dates) {
    forecast_date <- as.Date(forecast_date)
    pb_forecast_date$tick(
      tokens = list(date = forecast_date),
    )
    # Initialize list to store all forecasts
    date_forecasts <- list()
    for (forecaster in config$forecasters) {
      filename <- paste0(format(as.Date(forecast_date), "%Y-%m-%d"), "-", forecaster, ".csv")
      if (check_github_file(forecaster, filename, disease)) {
        # check if we even need to download
        forecast_data <- download_forecast_file(forecaster, filename, disease)
        if (!is.null(forecast_data)) {
          date_forecasts[[length(date_forecasts) + 1]] <- forecast_data
        }
      }
    }
    combined_forecasts <- bind_rows(date_forecasts)
    if (sync_to_s3 && length(combined_forecasts) > 0) {
      combined_forecasts %>%
        aws.s3::s3write_using(
          nanoparquet::write_parquet,
          object = paste0(config$s3_prefix, "/", forecast_date, "/", config$disease, "_forecasts.parquet"),
          bucket = config$s3_bucket
        )
    } else {
      aws.s3::s3save(
        combined_forecasts,
        object = paste0(config$s3_prefix, "/", forecast_date, "/", config$disease, "_forecasts.parquet"),
        bucket = config$s3_bucket
      )
    }
    all_forecasts[[length(all_forecasts) + 1]] <- combined_forecasts
  }
  return(bind_rows(all_forecasts))
}

cli::cli_alert_info("Fetching COVID forecasts {run_time_local} (UTC: {run_time})")
covid_forecasts <- fetch_forecast_files(disease = "covid")
cli::cli_alert_info("Fetching FLU forecasts {run_time_local} (UTC: {run_time})")
flu_forecasts <- fetch_forecast_files(disease = "flu")
print(glue::glue("Run successfully finished at {Sys.time()}"))
print("-------------------------------------------------------")
print("-------------------------------------------------------")
