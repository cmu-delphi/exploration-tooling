library(tidyverse)
library(httr)
library(lubridate)
library(progress)

options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)


# Configuration
config <- list(
  base_url = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/model-output",
  forecasters = c("CMU-TimeSeries", "FluSight-baseline", "FluSight-ensemble", "FluSight-base_seasonal", "UMass-flusion"),
  local_storage = "data/forecasts",
  tracking_file = "data/download_tracking.csv"
)

# Function to ensure directory structure exists
setup_directories <- function(base_dir) {
  dir.create(file.path(base_dir), recursive = TRUE, showWarnings = FALSE)
  for (forecaster in config$forecasters) {
    dir.create(file.path(base_dir, forecaster), recursive = TRUE, showWarnings = FALSE)
  }
}

# Function to load tracking data
load_tracking_data <- function() {
  if (file.exists(config$tracking_file)) {
    read_csv(config$tracking_file)
  } else {
    tibble(
      forecaster = character(),
      filename = character(),
      download_date = character(),
      status = character()
    )
  }
}

# Function to generate possible filenames for a date range
generate_filenames <- function(start_date, end_date, forecaster) {
  dates <- seq(as_date(start_date), as_date(end_date), by = "week")
  filenames <- paste0(
    format(dates, "%Y-%m-%d"),
    "-",
    forecaster,
    ".csv"
  )
  return(filenames)
}

# Function to check if file exists on GitHub
check_github_file <- function(forecaster, filename) {
  url <- paste0(config$base_url, "/", forecaster, "/", filename)
  response <- GET(url)
  return(status_code(response) == 200)
}

# Function to download a single file
download_forecast_file <- function(forecaster, filename) {
  url <- paste0(config$base_url, "/", forecaster, "/", filename)
  local_path <- file.path(config$local_storage, forecaster, filename)

  tryCatch(
    {
      download.file(url, local_path, mode = "wb", quiet = TRUE)
      return("success")
    },
    error = function(e) {
      return("failed")
    }
  )
}

# Main function to update forecast files
update_forecast_files <- function(days_back = 30) {
  # Setup
  setup_directories(config$local_storage)
  tracking_data <- load_tracking_data()

  # Generate date range
  end_date <- Sys.Date()
  start_date <- get_forecast_reference_date(end_date - days_back)

  # Process each forecaster
  new_tracking_records <- list()

  pb_forecasters <- progress_bar$new(
    format = "Downloading forecasts from :forecaster [:bar] :percent :eta",
    total = length(config$forecasters),
    clear = FALSE,
    width = 60
  )

  for (forecaster in config$forecasters) {
    pb_forecasters$tick(tokens = list(forecaster = forecaster))

    # Get potential filenames
    filenames <- generate_filenames(start_date, end_date, forecaster)

    # Filter out already downloaded files
    existing_files <- tracking_data %>%
      filter(forecaster == !!forecaster, status == "success") %>%
      pull(filename)

    new_files <- setdiff(filenames, existing_files)

    if (length(new_files) > 0) {
      # Create nested progress bar for files
      pb_files <- progress_bar$new(
        format = "  Downloading files [:bar] :current/:total :filename",
        total = length(new_files)
      )

      for (filename in new_files) {
        pb_files$tick(tokens = list(filename = filename))

        if (check_github_file(forecaster, filename)) {
          status <- download_forecast_file(forecaster, filename)

          new_tracking_records[[length(new_tracking_records) + 1]] <- tibble(
            forecaster = forecaster,
            filename = filename,
            download_date = as.character(Sys.time()),
            status = status
          )
        }
      }
    }
  }

  # Update tracking data
  if (length(new_tracking_records) > 0) {
    new_tracking_data <- bind_rows(new_tracking_records)
    tracking_data <- bind_rows(tracking_data, new_tracking_data)
    write_csv(tracking_data, config$tracking_file)
  }

  return(tracking_data)
}
