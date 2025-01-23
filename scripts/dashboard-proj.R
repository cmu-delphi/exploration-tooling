library(tidyverse)
library(httr)
library(lubridate)
library(progress)
library(targets)
source(here::here("R", "load_all.R"))

options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)

insufficient_data_geos <- c("as", "mp", "vi", "gu")

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

# Function to read all forecast data
read_all_forecasts <- function() {
  tracking_data <- read_csv(config$tracking_file)

  successful_downloads <- tracking_data %>%
    filter(status == "success")

  forecast_data <- map(1:nrow(successful_downloads), function(i) {
    row <- successful_downloads[i, ]
    path <- file.path(config$local_storage, row$forecaster, row$filename)
    if (file.exists(path)) {
      read_csv(path, col_types = list(
        reference_date = col_date(format = "%Y-%m-%d"),
        target_end_date = col_date(format = "%Y-%m-%d"),
        target = col_character(),
        location = col_character(),
        horizon = col_integer(),
        output_type = col_character(),
        output_type_id = col_character(),
        value = col_double(),
        forecaster = col_character(),
        forecast_date = col_date(format = "%Y-%m-%d")
      )) %>%
        mutate(
          forecaster = row$forecaster,
          forecast_date = as.Date(str_extract(row$filename, "\\d{4}-\\d{2}-\\d{2}")),
        )
    }
  })

  bind_rows(forecast_data) %>%
    add_state_info(geo_value_col = "location", old_geo_code = "state_code", new_geo_code = "state_id") %>%
    rename(geo_value = state_id) %>%
    select(-location) %>%
    filter(
      target == "wk inc flu hosp",
      output_type == "quantile",
    )
}

score_forecasts <- function(all_forecasts, nhsn_latest_data) {
  predictions_cards <- all_forecasts %>%
    rename(model = forecaster) %>%
    mutate(
      quantile = as.numeric(output_type_id),
      prediction = value
    ) %>%
    select(model, geo_value, forecast_date, target_end_date, quantile, prediction)

  truth_data <- nhsn_latest_data %>%
    mutate(
      target_end_date = as.Date(time_value),
      true_value = value
    ) %>%
    select(geo_value, target_end_date, true_value)

  evaluate_predictions(predictions_cards = predictions_cards, truth_data = truth_data) %>%
    rename(forecaster = model)
}

get_latest_data <- function() {
  update_forecast_files(days_back = 120)
  read_all_forecasts()
}

rlang::list2(
  tar_target(
    nhsn_latest_data,
    command = {
      if (wday(Sys.Date()) < 6 & wday(Sys.Date()) > 3) {
        # download from the preliminary data source from Wednesday to Friday
        most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
      } else {
        most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
      }
      most_recent_result %>%
        process_nhsn_data() %>%
        filter(disease == "nhsn_flu") %>%
        select(-disease) %>%
        filter(geo_value %nin% insufficient_data_geos) %>%
        mutate(
          source = "nhsn",
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value
        ) %>%
        filter(version == max(version)) %>%
        select(-version) %>%
        data_substitutions(disease = "flu") %>%
        as_epi_df(other_keys = "source", as_of = Sys.Date())
    }
  ),
  tar_target(
    name = nhsn_archive_data,
    command = {
      create_nhsn_data_archive(disease = "nhsn_flu")
    }
  ),
  tar_target(download_forecasts, update_forecast_files(days_back = 120)),
  tar_target(all_forecasts, read_all_forecasts()),
  tar_target(all_scores, score_forecasts(all_forecasts, nhsn_latest_data))
)
