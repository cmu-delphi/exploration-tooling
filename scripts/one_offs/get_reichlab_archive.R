#!/usr/bin/env Rscript

# Script to download NHSN CSV files from a specific date range
# Pattern: https://infectious-disease-data.s3.amazonaws.com/data-raw/influenza-nhsn/nhsn-YYYY-MM-DD.csv

# Load required packages
library(aws.s3)
library(cli)
library(dplyr)
library(epiprocess)
library(httr)
library(lubridate)
library(nanoparquet)
library(purrr)

options(readr.show_col_types = FALSE)
options(readr.show_progress = FALSE)

# Configuration
config <- list(
  base_url = "https://infectious-disease-data.s3.amazonaws.com/",
  file_path = "data-raw/influenza-nhsn/",
  file_prefix = "nhsn-",
  file_suffix = ".csv",
  start_date = ymd("2024-11-01"),
  end_date = ymd("2025-02-24"),
  download_dir = "cache/reichlab_archive",
  output_file = "cache/reichlab_archive/reichlab_archive.parquet",
  timeout_seconds = 10
)
# Create date sequence from start_date to end_date
config$all_dates <- seq.Date(config$start_date, config$end_date, by = 1L)
# Format dates as YYYY-MM-DD
config$formatted_dates <- format(config$all_dates, "%Y-%m-%d")

# Create directory for downloads if it doesn't exist
dir.create(config$download_dir, recursive = TRUE, showWarnings = FALSE)

# Function to download a file with error handling
download_file <- function(date_str) {
  file_name <- paste0(config$file_prefix, date_str, config$file_suffix)
  full_url <- paste0(config$base_url, config$file_path, file_name)
  local_path <- file.path(config$download_dir, file_name)

  # Try to download the file
  tryCatch(
    {
      response <- readr::read_csv(full_url, show_col_types = FALSE)

      # Check if download was successful
      if (nrow(response) > 0) {
        cli::cli_alert_success("Downloaded: {.file {file_name}}")
        readr::write_csv(response, local_path)
        return(TRUE)
      } else {
        cli::cli_alert_warning("File not found: {.file {file_name}} (Status code: {status_code(response)})")
        # Remove the empty file that was created
        return(FALSE)
      }
    },
    error = function(e) {
      cli::cli_alert_danger("Error downloading {.file {file_name}}: {e$message}")
      # Remove the empty file that was created
      if (file.exists(local_path)) file.remove(local_path)
      return(FALSE)
    }
  )
}

# Download all files
cli::cli_h1("Downloading Reichlab Archive")
cli::cli_alert_info("Date range: {config$start_date} to {config$end_date}")
cli::cli_alert_info("Checking {length(config$formatted_dates)} potential dates...")

# Download files with progress updates
results <- map_lgl(
  config$formatted_dates,
  function(date) {
    result <- download_file(date)
    return(result)
  },
  .progress = TRUE
)

# Summary
successful_downloads <- sum(results)
cli::cli_h2("Download Summary")
cli::cli_alert_info("Total dates checked: {length(config$formatted_dates)}")
cli::cli_alert_success("Successfully downloaded: {successful_downloads}")
cli::cli_alert_warning("Files not found or errors: {length(config$formatted_dates) - successful_downloads}")
cli::cli_alert_info("Downloaded files are in the {.file {config$download_dir}} directory")


process_csv <- function(file_path) {
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  process_nhsn_dataset(df)
}

process_nhsn_dataset <- function(df) {
  if ("Week Ending Date" %in% colnames(df)) {
    df <- df %>%
      rename(
        time_value = `Week Ending Date`,
        geo_value = `Geographic aggregation`,
        total_influenza_admissions = `Total Influenza Admissions`,
        total_covid_admissions = `Total COVID-19 Admissions`
      )
  } else if ("weekendingdate" %in% colnames(df)) {
    df <- df %>%
      rename(
        time_value = `weekendingdate`,
        geo_value = `jurisdiction`,
        total_influenza_admissions = `totalconfflunewadm`,
        total_covid_admissions = `totalconfc19newadm`
      )
  } else {
    stop("Unknown schema for file: ", file_path)
  }
  return(df %>% select(geo_value, time_value, total_influenza_admissions, total_covid_admissions))
}

cli::cli_h2("Combining Files")

# List all downloaded files
downloaded_files <- list.files(
  config$download_dir,
  pattern = paste0("^", config$file_prefix, ".*\\", config$file_suffix, "$"),
  full.names = TRUE
)

# Read and combine all files
combined_data <- map_df(
  downloaded_files,
  function(file) {
    data <- process_csv(file)
    data$source_file <- basename(file)
    return(data)
  },
  .progress = TRUE
)
# Use the source_file column to add a version column
combined_data %<>%
  mutate(
    version = as.Date(str_extract(source_file, "\\d{4}-\\d{2}-\\d{2}")),
    time_value = as.Date(time_value),
    geo_value = ifelse(geo_value == "USA", "us", tolower(geo_value))
  )
# Pivot longer to get a column for each disease
combined_data %<>% pivot_longer(cols = starts_with("total"), names_to = "disease")
combined_data %<>% arrange(disease, geo_value, time_value, version)
combined_data %<>% select(disease, geo_value, time_value, version, value)

# Write to parquet
combined_data %>% arrow::write_parquet(config$output_file)

cli::cli_alert_success("Combined data has {nrow(combined_data)} rows and {ncol(combined_data)} columns")
cli::cli_alert_success("Data written to {.file {config$output_file}}")
cli::cli_h1("Process Complete")


cli::cli_h1("Comparing archives")
df1 <- read_parquet(config$output_file) %>%
  filter(disease == "total_influenza_admissions") %>%
  filter(!is.na(value)) %>%
  select(-disease) %>%
  as_epi_archive(compactify = TRUE)
df2 <- s3read_using(read_parquet, object = "nhsn_data_archive.parquet", bucket = "forecasting-team-data") %>%
  filter(disease == "nhsn_flu", !grepl("region", geo_value)) %>%
  select(-disease, -version_timestamp) %>%
  as_epi_archive(compactify = TRUE)

# Compare versions
# as_ofs <- df1$DT %>% distinct(version) %>% pull(version)
# as_ofs <- as_ofs[2:length(as_ofs)]
# Forecast generation dates
as_ofs <- c(
  as.Date(c("2024-11-22", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
  seq.Date(as.Date("2025-01-08"), Sys.Date(), by = 7L)
)
as_of <- as_ofs[1]

for (as_of in as_ofs) {
  as_of <- as.Date(as_of)
  out1 <- df1 %>% epix_as_of(version = as_of)
  out2 <- df2 %>% epix_as_of(version = as_of)
  out <- identical(out1$value, out2$value)
  if (!out) {
    cli::cli_alert_danger("Snapshot not equal for {as_of}")
  } else {
    cli::cli_alert_success("Snapshot equal for {as_of}")
  }
  if (FALSE) {
    waldo::compare(out1, out2)
    out1 %>%
      filter(geo_value != "us") %>%
      autoplot()
    out2 %>%
      filter(geo_value != "us") %>%
      autoplot()
  }
}


if (FALSE) {
  # Get a Reichlab file
  df <- readr::read_csv("cache/reichlab_archive/nhsn-2024-11-20.csv") %>%
    select(
      weekendingdate = `Week Ending Date`,
      jurisdiction = `Geographic aggregation`,
      totalconfflunewadm = `Total Influenza Admissions`,
      totalconfc19newadm = `Total COVID-19 Admissions`
    )

  # Overwrite one of our raw files with it
  s3write_using(
    df,
    write_parquet,
    object = "nhsn_data_raw_2024-11-22_11-01-11.119035.parquet",
    bucket = "forecasting-team-data"
  )

  # Delete archive so it regenerates
  delete_object(object = "nhsn_data_archive.parquet", bucket = "forecasting-team-data")
}
