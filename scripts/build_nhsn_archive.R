# NHSN Archive Builder
#
# This script is meant to run every minute. It checks if the source data has
# been updated and updates the archive if so.
#
# It assumes you have the following credentials in your environment:
# - AWS_ACCESS_KEY_ID
# - AWS_SECRET_ACCESS_KEY
#
# It is meant to be general and usable for other data source projects. Adapting
# it to other data sources you will need to consider:
# 1. The new data source URL
# 2. A way to capture the last updated timestamp on the new data source
# 3. The format of the data
# 4. Your S3 bucket storage layout
#
# The crontab (every minute):
# * * * * * cd /path/to/root/of/this/project && R -q -f scripts/nhsn_download.R >> cache/nhsn_download.log 2>&1

library(aws.s3)
library(cli)
library(epiprocess)
library(glue)
library(here)
library(httr)
library(nanoparquet)
library(qs)
library(tidyverse)

# Needed for: get_s3_object_last_modified, get_socrata_updated_at
source("R/aux_data_utils.R")


config <- list(
  raw_query_url = "https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm",
  prelim_query_url = "https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm",
  raw_metadata_url = "https://data.cdc.gov/api/views/ua7e-t2fy",
  prelim_metadata_url = "https://data.cdc.gov/api/views/mpgq-jmmr",
  raw_file_name_prefix = "nhsn_data_raw",
  s3_bucket = "forecasting-team-data",
  archive_s3_key = "nhsn_data_archive.parquet"
)


#' Download the latest NHSN data from Socrata
#'
#' This function downloads the latest NHSN data from Socrata, if it has been
#' updated, and saves it to the raw data folder. It is assumed that you will run this
#' every minute or so, to make sure you are always working with the latest data.
#'
#' @param verbose Whether to print verbose output.
update_nhsn_data_raw <- function(verbose = FALSE) {
  # Get the last updated metadata for the archive and the raw and prelim data
  last_updated_at <- get_s3_object_last_modified(config$archive_s3_key, config$s3_bucket)
  raw_update_at <- get_socrata_updated_at(config$raw_metadata_url)
  prelim_update_at <- get_socrata_updated_at(config$prelim_metadata_url)
  raw_update_at_local <- with_tz(raw_update_at)
  prelim_update_at_local <- with_tz(prelim_update_at)

  if (raw_update_at > last_updated_at) {
    if (verbose) {
      cli_inform("The raw data has been updated at {raw_update_at_local} (UTC: {raw_update_at}).")
      cli_inform("Downloading the raw data...")
    }
    format_time <- format(with_tz(Sys.time(), tzone = "UTC"), "%Y-%m-%d_%H-%M-%OS5")
    raw_file <- glue("{config$raw_file_name_prefix}_{format_time}.parquet")
    read_csv(config$raw_query_url) %>% s3write_using(write_parquet, object = raw_file, bucket = config$s3_bucket)
  }

  if (prelim_update_at > last_updated_at) {
    if (verbose) {
      cli_inform("The prelim data has been updated at {prelim_update_at_local} (UTC: {prelim_update_at}).")
      cli_inform("Downloading the prelim data...")
    }
    format_time <- format(with_tz(Sys.time(), tzone = "UTC"), "%Y-%m-%d_%H-%M-%OS5")
    prelim_file <- glue("{config$raw_file_name_prefix}_{format_time}_prelim.parquet")
    read_csv(config$prelim_query_url) %>% s3write_using(write_parquet, object = prelim_file, bucket = config$s3_bucket)
  }
}

#' Process Raw NHSN Data File
#'
#' Download and process raw NHSN data into a tidy format with the following columns:
#' - geo_value: the jurisdiction of the data
#' - disease: the disease of the data
#' - time_value: the date of the data
#' - value: the value of the data
#' - version: the version of the data
#' - version_timestamp: the timestamp of the version
#'
process_nhsn_data_file <- function(key) {
  tryCatch(
    {
      version_timestamp <- get_version_timestamp(key)
      res <- s3read_using(read_parquet, object = key, bucket = config$s3_bucket) %>%
        mutate(
          geo_value = tolower(jurisdiction),
          time_value = as.Date(weekendingdate),
          nhsn_covid = totalconfc19newadm,
          nhsn_flu = totalconfflunewadm,
          version_timestamp = version_timestamp,
          version = as.Date(version_timestamp),
          geo_value = ifelse(geo_value == "usa", "us", geo_value)
        ) %>%
        select(-weekendingdate, -jurisdiction, -starts_with("totalconf")) %>%
        pivot_longer(cols = starts_with("nhsn"), names_to = "disease") %>%
        filter(!is.na(value)) %>%
        relocate(geo_value, disease, time_value)
      return(res)
    },
    error = function(cond) {
      cli_warn("Error processing {key}: {cond}")
      return(NULL)
    }
  )
}

# for filenames of the form nhsn_data_2024-11-19_16-29-43.191649.rds
get_version_timestamp <- function(filename) ymd_hms(str_match(filename, "[0-9]{4}-..-.._..-..-..\\.[^.^_]*"))

# Convert RDS files to Parquet files
df <- get_bucket_df(bucket = config$s3_bucket, prefix = 'nhsn_data_') %>%
  filter(grepl("rds", Key))
df$Key %>% map(function(key) {
  s3load(key, bucket = config$s3_bucket)
  if (grepl("prelim", key)) {
    out <- epi_data_raw_prelim
  } else {
    out <- epi_data_raw
  }
  new_key <- str_replace(key, "nhsn_data_", "nhsn_data_raw_")
  new_key <- str_replace(new_key, ".rds", ".parquet")
  out %>% s3write_using(write_parquet, object = new_key, bucket = config$s3_bucket)
})

#' Update NHSN archive from raw files.
#'
#' This function considers all the raw data files stored in S3 and creates or
#' updates a historical archive of the data, keeping only the latest version per
#' day. The archive has the columns geo_value, time_value, disease, endpoint
#' (either basic or prelim), version, version_timestamp (to enable keeping the
#' most recent value), and value.
update_nhsn_data_archive <- function(verbose = FALSE) {
  # Get the last timestamp of the archive
  last_timestamp <- get_s3_object_last_modified(config$archive_s3_key, config$s3_bucket)

  # Get a list of all new dataset snapshots from S3
  new_data_files <- get_bucket_df(bucket = config$s3_bucket, prefix = config$raw_file_name_prefix) %>%
    mutate(version_timestamp = get_version_timestamp(Key), version = as.Date(version_timestamp)) %>%
    filter(version_timestamp > last_timestamp) %>%
    as_tibble()

  # Filter to just the latest version_timestamp for each version date.
  new_data_files_latest_per_day <- new_data_files %>%
    group_by(version) %>%
    slice_max(version_timestamp) %>%
    ungroup()

  # If there were ties in the version_timestamp, keep the prelim data.
  new_data_files_latest_per_day <- new_data_files_latest_per_day %>%
    mutate(is_prelim = grepl("prelim", Key)) %>%
    group_by(version) %>%
    slice_max(is_prelim) %>%
    ungroup()

  if (length(new_data_files_latest_per_day) == 0) {
    if (verbose) {
      cli_inform("No new NHSN data found, no updates needed.")
    }
    return(invisible(NULL))
  }

  if (verbose) {
    cli_inform("Processing {nrow(new_data_files_latest_per_day)} new NHSN datasets.")
  }

  # Process each new dataset snapshot
  new_data <- new_data_files_latest_per_day$Key %>%
    map(process_nhsn_data_file, .progress = verbose) %>%
    bind_rows()

  # Look through the existing archive to see if there were updates today. If so, replace them with the new data.
  if (object_exists(config$archive_s3_key, bucket = config$s3_bucket)) {
    previous_archive <- s3read_using(read_parquet, object = config$archive_s3_key, bucket = config$s3_bucket)
    older_versions <- previous_archive %>% filter(!(version %in% unique(new_data$version)))
    new_archive <- bind_rows(older_versions, new_data)
  } else {
    new_archive <- new_data
  }

  if (verbose) {
    cli_inform("Saving new archive to S3.")
  }
  new_archive %>%
    arrange(disease, geo_value, time_value, version_timestamp) %>%
    s3write_using(write_parquet, object = config$archive_s3_key, bucket = config$s3_bucket)
}

update_nhsn_data <- function(verbose = FALSE) {
  time <- with_tz(Sys.time(), tzone = "UTC")
  local_time <- with_tz(time, tzone = "America/New_York")
  cli_inform(glue("Updating NHSN data at {local_time} (UTC: {time})..."))
  update_nhsn_data_raw(verbose = verbose)
  update_nhsn_data_archive(verbose = verbose)
}

