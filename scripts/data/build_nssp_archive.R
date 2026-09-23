  # Archive Builder
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
# * * * * * cd /path/to/root/of/this/project && R -s -q -f scripts/build_nssp_archive.R >> cache/build_nssp_archive.log 2>&1
suppressPackageStartupMessages({
  library(aws.s3)
  library(cli)
  library(epiprocess)
  library(glue)
  library(here)
  library(httr)
  library(nanoparquet)
  library(tidyverse)
  # Needed for: get_s3_object_last_modified, get_socrata_updated_at, MIN_TIMESTAMP
  source("R/utils.R")
})


# Suppresses read_csv progress and column type messages
options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)
options(cli.width = 120)

# Script run time
run_time <- with_tz(Sys.time(), tzone = "UTC")
run_time_local <- with_tz(run_time)

DATA_SOURCE_NAME <- "nssp"
# Configuration
config <- list(
  query_url = "https://data.cdc.gov/resource/rdmq-nq56.csv?$limit=10000000",
  metadata_url = "https://data.cdc.gov/api/views/rdmq-nq56",
  file_name_prefix = glue("{DATA_SOURCE_NAME}_rdmq_nq56"),
  s3_bucket = "forecasting-team-data",
  local_cache_path = glue("cache/{DATA_SOURCE_NAME}_cache"),
  local_archive_path = glue("cache/{DATA_SOURCE_NAME}_data_archive.parquet")
)


# for filenames of the form prefix_20241119T162943.rds
get_version_timestamp <- function(filename) ymd_hms(str_match(filename, "\\d{8}T\\d{6}"))

get_s3_updated_at <- function(missing_value = MIN_TIMESTAMP) {
  tryCatch(
    {
      out <- get_bucket_df(bucket = config$s3_bucket, prefix = config$file_name_prefix) %>%
        as_tibble() %>%
        mutate(version_timestamp = get_version_timestamp(Key)) %>%
        slice_max(version_timestamp) %>%
        pull(version_timestamp)

      if (length(out) == 0) {
        return(missing_value)
      } else {
        return(out)
      }
    },
    error = function(cond) {
      cli_warn("Error getting last update at: {cond}")
      return(missing_value)
    }
  )
}

#' Sync raw data files from S3 to local cache
#'
#' Downloads any files from S3 that don't exist in the local cache,
#' ensuring the cache is up to date before processing.
sync_s3_to_local_cache <- function(bucket = config$s3_bucket,
                                   prefix = config$file_name_prefix,
                                   local_cache_path = config$local_cache_path,
                                   verbose = TRUE) {
  # Ensure cache directory exists
  cache_dir <- here::here(local_cache_path)
  if (!dir.exists(cache_dir)) {
    if (verbose) cli_inform("Creating cache directory: {cache_dir}")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Get list of files from S3
  s3_files <- tryCatch(
    {
      get_bucket_df(bucket = bucket, prefix = prefix) %>%
        pull(Key)
    },
    error = function(cond) {
      cli_warn("Error listing S3 files: {cond}")
      return(character(0))
    }
  )

  if (length(s3_files) == 0) {
    if (verbose) cli_inform("No files found in S3 with prefix {prefix}")
    return(invisible(NULL))
  }

  # Get list of local files
  local_files <- list.files(cache_dir, full.names = FALSE)

  # Find files that need to be downloaded
  files_to_download <- setdiff(s3_files, local_files)

  if (length(files_to_download) == 0) {
    if (verbose) cli_inform("Local cache is up to date ({length(s3_files)} files)")
    return(invisible(NULL))
  }

  if (verbose) {
    cli_inform("Syncing {length(files_to_download)} file{?s} from S3 to local cache...")
  }

  # Download each missing file
  downloaded <- 0
  failed <- 0

  for (file in files_to_download) {
    local_path <- here::here(cache_dir, file)

    download_result <- tryCatch(
      {
        save_object(object = file, bucket = bucket, file = local_path)
        downloaded <- downloaded + 1
        if (verbose) cli_inform("  Downloaded: {file}")
        TRUE
      },
      error = function(cond) {
        cli_warn("  Failed to download {file}: {cond}")
        failed <- failed + 1
        FALSE
      }
    )
  }

  if (verbose) {
    cli_inform("Sync complete: {downloaded} downloaded, {failed} failed")
  }

  return(invisible(list(
    downloaded = downloaded,
    failed = failed,
    total_local = length(s3_files) - failed
  )))
}


#' Download the latest data from Socrata
#'
#' This function downloads the latest data from Socrata, if it has been
#' updated, and saves it to the raw data folder. It is assumed that you will run this
#' every minute or so, to make sure you are always working with the latest data.
#'
#' @param verbose Whether to print verbose output.
update_data_raw <- function() {
  # WARNING: Socrata metadata fields have been unreliable. If they fail, they
  # default to current time, which will trigger a download and then we compare
  # with hash archive.

  # Get the current time in UTC for logging.
  current_time <- with_tz(Sys.time(), tzone = "UTC")

  # Get the file hashes.
  hash_archive <- aws.s3::get_bucket_df(
    bucket = config$s3_bucket,
    prefix = config$file_name_prefix
  ) %>%
    select(filename = Key, hash = ETag)

  # Get the last time the raw data was updated from Socrata.
  socrata_update_at <- get_socrata_updated_at(config$metadata_url, missing_value = current_time)
  s3_update_at <- get_s3_updated_at()
  # If the raw data has been updated or there was a failure getting metadata,
  # download it.
  if (socrata_update_at > s3_update_at) {
    socrata_update_at_local <- with_tz(socrata_update_at)
    socrata_update_at_formatted <- format(socrata_update_at, "%Y%m%dT%H%M%S")
    socrata_file <- glue("{config$file_name_prefix}_{socrata_update_at_formatted}.parquet")
    local_file_path <- here::here(config$local_cache_path, socrata_file)
    cli_inform("The raw data has been updated at {socrata_update_at_local} (UTC: {socrata_update_at}).")
    cli_inform("Downloading the raw data... {socrata_file}")
    read_csv(config$query_url) %>% write_parquet(local_file_path)

    # Get the hash of the raw file. As long as the file uploads are not
    # multi-part, S3 ETag is just the MD5 hash of the file contents and that's
    # what we compute. If they do diverge, then we'll just save a lot of files
    # and at some deduplicate, so not a big deal.
    file_hash <- get_file_hash(local_file_path)

    # If the raw file hash is not in the archive, add it to S3 and local file.
    if (!file_hash %in% hash_archive$hash) {
      cli_inform("Adding raw file to S3 and local cache.")
      put_object(file = local_file_path, object = socrata_file, bucket = config$s3_bucket)
    } else {
      cli_inform("New raw file is a duplicate, removing from local cache.")
      unlink(local_file_path)
    }
  }
}


update_data <- function(verbose = FALSE) {
  if (verbose) {
    cli_inform(glue("Checking for updates to NHSN data at {run_time_local} (UTC: {run_time})..."))
  }

  # Sync S3 files to local cache before processing
  sync_s3_to_local_cache(verbose = verbose)

  update_data_raw()
}

update_data(verbose = TRUE)
