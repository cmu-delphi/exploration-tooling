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
# * * * * * cd /path/to/root/of/this/project && R -s -q -f scripts/build_nhsn_archive.R >> cache/build_nhsn_archive.log 2>&1
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

# Configuration
config <- list(
  raw_query_url = "https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=50000",
  prelim_query_url = "https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=50000",
  raw_metadata_url = "https://data.cdc.gov/api/views/ua7e-t2fy",
  prelim_metadata_url = "https://data.cdc.gov/api/views/mpgq-jmmr",
  raw_file_name_prefix = "nhsn_data_raw",
  s3_bucket = "forecasting-team-data",
  local_raw_cache_path = "cache/nhsn_raw_cache",
  local_archive_path = "cache/nhsn_data_archive.parquet"
)


# for filenames of the form nhsn_data_2024-11-19_16-29-43.191649.rds
get_version_timestamp <- function(filename) ymd_hms(str_match(filename, "[0-9]{4}-..-.._..-..-..\\.[^.^_]*"))

get_last_raw_update_at <- function(type = c("raw", "prelim"), missing_value = MIN_TIMESTAMP) {
  type <- match.arg(type)
  tryCatch(
    {
      if (type == "raw") {
        get_bucket_df(bucket = config$s3_bucket, prefix = config$raw_file_name_prefix) %>%
          filter(!grepl("prelim", Key)) %>%
          mutate(version_timestamp = get_version_timestamp(Key)) %>%
          slice_max(version_timestamp) %>%
          pull(version_timestamp)
      } else {
        get_bucket_df(bucket = config$s3_bucket, prefix = config$raw_file_name_prefix) %>%
          filter(grepl("prelim", Key)) %>%
          mutate(version_timestamp = get_version_timestamp(Key)) %>%
          slice_max(version_timestamp) %>%
          pull(version_timestamp)
      }
    },
    error = function(cond) {
      cli_warn("Error getting last {type} update at: {cond}")
      return(missing_value)
    }
  )
}

#' Sync raw data files from S3 to local cache
#'
#' Downloads any files from S3 that don't exist in the local cache,
#' ensuring the cache is up to date before processing.
sync_s3_to_local_cache <- function(bucket = config$s3_bucket,
                                   prefix = config$raw_file_name_prefix,
                                   local_cache_path = config$local_raw_cache_path,
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


#' Download the latest NHSN data from Socrata
#'
#' This function downloads the latest NHSN data from Socrata, if it has been
#' updated, and saves it to the raw data folder. It is assumed that you will run this
#' every minute or so, to make sure you are always working with the latest data.
#'
#' @param verbose Whether to print verbose output.
update_nhsn_data_raw <- function() {
  # WARNING: Socrata metadata fields have been unreliable. If they fail, they
  # default to current time, which will trigger a download and then we compare
  # with hash archive.

  # Get the current time in UTC for logging.
  current_time <- with_tz(Sys.time(), tzone = "UTC")

  # Get the file hashes.
  hash_archive <- aws.s3::get_bucket_df(
    bucket = config$s3_bucket,
    prefix = config$raw_file_name_prefix
  ) %>%
    select(filename = Key, hash = ETag)

  # Get the last time the raw data was updated from Socrata.
  raw_update_at <- get_socrata_updated_at(config$raw_metadata_url, missing_value = current_time)
  last_raw_file_update_at <- get_last_raw_update_at("raw")
  # If the raw data has been updated or there was a failure getting metadata,
  # download it.
  if (raw_update_at > last_raw_file_update_at) {
    raw_update_at_local <- with_tz(raw_update_at)
    raw_update_at_formatted <- format(raw_update_at, "%Y-%m-%d_%H-%M-%OS5")
    raw_file <- glue("{config$raw_file_name_prefix}_{raw_update_at_formatted}.parquet")
    local_file_path <- here::here(config$local_raw_cache_path, raw_file)
    cli_inform("The raw data has been updated at {raw_update_at_local} (UTC: {raw_update_at}).")
    cli_inform("Downloading the raw data... {raw_file}")
    read_csv(config$raw_query_url) %>% write_parquet(local_file_path)

    # Get the hash of the raw file. As long as the file uploads are not
    # multi-part, S3 ETag is just the MD5 hash of the file contents and that's
    # what we compute. If they do diverge, then we'll just save a lot of files
    # and at some deduplicate, so not a big deal.
    raw_file_hash <- get_file_hash(local_file_path)

    # If the raw file hash is not in the archive, add it to S3 and local file.
    if (!raw_file_hash %in% hash_archive$hash) {
      cli_inform("Adding raw file to S3 and local cache.")
      put_object(file = local_file_path, object = raw_file, bucket = config$s3_bucket)
    } else {
      cli_inform("New raw file is a duplicate, removing from local cache.")
      unlink(local_file_path)
    }
  }

  # Get the last time the prelim data was updated from Socrata.
  prelim_update_at <- get_socrata_updated_at(config$prelim_metadata_url, missing_value = current_time)
  last_prelim_file_update_at <- get_last_raw_update_at("prelim")
  # If the prelim data has been updated or there was a failure getting metadata,
  # download it.
  if (prelim_update_at > last_prelim_file_update_at) {
    prelim_update_at_local <- with_tz(prelim_update_at)
    prelim_update_at_formatted <- format(prelim_update_at, "%Y-%m-%d_%H-%M-%OS5")
    prelim_file <- glue("{config$raw_file_name_prefix}_{prelim_update_at_formatted}_prelim.parquet")
    local_prelim_file_path <- here::here(config$local_raw_cache_path, prelim_file)
    cli_inform("The prelim data has been updated at {prelim_update_at_local} (UTC: {prelim_update_at}).")
    cli_inform("Downloading the prelim data... {prelim_file}")
    read_csv(config$prelim_query_url) %>% write_parquet(local_prelim_file_path)

    # Get the hash of the prelim file.
    prelim_file_hash <- get_file_hash(local_prelim_file_path)

    # If the prelim file hash is not in the archive, add it to S3 and local file.
    if (!prelim_file_hash %in% hash_archive$hash) {
      cli_inform("Adding prelim file to S3 and local cache.")
      put_object(file = local_prelim_file_path, object = prelim_file, bucket = config$s3_bucket)
    } else {
      cli_inform("New prelim file is a duplicate, removing from local cache.")
      unlink(local_prelim_file_path)
    }
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
      # Try to read from local cache first, fall back to S3 if not found
      local_file_path <- here::here(config$local_raw_cache_path, key)
      res <- tryCatch(
        {
          if (file.exists(local_file_path)) {
            read_parquet(local_file_path)
          } else {
            cli_warn("File {key} not in local cache, reading from S3")
            s3read_using(read_parquet, object = key, bucket = config$s3_bucket)
          }
        },
        error = function(cond) {
          cli_warn("Error reading {key} from local cache, trying S3: {cond}")
          s3read_using(read_parquet, object = key, bucket = config$s3_bucket)
        }
      )

      res <- res %>%
        mutate(
          geo_value = tolower(jurisdiction),
          time_value = as.Date(weekendingdate),
          nhsn_covid = totalconfc19newadm,
          nhsn_flu = totalconfflunewadm,
          version_timestamp = version_timestamp,
          version = as.Date(version_timestamp),
          geo_value = ifelse(geo_value == "usa", "us", geo_value)
        ) %>%
        select(geo_value, time_value, nhsn_covid, nhsn_flu, version, version_timestamp) %>%
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

#' Update NHSN archive from raw files.
#'
#' This function considers all the raw data files stored in local cache and creates or
#' updates a historical archive of the data, keeping only the latest version per
#' day. The archive has the columns geo_value, time_value, disease, endpoint
#' (either basic or prelim), version, version_timestamp (to enable keeping the
#' most recent value), and value.
update_nhsn_data_archive <- function() {
  # Get the last timestamp of the local archive
  archive_path <- here::here(config$local_archive_path)
  last_timestamp <- get_local_file_last_modified(archive_path, missing_value = MIN_TIMESTAMP)

  # Get a list of all new dataset snapshots from local cache
  cache_dir <- here::here(config$local_raw_cache_path)
  new_data_files <- tibble(
    Key = list.files(cache_dir, full.names = FALSE)
  ) %>%
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

  if (nrow(new_data_files_latest_per_day) == 0) {
    return(invisible(NULL))
  }

  cli_inform(
    "New datasets available at, adding {nrow(new_data_files_latest_per_day)} new NHSN datasets to the archive."
  )

  # Process each new dataset snapshot
  new_data <- new_data_files_latest_per_day$Key %>%
    map(process_nhsn_data_file, .progress = interactive()) %>%
    bind_rows()

  # Look through the existing archive to see if there were updates today. If so, replace them with the new data.
  if (file.exists(archive_path)) {
    previous_archive <- read_parquet(archive_path)
    older_versions <- previous_archive %>% filter(!(version %in% unique(new_data$version)))
    new_archive <- bind_rows(older_versions, new_data)
  } else {
    new_archive <- new_data
  }

  # Ensure archive directory exists
  archive_dir <- dirname(archive_path)
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Write archive to local file
  new_archive %>%
    arrange(disease, geo_value, time_value, version_timestamp) %>%
    write_parquet(archive_path)
}

update_nhsn_data <- function(verbose = FALSE) {
  if (verbose) {
    cli_inform(glue("Checking for updates to NHSN data at {run_time_local} (UTC: {run_time})..."))
  }

  # Sync S3 files to local cache before processing
  sync_s3_to_local_cache(verbose = verbose)

  update_nhsn_data_raw()
  update_nhsn_data_archive()
}

update_nhsn_data(verbose = TRUE)
