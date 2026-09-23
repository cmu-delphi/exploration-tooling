MIN_TIMESTAMP <- as.POSIXct("2000-01-01 00:00:00S", tz = "UTC")

get_local_file_last_modified <- function(file_path, missing_value = MIN_TIMESTAMP) {
  if (!file.exists(file_path)) {
    return(missing_value)
  }
  file.info(file_path)$mtime %>% as.POSIXct(tz = "UTC")
}

#' Get the last modified date of an S3 object
#'
#' @param bucket The name of the S3 bucket.
#' @param key The key of the S3 object.
#'
#' @return The last modified date of the S3 object in POSIXct format.
get_s3_object_last_modified <- function(key, bucket, missing_value = MIN_TIMESTAMP) {
  metadata <- suppressMessages(head_object(key, bucket = bucket))
  if (!metadata) {
    return(missing_value)
  }
  # Format looks like "Fri, 31 Jan 2025 22:01:16 GMT"
  attr(metadata, "last-modified") %>%
    str_replace_all(" GMT", "") %>%
    as.POSIXct(format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
}

#' Get the last updated date of a Socrata dataset
#'
#' FYI: This hits a cache layer, which is only updated ~every 4 hours.
#'
#' @param dataset_url The URL of the Socrata dataset.
#'
#' @return The last updated date of the Socrata dataset in POSIXct format.
get_socrata_updated_at <- function(dataset_url, missing_value) {
  tryCatch(
    {
      rowsUpdatedAt <- httr::with_config(
        httr::config(timeout = 5),
        httr::RETRY("GET", dataset_url, times = 5, pause_min = 5, pause_cap = 5)
      ) %>%
        httr::content() %>%
        # This field comes in as integer seconds since epoch, so we need to convert it.
        pluck("rowsUpdatedAt")
      if (is.null(rowsUpdatedAt)) {
        return(missing_value)
      }
      rowsUpdatedAt %>% as.POSIXct(origin = "1970-01-01", tz = "UTC")
    },
    error = function(cond) {
      return(missing_value)
    }
  )
}

#' Check whether NHSN and NSSP source data is fresh enough to forecast on
#'
#' Builds (or confirms up to date) the `nhsn_archive_data` and
#' `nssp_archive_data` targets for the active `targets` project (selected via
#' `TAR_PROJECT`/`TAR_CONFIG`, see `tar_config_get()`), then checks the latest
#' `time_value` actually present in each archive against the current date.
#' Used by the production forecast cron/systemd job to decide whether to
#' proceed with a forecast run or wait for newer data, rather than submitting
#' a forecast built on stale inputs. Because these targets are `tar_change()`
#' targets keyed on the upstream API's latest-update date, this also takes
#' care of re-fetching them if newer data has appeared upstream.
#'
#' @param max_age_days Maximum allowed age, in days, of the latest time_value
#'   before the data is considered stale.
#' @return TRUE if both NHSN and NSSP archives have a time_value within
#'   `max_age_days`, FALSE otherwise.
check_data_freshness <- function(max_age_days = 7) {
  targets::tar_make(names = targets::any_of(c("nhsn_archive_data", "nssp_archive_data")))
  nhsn_archive <- targets::tar_read(nhsn_archive_data)
  nssp_archive <- targets::tar_read(nssp_archive_data)
  nhsn_latest <- max(nhsn_archive$DT$time_value)
  nssp_latest <- max(nssp_archive$DT$time_value)
  nhsn_age <- as.numeric(Sys.Date() - nhsn_latest)
  nssp_age <- as.numeric(Sys.Date() - nssp_latest)
  cli::cli_inform("NHSN latest time_value: {nhsn_latest} ({nhsn_age} days old)")
  cli::cli_inform("NSSP latest time_value: {nssp_latest} ({nssp_age} days old)")
  nhsn_age <= max_age_days && nssp_age <= max_age_days
}
