#' Manage S3 cache
#' @param rel_cache_dir The relative path to the cache directory, e.g.
#' "data-processed/2021-09-01".
#' @param bucket_name The name of the S3 bucket to sync.
#' @param direction Set 'download' to download files or 'upload' to upload
#' files.
#' @param verbose Set to TRUE to print the files being synced.
#'
#' @importFrom aws.s3 s3sync get_bucket
#' @importFrom here here
#' @export
manage_S3_forecast_cache <- function(rel_cache_dir, bucket_name = "forecasting-team-data", direction = "download", verbose = FALSE) {
  cache_path <- here(rel_cache_dir)
  if (!dir.exists(cache_path)) dir.create(cache_path)

  s3b <- get_bucket(bucket_name)
  if (verbose) {
    s3sync(cache_path, s3b, paste0("covid-hosp-forecast/", rel_cache_dir), direction = direction)
  } else {
    sink("/dev/null")
    s3sync(cache_path, s3b, paste0("covid-hosp-forecast/", rel_cache_dir), direction = direction, verbose = FALSE)
    sink()
  }
  return(TRUE)
}
