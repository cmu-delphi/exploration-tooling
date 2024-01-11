#' Manage S3 cache
#' @param rel_cache_dir The relative path to the cache directory, e.g.
#' "data-processed/2021-09-01". Default is `"{tar_project}/objects"`
#' @param bucket_name The name of the S3 bucket to sync.
#' @param direction Set 'download' to download files, 'upload' to upload files,
#'   and `sync` to do both
#' @param verbose Set to TRUE to print the files being synced.
#' @param prefix specify the prefix for `s3sync`, which filters down which files
#'   to sync to those starting with `prefix`.
#' @param tar_project which targets project we're working on
#' @importFrom aws.s3 s3sync get_bucket
#' @importFrom here here
#' @export
manage_S3_forecast_cache <- function(rel_cache_dir = NULL,
                                     bucket_name = "forecasting-team-data",
                                     direction = "sync",
                                     verbose = FALSE,
                                     prefix = Sys.getenv("AWS_S3_PREFIX", "exploration"),
                                     tar_project = Sys.getenv("TAR_PROJECT", "")) {
  if (is.null(rel_cache_dir)) {
    cache_path <- tar_project
  } else {
    cache_path <- here(rel_cache_dir)
  }
  if (!dir.exists(cache_path)) dir.create(cache_path)

  full_prefix <- paste0(prefix, "/", tar_project, "/")
  s3b <- get_bucket(bucket_name, prefix = full_prefix)
  print(paste("local:", cache_path))
  print(paste("remote:", prefix))
  if (direction == "sync") {
    if (verbose) {
      s3sync(cache_path, s3b, prefix = full_prefix)
    } else {
      sink("/dev/null")
      s3sync(cache_path, s3b, prefix = full_prefix, verbose = FALSE)
      sink()
    }
  } else {
    if (verbose) {
      s3sync(cache_path, s3b, prefix = full_prefix, direction = direction)
    } else {
      sink("/dev/null")
      s3sync(cache_path, s3b, prefix = full_prefix, direction = direction, verbose = FALSE)
      sink()
    }
  }
  s3b_free <- get_bucket(bucket_name, prefix = prefix, max = 1)
  aws.s3::save_object(paste0(prefix, "/", external_scores_path), s3b_free)
  return(TRUE)
}
