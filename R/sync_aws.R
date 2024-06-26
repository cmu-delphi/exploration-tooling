#' Sync AWS S3 cache
#'
#' @param rel_cache_dir The relative path to the cache directory, e.g.
#' "data-processed/2021-09-01". Default is `"{tar_project}/objects"`
#' @param bucket_name The name of the S3 bucket to sync.
#' @param direction Set 'download' to download files, 'upload' to upload files,
#'   and `sync` to do both
#' @param verbose Set to TRUE to print the files being synced.
#' @param prefix specify the prefix for `s3sync`, which filters down which files
#'   to sync to those starting with `prefix`.
#' @param tar_project which targets project we're working on
#' @param external_scores_path which external scores file to sync
#'
#' @importFrom aws.s3 s3sync get_bucket
#' @importFrom here here
#' @export
sync_aws <- function(rel_cache_dir = NULL,
                     bucket_name = "forecasting-team-data",
                     direction = "sync",
                     verbose = FALSE,
                     prefix = Sys.getenv("AWS_S3_PREFIX", "exploration"),
                     tar_project = Sys.getenv("TAR_PROJECT", ""),
                     external_scores_path = Sys.getenv("EXTERNAL_SCORES_PATH", "")) {
  if (is.null(rel_cache_dir)) {
    cache_path <- tar_project
  } else {
    cache_path <- here(rel_cache_dir)
  }
  if (!dir.exists(cache_path)) dir.create(cache_path)

  project_prefix <- paste0(prefix, "/", tar_project, "/")
  s3b <- get_bucket(bucket_name, prefix = project_prefix)
  cli::cli_inform(c(
    "{direction}ing cache to S3 bucket '{bucket_name}'",
    "i" = "Local cache path: {cache_path}",
    "i" = "AWS prefix: {prefix}"
  ))
  if (direction == "sync") {
    if (verbose) {
      s3sync(cache_path, s3b, prefix = project_prefix)
    } else {
      sink("/dev/null")
      s3sync(cache_path, s3b, prefix = project_prefix, verbose = FALSE)
      sink()
    }
  } else {
    if (verbose) {
      s3sync(cache_path, s3b, prefix = project_prefix, direction = direction)
    } else {
      sink("/dev/null")
      s3sync(cache_path, s3b, prefix = project_prefix, direction = direction, verbose = FALSE)
      sink()
    }
  }
  # sync external score file if it exists
  if ((is.null(external_scores_path)) && (external_scores_path != "") && external_scores_path != " ") {
    s3b <- get_bucket(bucket_name, prefix = prefix, max = 1)
    aws.s3::save_object(paste0(project_prefix, "/", external_scores_path), s3b)
  }
  return(TRUE)
}
