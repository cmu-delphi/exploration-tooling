library(aws.s3)

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
                     external_scores_path = Sys.getenv("EXTERNAL_SCORES_PATH", ""),
                     aux_data_path = Sys.getenv("AUX_DATA_PATH", "")) {
  if (is.null(rel_cache_dir)) {
    cache_path <- tar_project
  } else {
    cache_path <- here::here(rel_cache_dir)
  }

  if (!dir.exists(cache_path)) dir.create(cache_path)

  project_prefix <- paste0(prefix, "/", tar_project, "/")
  cli::cli_inform(c(
    "{direction}ing cache to S3 bucket '{bucket_name}'",
    "i" = "Local cache path: {cache_path}",
    "i" = "AWS prefix: {prefix}"
  ))

  if (direction == "sync") {
    if (verbose) {
      s3sync(cache_path, bucket_name, prefix = project_prefix)
    } else {
      sink("/dev/null")
      s3sync(cache_path, bucket_name, prefix = project_prefix, verbose = FALSE)
      sink()
    }
  } else {
    if (verbose) {
      s3sync(cache_path, bucket_name, prefix = project_prefix, direction = direction)
    } else {
      sink("/dev/null")
      s3sync(cache_path, bucket_name, prefix = project_prefix, direction = direction, verbose = FALSE)
      sink()
    }
  }

  # sync external score file if it exists
  if ((is.null(external_scores_path)) && (external_scores_path != "") && external_scores_path != " ") {
    aws.s3::save_object(paste0(project_prefix, "/", external_scores_path), bucket_name)
  }
  if (aux_data_path != "" && aux_data_path != " ") {
    if (!dir.exists(here::here(aux_data_path))) dir.create(here::here(aux_data_path))
    if (verbose) {
      aws.s3::s3sync(
        here::here(aux_data_path), bucket_name,
        prefix = paste0(prefix, "/", aux_data_path), direction = direction
      )
    } else {
      sink("/dev/null")
      aws.s3::s3sync(
        here::here(aux_data_path), bucket_name,
        prefix = paste0(prefix, "/", aux_data_path, "/"), direction = direction, verbose = FALSE
      )
      sink()
    }
  }

  return(TRUE)
}
