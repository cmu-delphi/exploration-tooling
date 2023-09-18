manage_S3_forecast_cache <- function(rel_cache_dir, bucket_name = "forecasting-team-data", direction = "download", verbose = FALSE) {
  cache_path <- here::here(rel_cache_dir)
  if (!dir.exists(cache_path)) dir.create(cache_path)

  s3b <- aws.s3::get_bucket(bucket_name)
  if (verbose) {
    aws.s3::s3sync(cache_path, s3b, paste0("covid-hosp-forecast/", rel_cache_dir), direction = direction)
  } else {
    sink("/dev/null")
    aws.s3::s3sync(cache_path, s3b, paste0("covid-hosp-forecast/", rel_cache_dir), direction = direction, verbose = FALSE)
    sink()
  }
  return(TRUE)
}
