rename_forecasters <- function(df) {
  dplyr::mutate(df, forecaster = dplyr::case_match(
    forecaster,
    "windowed_seasonal_extra_sources" ~ "windowed_seasonal_nssp",
    "ensemble_linclim_windowed_seasonal" ~ "retro_submission",
    "ens_ar_only" ~ "ensemble_windowed",
    .default = forecaster
  ))
}

cache_dir <- file.path(getwd(), "_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
cache_key <- "2026-04-27"
cached_load <- function(name, fn) {
  path <- file.path(cache_dir, paste0(name, "_", cache_key, ".rds"))
  if (file.exists(path)) readRDS(path) else { result <- fn(); saveRDS(result, path); result }
}
s3_read <- function(object, bucket = "forecasting-team-data") {
  aws.s3::s3read_using(qs2::qs_read, object = object, bucket = bucket)
}
