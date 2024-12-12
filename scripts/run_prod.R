#!/usr/bin/env Rscript
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# This is a helper script to run the pipeline. Choose how to execute the
# pipeline below. See https://books.ropensci.org/targets/hpc.html to learn
# about your options.
#
# Forecasters not run in `targets` can be included for comparison by providing
# the path to a score file.
#
#   # Example fetching external scores from the forecasting bucket
#   library(aws.s3)
#
#   # We recommend setting these in your user config files such as ~/.zprofile.
#   Sys.setenv(
#     AWS_ACCESS_KEY_ID = "",
#     AWS_SECRET_ACCESS_KEY = ""
#   )
#
#   s3b <- get_bucket("forecasting-team-data")
#
#   # Load object
#   scorecards <- s3readRDS(
#     object = "2023/exploration-scorecards-2023-10-04.RDS",
#     bucket = s3b
#   )
#   # Save to disk
#   saveRDS(scorecards, "exploration-scorecards-2023-10-04.RDS")
print(glue::glue("starting a run at {Sys.time()}"))
tar_project <- Sys.getenv("TAR_RUN_PROJECT", "flu_hosp_prod")
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
debug_mode <- as.logical(Sys.getenv("DEBUG_MODE", FALSE))
use_shiny <- as.logical(Sys.getenv("USE_SHINY", FALSE))
aws_s3_prefix <- Sys.getenv("AWS_S3_PREFIX", "exploration")
aws_s3_prefix <- paste0(aws_s3_prefix, "/", tar_project)
if (external_scores_path == "") {
  external_scores_path <- paste0(tar_project, "/", "legacy-exploration-scorecards.qs")
}
cli::cli_inform(
  c(
    "i" = "Reading environment variables...",
    "*" = "TAR_PROJECT = {tar_project}",
    "*" = "EXTERNAL_SCORES_PATH = {external_scores_path}",
    "*" = "DEBUG_MODE = {debug_mode}",
    "*" = "USE_SHINY = {use_shiny}",
    "*" = "AWS_S3_PREFIX = {aws_s3_prefix}"
  )
)

suppressPackageStartupMessages({
  library(targets)
  library(shiny)
})


# targets needs the output dir to already exist.
store_dir <- tar_path_store()
if (!dir.exists(store_dir)) dir.create(store_dir)

tar_manifest()
print(Sys.time())
tar_make(
  store = tar_config_get("store", project = tar_project),
  script = tar_config_get("script", project = tar_project),
  use_crew = TRUE
)

print(Sys.time())
print("########################################################")
print("########################################################")
