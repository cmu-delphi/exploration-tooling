#!/usr/bin/env Rscript

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

tar_project <- Sys.getenv("TAR_PROJECT", "covid_hosp_explore")
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
debug_mode <- as.logical(Sys.getenv("DEBUG_MODE", TRUE))
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

#' This function is useful if you're running into issues with the pipeline
#' quitting after about 5 hours due to parallelism issues. It will restart the
#' pipeline and continue where it left off.
restart_loop <- function() {
  result <- 1
  while (result) {
    result <- tryCatch(
      {
        tar_make()
      },
      error = function(e) {
        print("Error! Restarting...")
        print(e)
        return(1)
      },
      finally = function(e) {
        print("Finished!")
        return(0)
      }
    )
  }
}

tar_manifest()
if (debug_mode) {
  tar_make(callr_function = NULL, use_crew = FALSE)
} else {
  tar_make()
  # restart_loop()
}

if (use_shiny) {
  source("scripts/dashboard.R")
}
