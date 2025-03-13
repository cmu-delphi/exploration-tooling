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

# This is TAR_RUN_PROJECT and not TAR_PROJECT, because the latter gets
# overwritten by the environment variable of the same name in the shell that
# runs this script.
tar_project <- Sys.getenv("TAR_RUN_PROJECT", "flu_hosp_prod")
# Where to place files in S3 (mostly unused)
aws_s3_prefix <- Sys.getenv("AWS_S3_PREFIX", "exploration") %>% paste0("/", tar_project)
# Where to place flu forecasts
flu_submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
# Where to place covid forecasts
covid_submission_directory <- Sys.getenv("COVID_SUBMISSION_DIRECTORY", "cache")
cli::cli_inform(
  c(
    "i" = "Reading environment variables...",
    "*" = "TAR_PROJECT = {tar_project}",
    "*" = "AWS_S3_PREFIX = {aws_s3_prefix}",
    "*" = "FLU_SUBMISSION_DIRECTORY = {flu_submission_directory}",
    "*" = "COVID_SUBMISSION_DIRECTORY = {covid_submission_directory}"
  )
)


# Targets needs the output dir to already exist.
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

tar_make(
  store = tar_config_get("store", project = tar_project),
  script = tar_config_get("script", project = tar_project)
)
