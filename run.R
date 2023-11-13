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

print("Reading environment variables (TAR_PROJECT, EXTERNAL_SCORES_PATH, DEBUG_MODE, USE_SHINY)...")
tar_project <- Sys.getenv("TAR_PROJECT", "covid_hosp_explore")
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
debug_mode <- as.logical(Sys.getenv("DEBUG_MODE", TRUE))
use_shiny <- as.logical(Sys.getenv("USE_SHINY", FALSE))

cat("Using project: ", tar_project, "\n")
if (external_scores_path != "") cat("Using external scores from ", external_scores_path, "\n")
if (debug_mode) cat("Debug mode is on.")
if (use_shiny) cat("Running shiny server after results.")


suppressPackageStartupMessages({
  library(targets)
  library(shiny)
})

# targets needs the output dir to already exist.
store_dir <- tar_path_store()
if (!dir.exists(store_dir)) dir.create(store_dir)

tar_manifest()
if (debug_mode) {
  tar_make(callr_function = NULL)
} else {
  tar_make()
}
# tar_make_clustermq(workers = 2) # nolint
# tar_make_future(workers = 2) # nolint

if (use_shiny) {
  # Prevent functions defined in /R dir from being loaded unnecessarily
  options(shiny.autoload.r = FALSE)

  forecaster_options <- unique(tar_read(forecasters)[["parent_id"]])
  # Map forecaster names to score files
  forecaster_options <- setNames(
    # File names
    paste0("score_", gsub(" ", ".", forecaster_options)),
    # Display names
    forecaster_options
  )

  # Add ensembles
  ensemble_options <- tar_read(ensembles)[["a"]]
  ensemble_options <- setNames(
    # File names
    paste0("ensemble_score_", ensemble_options),
    # Display names
    paste0("ensemble score ", ensemble_options)
  )

  external_options <- tar_read(external_names)
  EXTERNAL_PREFIX <- "[external] "
  if (!is.null(external_options) && length(external_options) > 0) {
    external_options <- setNames(
      # File names
      # Get names of all branches of `external_scores` target by index. The way these
      # were specified, `external_names` provides the order of the branches.
      tar_branch_names(external_scores, seq_along(external_options)),
      # Display names
      paste0(
        EXTERNAL_PREFIX,
        gsub(" forecaster", "", gsub("_", " ", external_options, fixed = TRUE), fixed = TRUE)
      )
    )
  } else {
    external_options <- character(0)
  }

  forecaster_options <- c(ensemble_options, forecaster_options, external_options)

  runApp(here::here("app.R"), port = 3838)
}
