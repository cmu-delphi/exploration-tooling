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

readline_wrapper <- function(msg = "which project would you like to run?
1: covid_hosp_explore
2: flu_hosp_explore
3: covid_hosp_prod
4: flu_hosp_prod
input: ") {
  if (interactive()) {
    txt <- readline(msg)
  } else {
    cat(msg)
    txt <- readLines("stdin", n = 1)
  }
  return(txt)
}
project_selection <- readline_wrapper()
external_scores_path <- readline_wrapper("path to RDS file containing external forecast scores, if desired:")

debug_mode <- readline_wrapper("Would you like to run debug mode? (y/[N]): ")

suppressPackageStartupMessages({
  library(targets)
  library(shiny)
})

TAR_PROJECT <- switch(as.character(project_selection),
  "1" = "covid_hosp_explore",
  "2" = "flu_hosp_explore",
  "3" = "covid_hosp_prod",
  "4" = "flu_hosp_prod",
  # else
  stop("selection `", project_selection, "` is invalid")
)
Sys.setenv(TAR_PROJECT = TAR_PROJECT)

# targets needs the output dir to already exist.
store_dir <- tar_path_store()
if (!dir.exists(store_dir)) dir.create(store_dir)

# Load external scores file, if provided
if (external_scores_path == "") {
  LOAD_EXTERNAL_SCORES <- FALSE
} else {
  LOAD_EXTERNAL_SCORES <- TRUE
}

# Dynamically define the external files constants for access within the `targets` session.
# https://stackoverflow.com/questions/72096149/how-to-pass-values-into-targets-r-or-use-dynamic-variables
tar_helper(
  here::here(file.path(store_dir, "dynamic_constants.R")),
  {
    LOAD_EXTERNAL_SCORES <- !!LOAD_EXTERNAL_SCORES
    external_scores_path <- !!external_scores_path
  }
)

tar_manifest()
if (debug_mode == "y") {
  tar_make(callr_function = NULL)
} else {
  tar_make()
}
# tar_make_clustermq(workers = 2) # nolint
# tar_make_future(workers = 2) # nolint


use_shiny <- readline_wrapper("Would you like to run the shiny app? (y/[N]): ")
if (use_shiny == "y") {
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
