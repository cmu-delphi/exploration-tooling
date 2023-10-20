#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.
readline_wrapper <- function(msg = "which project would you like to run?
1: covid_hosp_explore
2: flu_hosp_explore
3: covid_hosp_prod
4: flu_hosp_prod
5: forecaster_testing
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

# renv::init()
renv::restore()

suppressPackageStartupMessages({
  library(targets)
  library(shiny)
})

TAR_PROJECT <- switch(
  as.character(project_selection),
  "1" = "covid_hosp_explore",
  "2" = "flu_hosp_explore",
  "3" = "covid_hosp_prod",
  "4" = "flu_hosp_prod",
  "5" = "forecaster_testing",
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

# scores <- readRDS(external_scores_path)
# external_forecaster_options <- unique(scores$forecaster)
#
# # Create local dir in which to store by-forecaster scores
# OUTPUT_DIR <- "cache"
# if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)
#
# # Save score for each forecaster separately to local cache dir.
# invisible(lapply(group_split(scores, forecaster), function(one_forecaster) {
#   forecaster <- one_forecaster$forecaster[1L]
#   saveRDS(one_forecaster, file.path(OUTPUT_DIR, paste0(forecaster, ".RDS")))
# }))
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
tar_make()
# tar_make_clustermq(workers = 2) # nolint
# tar_make_future(workers = 2) # nolint


# Prevent functions defined in /R dir from being loaded unnecessarily
options(shiny.autoload.r=FALSE)

forecaster_options <- unique(tar_read(forecasters)[["parent_id"]])
# Map forecaster names to score files
forecaster_options <- setNames(
  paste0("score_", gsub(" ", ".", forecaster_options)),
  forecaster_options
)

# Add ensembles
ensemble_options <- tar_read(ensembles)[["a"]]
ensemble_options <- setNames(
  paste0("ensemble_score_", ensemble_options),
  paste0("ensemble score ", ensemble_options)
)

forecaster_options <- c(ensemble_options, forecaster_options)

# runApp(here::here("app.R"), port=3838)
