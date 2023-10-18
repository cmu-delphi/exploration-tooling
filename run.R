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
userin <- readline_wrapper()

# renv::init()
renv::restore()

suppressMessages({
  library(targets)
  library(shiny)
})

TAR_PROJECT <- switch(
  as.character(userin),
  "1" = "covid_hosp_explore",
  "2" = "flu_hosp_explore",
  "3" = "covid_hosp_prod",
  "4" = "flu_hosp_prod",
  "5" = "forecaster_testing",
  # else
  stop("selection `", userin, "` is invalid")
)
Sys.setenv(TAR_PROJECT = TAR_PROJECT)

# targets needs the output dir to already exist.
store_dir <- tar_path_store()
if (!dir.exists(store_dir)) dir.create(store_dir)

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
runApp(here::here("app.R"), port=3838)
