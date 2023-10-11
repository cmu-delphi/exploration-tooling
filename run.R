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
EXTERNAL_DATA <- FALSE
## TODO: Alternately, create and save an object in `_targets.R`
## that lists all objs of interest and `tar_read` that in.
forecaster_options <- tar_objects(names=contains("score"))
runApp(here::here("app.R"), port=3838)
