#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# renv::init()
renv::restore()

suppressMessages({
  library(targets)
  library(shiny)
})


tar_manifest()
tar_make()
# tar_make_clustermq(workers = 2) # nolint
# tar_make_future(workers = 2) # nolint


# Prevent functions defined in /R dir from being loaded unnecessarily
options(shiny.autoload.r=FALSE)
## TODO: Alternately, create and save an object in `_targets.R`
## that lists all objs of interest and `tar_read` that in.
forecaster_options <- tar_objects(names=contains("score"))
runApp(here::here("app.R"), port=3838)
