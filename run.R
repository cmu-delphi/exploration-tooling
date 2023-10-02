#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

renv::init()

suppressMessages({
  library(targets)
  library(shiny)
})


tar_manifest()
tar_make()
# tar_make_clustermq(workers = 2) # nolint
# tar_make_future(workers = 2) # nolint


# Prevent functions defined in /R dir from being loaded
options(shiny.autoload.r=FALSE)

forecaster_options <- tar_objects(names=contains("score"))
runApp("app.R")
