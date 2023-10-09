#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.
readline_wrapper <- function(msg = "which project would you like to run?
1. covid_hosp_explore
2. flu_hosp_explore
3. covid_hosp_prod
4. flu_hosp_prod
5. forecaster_testing
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

if (userin == "1") TAR_PROJECT <- "covid_hosp_explore"
if (userin == "2") TAR_PROJECT <- "flu_hosp_explore"
if (userin == "3") TAR_PROJECT <- "covid_hosp_prod"
if (userin == "4") TAR_PROJECT <- "flu_hosp_prod"
if (userin == "5") TAR_PROJECT <- "forecaster_testing"
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
forecaster_options <- tar_read(forecasters)[["id"]]
# Map forecaster names to score files
forecaster_options <- setNames(
  paste0("score_", gsub(" ", ".", forecaster_options)),
  forecaster_options
)
runApp(here::here("app.R"), port=3838)
