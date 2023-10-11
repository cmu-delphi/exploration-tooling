#!/usr/bin/env Rscript

# This is a helper script to launch the shiny app with external scores in
# RDS format.
## Run backfill corrections pipeline.
##
## Usage:
##
## Rscript run.R [options]

## Example fetching scores from the forecasting bucket
# library(aws.s3)
# 
# Sys.setenv(
#   AWS_ACCESS_KEY_ID = "",
#   AWS_SECRET_ACCESS_KEY = ""
# )
# 
# s3b <- get_bucket("forecasting-team-data")
# 
# # Load object
# scorecards <- s3readRDS(
#   object = "2023/exploration-scorecards-2023-10-04.RDS",
#   bucket = s3b
# )
# saveRDS(scorecards, "exploration-scorecards-2023-10-04.RDS")


suppressPackageStartupMessages({
  library(argparser)
  library(shiny)
  library(dplyr)
})

parser <- arg_parser(description="Run shiny app with a specified RDS score file")
parser <- add_argument(
  parser, arg="score_path",
  help="path to RDS score file"
)
parser <- add_argument(
  parser, arg="--cache_path", default = "cache",
  help="path to dir in which to store by-forecaster scores"
)
args = parse_args(parser)

# renv::init()
renv::restore()

# Prevent functions defined in /R dir from being loaded unnecessarily
options(shiny.autoload.r=FALSE)

EXTERNAL_DATA <- TRUE
scores <- readRDS(args$score_path)
forecaster_options <- unique(scores$forecaster)

# Create local dir in which to store by-forecaster scores
OUTPUT_DIR <- args$cache_path
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# Save score for each forecaster separately to local cache dir.
invisible(lapply(group_split(scores, forecaster), function(one_forecaster) {
  forecaster <- one_forecaster$forecaster[1L]
  saveRDS(one_forecaster, file.path(OUTPUT_DIR, paste0(forecaster, ".RDS")))
}))

runApp(here::here("app.R"), port=3838)
