suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes) # Load other packages as needed.

  library(crew)
  library(dplyr)
  library(epipredict)
  library(epieval)
  library(lubridate)
  library(parsnip)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(rlang)
})

# The external scores processing causes the pipeline to exit with an error,
# apparently due to running out of memory. Set up a non-parallel `crew`
# controller to avoid.
# https://books.ropensci.org/targets/crew.html#heterogeneous-workers
main_controller <- crew_controller_local(
  name = "main_controller",
  workers = parallel::detectCores() - 1L
)
serial_controller <- crew_controller_local(
  name = "serial_controller",
  workers = 1L
)

tar_option_set(
  packages = c(
    "assertthat",
    "dplyr",
    "epieval",
    "epipredict",
    "ggplot2",
    "lubridate",
    "parsnip",
    "tibble",
    "tidyr",
    "epidatr"
  ), # packages that your targets need to run
  imports = c("epieval"),
  format = "qs", # Optionally set the default storage format. qs is fast.
  controller = crew_controller_group(main_controller, serial_controller),
  # Set default crew controller.
  # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
  resources = tar_resources(
    crew = tar_resources_crew(controller = "main_controller")
  )
)

use_aws_s3 <- as.logical(Sys.getenv("USE_AWS_S3", FALSE))
tar_project <- Sys.getenv("TAR_PROJECT", "covid_hosp_explore")
aws_s3_prefix <- Sys.getenv("AWS_S3_PREFIX", "exploration")
aws_s3_prefix <- paste0(aws_s3_prefix, "/", tar_project)
if (use_aws_s3) {
  tar_option_set(
    repository = "aws",
    resources = tar_resources(
      aws = tar_resources_aws(
        bucket = "forecasting-team-data",
        prefix = aws_s3_prefix,
        region = "us-east-1"
      )
    )
  )
}

linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
ONE_AHEAD_FORECAST_NAME <- "forecast_by_ahead"
ONE_AHEAD_SCORE_NAME <- "score_by_ahead"
ONE_AHEAD_ENSEMBLE_NAME <- "ensemble_by_ahead"
