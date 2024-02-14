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
  workers = parallel::detectCores() - 1L,
  launch_max = 20
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
    crew = tar_resources_crew(
      controller = "main_controller",
      seconds_timeout = 7 * 24 * 60 * 60
    )
  )
)

linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
ONE_AHEAD_FORECAST_NAME <- "forecast_by_ahead"
ONE_AHEAD_SCORE_NAME <- "score_by_ahead"
ONE_AHEAD_ENSEMBLE_NAME <- "ensemble_by_ahead"
