suppressPackageStartupMessages({
  source(here::here("R", "load_all.R"))
})

# On tanka, we have 64 cores, but we leave some free to try to reduce thrashing
# and to allow for other users.
if (parallel::detectCores() < 30) {
  num_workers <- parallel::detectCores() - 1L
} else {
  num_workers <- parallel::detectCores() - 20L
}

main_controller <- crew_controller_local(
  name = "main_controller",
  workers = num_workers,
  # These settings were cobbled together from various discussion threads on the
  # targets Github. There's been an ongoing issue in a dependency of {crew}
  # called {mirai}, where workers mysteriously stop working. The settings below
  # are an attempt to mitigate that.
  seconds_idle = 60L,
  seconds_timeout = 7 * 24 * 60 * 60L,
  tasks_max = 1L,
  launch_max = 10000L
)
# The external scores processing causes the pipeline to exit with an error,
# apparently due to running out of memory. Set up a non-parallel `crew`
# controller to avoid.
# https://books.ropensci.org/targets/crew.html#heterogeneous-workers
serial_controller <- crew_controller_local(
  name = "serial_controller",
  workers = 1L,
  local_log_directory = "local_logs",
  seconds_idle = 60L,
  seconds_timeout = 7 * 24 * 60 * 60L,
  tasks_max = 1L,
  launch_max = 10000L
)

# Serial mode is better for debugging.
debug_mode <- as.logical(Sys.getenv("DEBUG_MODE", "FALSE"))
if (debug_mode) {
  controllers <- crew_controller_group(serial_controller)
} else {
  controllers <- crew_controller_group(main_controller, serial_controller)
}

tar_option_set(
  format = "qs", # Optionally set the default storage format. qs is fast.
  controller = controllers,
  # Set default crew controller.
  # https://books.ropensci.org/targets/crew.html#heterogeneous-workers
  resources = tar_resources(
    crew = tar_resources_crew(
      controller = "main_controller"
    )
  ),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
)

linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
ONE_AHEAD_FORECAST_NAME <- "forecast_by_ahead"
ONE_AHEAD_SCORE_NAME <- "score_by_ahead"
ONE_AHEAD_ENSEMBLE_NAME <- "ensemble_by_ahead"
