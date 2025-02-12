suppressPackageStartupMessages({
  source(here::here("R", "load_all.R"))
})

# On tanka, we have 64 cores, but we leave some free to try to reduce thrashing
# and to allow for other users.
if (parallel::detectCores() == 64) {
  num_workers <- 30L
} else {
  num_workers <- parallel::detectCores() - 1L
}

main_controller <- crew_controller_local(
  name = "main_controller",
  workers = num_workers,
  seconds_idle = 60L,
  seconds_timeout = 24 * 60 * 60L,
  garbage_collection = TRUE,
  options_local = crew_options_local(log_directory = "local_logs")
)
# The external scores processing causes the pipeline to exit with an error,
# apparently due to running out of memory. Set up a non-parallel `crew`
# controller to avoid.
# https://books.ropensci.org/targets/crew.html#heterogeneous-workers
serial_controller <- crew_controller_local(
  name = "serial_controller",
  workers = 1L,
  options_local = crew_options_local(log_directory = "local_logs"),
  seconds_idle = 60L,
  seconds_timeout = 24 * 60 * 60L,
  garbage_collection = TRUE
)

# Serial mode is better for debugging.
debug_mode <- as.logical(Sys.getenv("DEBUG_MODE", "FALSE"))
if (debug_mode) {
  controllers <- crew_controller_group(serial_controller)
  on_error <- "stop"
} else {
  controllers <- crew_controller_group(main_controller, serial_controller)
  on_error <- "trim"
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
  error = on_error,
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker" # this may need to go back to main
)

linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
randforest_grf <- rand_forest(engine = "grf_quantiles", mode = "regression")

# Suppress readr::read_csv progress and column type messages
options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)