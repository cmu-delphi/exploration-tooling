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

tar_option_set(
  format = "qs", # Optionally set the default storage format. qs is fast.
  controller = crew_controller_local(
    workers = num_workers,
    options_local = crew_options_local(log_directory = "local_logs")
  ),
  memory = "transient",
  error = "stop",
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