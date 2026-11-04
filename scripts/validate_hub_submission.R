# Run the hub validations on one round's submission files in a hub checkout.
# These are the files scripts/commit-script.sh commits for the same round.
# Exits nonzero if a check fails, so `just submit` stops before it pushes.
#
# Usage: Rscript scripts/validate_hub_submission.R <hub-directory> <reference-date>
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

args <- commandArgs(trailingOnly = TRUE)
hub_path <- args[[1]]
reference_date <- as.Date(args[[2]])

# CMU-TimeSeries must be present; CMU-TimeSeries_Cal is not made for every hub.
required <- sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", reference_date)
optional <- sprintf("CMU-TimeSeries_Cal/%s-CMU-TimeSeries_Cal.csv", reference_date)
if (!file.exists(file.path(hub_path, "model-output", required))) {
  cli::cli_abort("No submission file {.path {required}} in {.path {hub_path}}.")
}
file_paths <- c(required, optional[file.exists(file.path(hub_path, "model-output", optional))])

for (file_path in file_paths) {
  validate_submission_or_abort(hub_path, file_path)
  cli::cli_alert_success("{.path {file_path}} passed hub validation.")
}
