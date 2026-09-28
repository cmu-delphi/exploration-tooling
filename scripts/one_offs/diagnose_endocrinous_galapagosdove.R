#!/usr/bin/env Rscript
# One-off diagnostic for the revision_aware_nssp / outlier_n_weeks=4 branch
# (codename "endocrinous.galapagosdove" as of 2026-09-26) that was still slow
# and near-empty on the remote after the pop_scaling and outlier-join fixes.
#
# Run this directly in the flu_hosp_explore project on the remote, where
# joined_archive_data is live/current -- a locally synced copy is stale (last
# push to S3 was 2026-09-19) and does NOT reproduce the problem.
#
# Usage:
#   TAR_PROJECT=flu_hosp_explore Rscript scripts/one_offs/diagnose_endocrinous_galapagosdove.R

suppressPackageStartupMessages(source("R/load_all.R"))
library(targets)

Sys.setenv(TAR_PROJECT = "flu_hosp_explore")

g_dummy_mode <- FALSE
g_aheads <- 0:4 * 7
g_very_latent_locations <- list(list(c("source"), c("flusurv", "ILI+")))
g_forecaster_parameter_combinations <- get_flu_forecaster_params()
g_forecaster_params_grid <- g_forecaster_parameter_combinations %>%
  purrr::imap(\(x, i) make_forecaster_grid(x, i)) %>%
  dplyr::bind_rows()

target_id <- "endocrinous.galapagosdove"
row <- g_forecaster_params_grid[g_forecaster_params_grid$id == target_id, ]
if (nrow(row) != 1) {
  cli::cli_abort("Expected exactly 1 grid row for id {target_id}, found {nrow(row)}. Did the config change?")
}
cli::cli_inform("Params for {target_id}:")
print(row %>% dplyr::select(id, forecaster, ahead_multiplier, sort_quantiles, needs_archive))
str(row$params[[1]])

quantreg_fn <- epipredict::quantile_reg(method = "fn")
joined_archive_data <- tar_read(joined_archive_data)
cli::cli_inform("joined_archive_data versions_end: {format(joined_archive_data$versions_end)}")

# NSSP coverage check: this branch needs 4 nssp lags (0/7/14/21 days) at nhsn's
# geos, and the forecast anchor is nhsn's latest vintage. `nssp` is a column
# (LOCF-merged onto nhsn's rows via epix_merge), not a `source` value -- check
# its NA rate on nhsn rows, not `source == "nssp"` (matches nothing). If nssp
# has gone stale/sparse relative to nhsn recently, forecast_rows can end up
# empty for most or all dates -- exactly the "no non-missing arguments to max"
# warning and near-total nulls seen on the remote.
nssp_recent <- joined_archive_data$DT %>%
  dplyr::as_tibble() %>%
  dplyr::filter(source == "nhsn", time_value >= max(time_value) - 90) %>%
  dplyr::summarize(
    n_rows = dplyr::n(),
    n_na_nssp = sum(is.na(nssp)),
    n_geos = dplyr::n_distinct(geo_value),
    max_time_value = max(time_value),
    .by = NULL
  )
cli::cli_inform("nssp NA-rate on nhsn rows in the last 90 days of time_value:")
print(nssp_recent)

nhsn_recent_max <- joined_archive_data$DT %>%
  dplyr::as_tibble() %>%
  dplyr::filter(source == "nhsn") %>%
  dplyr::summarize(max_time_value = max(time_value))
cli::cli_inform("nhsn max time_value: {format(nhsn_recent_max$max_time_value)}")

forecast_dates <- seq.Date(as.Date("2024-11-20"), as.Date("2026-04-29"), by = 7L)
ahead_branch <- 7
t0 <- Sys.time()
warn_count <- 0
out <- withCallingHandlers(
  purrr::map(forecast_dates, function(fdate) {
    input <- make_forecast_archive_snapshot(joined_archive_data, forecast_date = fdate, generation_date = fdate)
    run_forecaster(
      snapshot = input,
      forecaster = scaled_pop_seasonal_revision,
      aheads = ahead_branch * row$ahead_multiplier,
      params = row$params[[1]],
      id = row$id,
      sort_quantiles = row$sort_quantiles
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(ahead = as.numeric(target_end_date - forecast_date)),
  warning = function(w) {
    warn_count <<- warn_count + 1
    cli::cli_inform("WARNING: {conditionMessage(w)}")
    invokeRestart("muffleWarning")
  },
  message = function(m) invokeRestart("muffleMessage")
)
t1 <- Sys.time()
cli::cli_inform("elapsed: {format(t1 - t0)}")
cli::cli_inform("nrow: {nrow(out)}, warnings: {warn_count}")
