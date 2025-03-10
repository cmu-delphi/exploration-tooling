suppressPackageStartupMessages(source("R/load_all.R"))

# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).
set_targets_config()
linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
randforest_grf <- rand_forest(engine = "grf_quantiles", mode = "regression")
# Dummy mode will replace all forecaster functions with a fast dummy forecaster. Helps
# with prototyping the pipeline.
g_dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))
g_disease <- "covid"
g_aheads <- 0:4 * 7
g_hhs_signal <- "confirmed_admissions_covid_1d"
# The date when the forecast was generated (this is effectively the AS OF date).
g_forecast_generation_dates <- seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L)[1:4]
# The reference date for the forecast.
g_forecast_dates <- seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L)[1:4]
# This moves the week marker from Saturday to Wednesday
g_time_value_adjust <- 3
# Directory for reports.
g_reports_dir <- "reports"
# Fetch arguments for epidatr.
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
# Geos that have insufficient data for forecasting.
g_insufficient_data_geos <- c("as", "pr", "vi", "gu", "mp")
# Human-readable object to be used for inspecting the forecasters in the pipeline.
g_forecaster_parameter_combinations <- get_covid_forecaster_params()
# Targets-readable object to be used for running the pipeline.
g_forecaster_params_grid <- g_forecaster_parameter_combinations %>%
  imap(\(x, i) make_forecaster_grid(x, i)) %>%
  bind_rows()

# Create targets
# parameter_targets creates
# - aheads
# - forecast_dates
# - forecaster_params_grid
parameter_targets <- create_parameter_targets()
# data_targets creates "internal" targets:
# - hhs_archive
# - nssp_archive
# - google_symptoms_archive
# - nwss_coarse
# - hhs_region
# - validate_joined_archive_data
# and the ones used in downstream targets directly:
# - joined_archive_data
# - hhs_evaluation
# - state_geo_values
data_targets <- create_covid_data_targets()
# forecast_targets creates:
# - forecast_{g_forecaster_params_grid$id} via a tar_map
# - score_{g_forecaster_params_grid$id} via a tar_map
# - delphi_forecasts
# - delphi_scores
forecast_targets <- create_forecast_targets()
# external_targets creates:
# - outside_forecaster_subset
# - external_forecasts_file
# - external_forecasts
# And the one used externally:
# - external_scores
external_targets <- create_covid_external_targets()
# joined_targets creates:
# - joined_forecasts
# - joined_scores
# - notebook_{g_forecaster_params_grid$family}
# - overall_notebook
joined_targets <- create_joined_targets()

# Combine all targets
rlang::list2(
  parameter_targets,
  data_targets,
  forecast_targets,
  external_targets,
  joined_targets
)
