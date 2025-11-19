suppressPackageStartupMessages(source("R/load_all.R"))

# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).

# Setup targets config.
set_targets_config()
# These are trainer model objects needed by the targets pipeline.
linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
randforest_grf <- rand_forest(engine = "grf_quantiles", mode = "regression")
# Dummy mode will replace all forecaster functions with a fast dummy forecaster. Helps
# with prototyping the pipeline.
g_dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))
g_disease <- "covid"
g_dataset <- "nssp"
g_percent_to_fraction <- TRUE
g_aheads <- 0:4 * 7
# this is just for downloading nhsn data
g_hhs_signal <- "confirmed_admissions_covid_1d"
g_s3_prefix <- "exploration"
g_start_date <- round_date(as.Date("2024-11-20"), "week", week_start = 7) + 3
g_end_date <- round_date(as.Date("2025-06-04"), "week", week_start = 7) + 3
# The date when the forecast was generated (this is effectively the AS OF date).
g_forecast_generation_dates <- seq.Date(g_start_date, g_end_date, by = 7L)
# The reference date for the forecast.
g_forecast_dates <- seq.Date(g_start_date, g_end_date, by = 7L)
# This moves the week marker from Saturday to Wednesday
g_time_value_adjust <- 3
# Directory for reports.
g_reports_dir <- "reports"
# Fetch arguments for epidatr.
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
# Geos with insufficient data for forecasting.
g_insufficient_data_geos <- c("as", "pr", "vi", "gu", "mp", "mo")
# Parameters object used for grouping forecasters by family.
g_forecaster_parameter_combinations <- get_covid_nssp_forecaster_params()
# Targets-readable object used for running the pipeline.
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
# - nhsn_archive
# - nssp_archive
# - google_symptoms_archive
# - ~nwss_coarse~ currently broken
# - hhs_region
# - validate_joined_archive_data
# and the ones used in downstream targets directly:
# - joined_archive_data
# - hhs_evaluation
# - state_geo_values
data_targets <- create_covid_data_targets()
# set joined_archive to be the correct one
data_target_modification <- list2(
  tar_target(
    joined_archive_data,
    command = {
      archive <- joined_archive_data_nhsn$DT %>%
        mutate(
          nhsn = value,
          value = nssp
        ) %>%
        as_epi_archive(compactify = TRUE)
      archive$geo_type <- "state"
      epix_slide_simple(archive, dummy_forecaster, forecast_dates, cache_key = "joined_archive_data")
      archive
    }
  ),
  tar_target(
    name = evaluation_data,
    command = {
      truth_data <- nssp_archive %>%
        epix_as_of_current() %>%
        filter(geo_value %nin% g_insufficient_data_geos)
      truth_data %>%
        rename(
          target_end_date = time_value,
          true_value = nssp
        )
    }
  ),
)
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
external_targets <- list2(
  create_covid_external_targets(g_start_date, g_end_date),
  tar_target(
    outside_forecaster_subset,
    command = c("CovidHub-baseline", "CovidHub-ensemble")
  ),
)
# joined_targets creates:
# - joined_forecasts
# - joined_scores
# - notebook_{g_forecaster_params_grid$family}
# - overall_notebook
joined_targets <- create_joined_targets()

# Combine all targets
rlang::list2(
  parameter_targets,
  data_target_modification,
  data_targets,
  forecast_targets,
  external_targets,
  joined_targets
)
