suppressPackageStartupMessages(source("R/load_all.R"))

# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).
set_targets_config()
linreg <- parsnip::linear_reg()
quantreg <- epipredict::quantile_reg()
randforest_grf <- rand_forest(engine = "grf_quantiles", mode = "regression")
# Debug mode will replace all forecaster functions with a fast dummy forecaster. Helps
# with prototyping the pipeline.
g_dummy_mode = as.logical(Sys.getenv("DUMMY_MODE", FALSE))
g_disease = "flu"
g_aheads = 0:4 * 7
g_hhs_signal = "confirmed_admissions_influenza_1d"
# The date when the forecast was generated (this is effectively the AS OF date).
g_forecast_generation_dates = seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L)[1:4]
# The reference date for the forecast.
g_forecast_dates = seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L)[1:4]
# This moves the week marker from Saturday to Wednesday
g_time_value_adjust = 3
# Directory for reports.
g_reports_dir = "reports"
# Fetch arguments for epidatr.
g_fetch_args = epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
# Locations we shouldn't take into account when deciding on latency
g_very_latent_locations = list(list(
  c("source"),
  c("flusurv", "ILI+")
))
# Geos with insufficient data for forecasting.
g_insufficient_data_geos = c("as", "pr", "vi", "gu", "mp")
# Human-readable object to be used for inspecting the forecasters in the pipeline.
g_forecaster_parameter_combinations <- get_covid_forecaster_params()
# Targets-readable object to be used for running the pipeline.
g_forecaster_params_grid <- g_forecaster_parameter_combinations %>%
  imap(\(x, i) make_forecaster_grid(x, i)) %>%
  bind_rows()

custom_targets <- list2(
  tar_target(
    rescaled_delphi_forecasts,
    command = {
      delphi_forecasts %>%
        mutate(forecast_date = ceiling_date(forecast_date, "weeks", week_start = 6)) %>%
        left_join(hhs_evaluation_data %>% distinct(geo_value, population), by = "geo_value") %>%
        mutate(prediction = prediction * population / 10L**5) %>%
        select(-population)
    }
  )
)

# Create targets
parameter_targets <- create_parameter_targets()
data_targets <- create_flu_data_targets()
forecast_targets <- create_forecast_targets()
external_targets <- create_flu_external_targets()
joined_targets <- create_joined_targets()

# Combine all targets
rlang::list2(
  parameter_targets,
  data_targets,
  forecast_targets,
  external_targets,
  custom_targets,
  joined_targets
)