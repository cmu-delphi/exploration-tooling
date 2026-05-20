# Forecaster functions for the RSV hospitalization production pipeline.
# These are referenced by name via rlang::syms() in g_forecaster_params_grid.

g_rsv_linear <- function(epi_data, ahead, extra_data, ...) {
  forecaster_baseline_linear(epi_data, ahead, ..., residual_tail = 0.97, residual_center = 0.097, no_intercept = TRUE)
}

g_rsv_linear_no_population_scale <- function(epi_data, ahead, extra_data, ...) {
  forecaster_baseline_linear(epi_data, ahead, ..., residual_tail = 0.97, residual_center = 0.097, no_intercept = TRUE, population_scale = FALSE)
}

g_rsv_climate_base <- function(epi_data, ahead, extra_data, ...) {
  climatological_model(epi_data, ahead, ...)
}

g_rsv_climate_geo_agged <- function(epi_data, ahead, extra_data, ...) {
  climatological_model(epi_data, ahead, ..., geo_agg = TRUE)
}

g_rsv_windowed_seasonal <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    scaled_pop_seasonal(
      outcome = "value",
      ahead = ahead * 7,
      ...,
      seasonal_method = "none",
      trainer = epipredict::quantile_reg(),
      drop_non_seasons = TRUE,
      pop_scaling = FALSE,
      lags = list(c(0, 7))
    ) %>%
    mutate(target_end_date = target_end_date + 3)
}

g_rsv_windowed_seasonal_extra_sources <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    left_join(extra_data, by = join_by(geo_value, time_value)) %>%
    scaled_pop_seasonal(
      outcome = "value",
      ahead = ahead * 7,
      extra_sources = "nssp",
      ...,
      seasonal_method = "window",
      trainer = epipredict::quantile_reg(),
      drop_non_seasons = TRUE,
      pop_scaling = FALSE,
      lags = list(c(0, 7), c(0, 7))
    ) %>%
    mutate(target_end_date = target_end_date + 3) %>%
    filter(geo_value %nin% c("mo", "wy"))
}
