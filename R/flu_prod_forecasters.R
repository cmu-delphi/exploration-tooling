# Forecaster functions for the flu hospitalization production pipeline.
# These are referenced by name via rlang::syms() in g_forecaster_params_grid.
# g_very_latent_locations is defined in flu_hosp_prod.R.

g_flu_linear <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter((source == "nhsn") | (source == "nssp")) %>%
    forecaster_baseline_linear(ahead, ..., residual_tail = 0.99, residual_center = 0.35, no_intercept = TRUE)
}

g_flu_linear_no_population_scale <- function(epi_data, ahead, extra_data, ...) {
  forecaster_baseline_linear(
    epi_data, ahead, ...,
    residual_tail = 0.99, residual_center = 0.35,
    no_intercept = TRUE, population_scale = FALSE
  )
}

g_flu_climate_base <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter((source == "nhsn") | (source == "nssp")) %>%
    climatological_model(ahead, ...)
}

g_flu_climate_geo_agged <- function(epi_data, ahead, extra_data, ...) {
  epi_data %>%
    filter((source == "nhsn") | (source == "nssp")) %>%
    climatological_model(ahead, ..., geo_agg = TRUE)
}

g_flu_windowed_seasonal <- function(epi_data, ahead, extra_data, ...) {
  scaled_pop_seasonal(
    epi_data,
    outcome = "value",
    ahead = ahead * 7,
    ...,
    trainer = epipredict::quantile_reg(),
    seasonal_method = "window",
    pop_scaling = FALSE,
    lags = c(0, 7),
    keys_to_ignore = g_very_latent_locations
  ) %>%
    mutate(target_end_date = target_end_date + 3)
}

g_flu_windowed_seasonal_extra_sources <- function(epi_data, ahead, extra_data, ...) {
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
      lags = list(c(0, 7), c(0, 7)),
      keys_to_ignore = g_very_latent_locations
    ) %>%
    select(-source) %>%
    mutate(target_end_date = target_end_date + 3) %>%
    filter(geo_value %nin% c("mo", "wy"))
}
