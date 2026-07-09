# Ensemble, weighting, and truth-data computations for the flu production and
# backfill pipelines. Extracted verbatim from the per-date ensemble targets in
# scripts/_flu_prod_shared.R (REFACTOR.md Exp 1) so the logic is testable and no
# longer duplicated as frozen target-command blocks. Each function is pure in its
# arguments; the g_* it reads (g_forecaster_params_grid, g_truth_data_date,
# g_insufficient_data_geos) are pipeline config globals, same as elsewhere.

# Per-forecaster geo weights for nhsn and nssp, parsed from the exclusion files.
flu_geo_weights <- function(flu_geo_exclusions, flu_nssp_geo_exclusions, forecast_date_int) {
  make_weights <- function(excl_file) {
    w <- parse_prod_weights(excl_file, forecast_date_int, g_forecaster_params_grid$id)
    if (nrow(w %>% filter(forecast_date == as.Date(forecast_date_int))) == 0) {
      cli_abort("there are no weights for the forecast date {forecast_date}")
    }
    w
  }
  list(
    nhsn = make_weights(flu_geo_exclusions),
    nssp = make_weights(flu_nssp_geo_exclusions)
  )
}

# Climate/linear ensemble. nhsn uses default climate weights; nssp caps them.
flu_ensemble_clim_lin <- function(forecast_filtered, aheads, geo_weights, geo_exclusions) {
  # flu nhsn: no max_climate_* params; flu nssp: has them
  nhsn_clim_lin <- forecast_filtered$nhsn %>%
    ensemble_climate_linear(aheads, other_weights = geo_weights$nhsn) %>%
    filter(geo_value %nin% geo_exclusions) %>%
    ungroup() %>%
    sort_by_quantile() %>%
    mutate(forecaster = "climate_linear")
  nssp_clim_lin <- forecast_filtered$nssp %>%
    ensemble_climate_linear(
      aheads,
      other_weights = geo_weights$nssp,
      max_climate_ahead_weight = 0.6,
      max_climate_quantile_weight = 0.6
    ) %>%
    filter(geo_value %nin% geo_exclusions) %>%
    ungroup() %>%
    sort_by_quantile() %>%
    mutate(forecaster = "climate_linear")
  list(nhsn = nhsn_clim_lin, nssp = nssp_clim_lin)
}

# Unweighted mean of the AR (windowed seasonal) forecasters, nhsn only.
flu_ens_ar_only <- function(forecast_filtered) {
  forecast_filtered$nhsn %>%
    filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
    group_by(geo_value, forecast_date, target_end_date, quantile) %>%
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    sort_by_quantile() %>%
    mutate(forecaster = "ens_ar_only")
}

# Weighted mixture of the climate/linear ensemble and the AR forecasters.
flu_ensemble_mixture <- function(forecast_filtered, ensemble_clim_lin, geo_weights) {
  ar_nhsn <- forecast_filtered$nhsn %>%
    filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))
  ar_nssp <- forecast_filtered$nssp %>%
    filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
    filter(forecast_date < target_end_date) # flu nssp: drop neg aheads from AR
  list(
    nhsn = ensemble_clim_lin$nhsn %>%
      bind_rows(ar_nhsn) %>%
      ensemble_weighted(geo_weights$nhsn) %>%
      mutate(forecaster = "ensemble_mix"),
    nssp = ensemble_clim_lin$nssp %>%
      bind_rows(ar_nssp) %>%
      ensemble_weighted(geo_weights$nssp) %>%
      mutate(forecaster = "ensemble_mix")
  )
}

# Truth data for scoring/plotting: as-of and latest nhsn, plus nssp rescaled to
# each primary signal's magnitude.
flu_truth_data <- function(nhsn_archive_data, nhsn_latest_data, nssp_latest_data, forecast_generation_date_int) {
  nhsn_raw <- nhsn_archive_data %>%
    epix_as_of(min(as.Date(forecast_generation_date_int), nhsn_archive_data$versions_end)) %>%
    mutate(source = "nhsn as_of forecast") %>%
    bind_rows(nhsn_latest_data %>% mutate(source = "nhsn")) %>%
    select(geo_value, target_end_date = time_value, value, source) %>%
    filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos)
  nssp_raw <- nssp_latest_data %>%
    select(geo_value, target_end_date = time_value, value = nssp) %>%
    filter(target_end_date > g_truth_data_date, geo_value %nin% g_insufficient_data_geos) %>%
    mutate(target_end_date = target_end_date + 3, source = "nssp")
  normalize_to_primary <- function(primary, secondary) {
    rel_max <- secondary %>%
      rename(sec = value) %>%
      full_join(primary %>% select(geo_value, target_end_date, value), by = join_by(geo_value, target_end_date)) %>%
      group_by(geo_value) %>%
      summarise(scale = max(value, na.rm = TRUE) / max(sec, na.rm = TRUE))
    secondary %>%
      left_join(rel_max, by = join_by(geo_value)) %>%
      mutate(value = value * scale) %>%
      select(-scale) %>%
      bind_rows(primary, .)
  }
  list(
    nhsn = normalize_to_primary(nhsn_raw, nssp_raw),
    nssp = normalize_to_primary(nssp_raw, nhsn_raw)
  )
}
