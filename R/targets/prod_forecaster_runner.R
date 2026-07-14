# Shared runner for the production forecasting pipelines.
#
# In the exploration convention a forecaster is a bare function
# `fn(epi_data, ahead, ...)` configured entirely by `params`. The flu prod
# forecasters additionally need per-forecaster wrapping that isn't (yet) a
# forecaster parameter: ahead-unit conversion (weeks -> days), a target-date
# shift, an extra-source join, and source/geo filtering. This runner applies
# that wrapping around the partially-applied forecaster so the grid rows can be
# plain (id, forecaster, params).
#
# TEMP: the wrapping behavior is derived from `id` here. Steps 1b/1c replace the
# id-dispatch with explicit grid columns.
run_prod_forecaster <- function(epi_data, extra_data, forecaster, aheads, params, param_names, id) {
  seasonal <- id %in% c("windowed_seasonal", "windowed_seasonal_extra_sources", "seasonal_nssp_latest")
  join_extra <- id %in% c("windowed_seasonal_extra_sources", "seasonal_nssp_latest")
  ahead_multiplier <- if (seasonal) 7 else 1
  target_date_shift <- if (seasonal) 3 else 0
  filter_sources <- if (id %in% c("linear", "climate_base", "climate_geo_agged")) c("nhsn", "nssp") else NULL
  excluded_geos <- if (join_extra) c("mo", "wy") else NULL

  if (!is.null(filter_sources) && "source" %in% colnames(epi_data)) {
    epi_data <- epi_data %>% filter(source %in% filter_sources)
  }
  if (join_extra) {
    epi_data <- epi_data %>% left_join(extra_data, by = join_by(geo_value, time_value))
  }
  forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads * ahead_multiplier, params, param_names)
  out <- forecaster_fn(epi_data)
  if (join_extra && "source" %in% colnames(out)) {
    out <- out %>% select(-source)
  }
  out %>%
    mutate(target_end_date = target_end_date + target_date_shift) %>%
    filter(geo_value %nin% excluded_geos) %>%
    mutate(forecaster = id, geo_value = as.factor(geo_value))
}
