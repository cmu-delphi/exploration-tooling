# Shared runner for the production forecasting pipelines.
#
# In the exploration convention a forecaster is a bare function
# `fn(epi_data, ahead, ...)` configured entirely by `params`. The flu prod
# forecasters additionally need per-forecaster wrapping that isn't a forecaster
# parameter: ahead-unit conversion (weeks -> days), a target-date shift, an
# extra-source join, and source/geo filtering. This runner applies that wrapping
# around the partially-applied forecaster, driven by explicit grid columns so the
# grid rows fully define forecaster behavior.
#
# @param ahead_units       "weeks" (ahead as-is) or "days" (ahead * 7).
# @param target_date_shift days added to target_end_date after forecasting.
# @param join_extra_data    left-join extra_data before forecasting and drop the
#                           resulting source column afterwards.
# @param filter_sources     if non-NULL, keep only these sources in the input.
# @param excluded_geos      geos dropped from the output (NULL keeps all).
run_prod_forecaster <- function(
  epi_data, extra_data, forecaster, aheads, params, param_names, id,
  ahead_units, target_date_shift, join_extra_data, filter_sources, excluded_geos
) {
  ahead_multiplier <- if (ahead_units == "days") 7 else 1

  if (!is.null(filter_sources) && "source" %in% colnames(epi_data)) {
    epi_data <- epi_data %>% filter(source %in% filter_sources)
  }
  if (join_extra_data) {
    epi_data <- epi_data %>% left_join(extra_data, by = join_by(geo_value, time_value))
  }
  forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads * ahead_multiplier, params, param_names)
  out <- forecaster_fn(epi_data)
  if (join_extra_data && "source" %in% colnames(out)) {
    out <- out %>% select(-source)
  }
  out %>%
    mutate(target_end_date = target_end_date + target_date_shift) %>%
    filter(geo_value %nin% excluded_geos) %>%
    mutate(forecaster = id, geo_value = as.factor(geo_value))
}
