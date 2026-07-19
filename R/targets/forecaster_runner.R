# Shared runner for the exploration and production forecasting pipelines.
#
# In the exploration convention a forecaster is a bare function
# `fn(epi_data, ahead, ...)` configured entirely by `params`. Both pipelines
# need the same per-forecaster wrapping that isn't itself a forecaster
# parameter: ahead-unit conversion (weeks -> days), a target-date shift, an
# extra-source join, source/geo filtering, and the flu quantile-whitening
# workaround. This runner applies that wrapping around the partially-applied
# forecaster over a single as-of snapshot, driven by explicit columns so the
# grid rows fully define forecaster behavior. Explore maps it over dates; prod
# calls it once per (forecaster, date) target.
#
# @param snapshot          an as-of epi_df from make_forecast_snapshot().
# @param aheads            aheads already in the forecaster's native unit (the
#                          per-forecaster ahead_multiplier is applied by the
#                          caller at grid/call-site time, not here).
# @param target_date_shift days added to target_end_date after forecasting.
# @param join_extra_data   left-join extra_data before forecasting and drop the
#                          resulting source column afterwards.
# @param extra_data        exogenous data to join when join_extra_data is TRUE.
# @param filter_sources    if non-NULL, keep only these sources in the input.
# @param excluded_geos     geos dropped from the output (NULL keeps all).
# @param primary_source    if non-NULL, injected as the forecaster's
#                          `primary_source` param (the source acting as the
#                          forecast target) -- but only for forecasters that
#                          accept it. Lets the nssp-as-target path point
#                          scaled_pop_seasonal at its honestly-stamped `nssp`
#                          rows without making it a grid param (the same grid row
#                          drives both the nhsn and nssp forecasts).
# @param sort_quantiles    if TRUE, enforce quantile monotonicity on the output
#                          (the flu whitening workaround; a no-op elsewhere).
# @return a forecast tibble on hub convention: the forecaster's core columns
#   with `value`, a `forecaster` id, and target_end_date shifted per
#   target_date_shift. forecast_date stays forecaster-native (the snapshot's
#   as_of); each pipeline aligns it downstream as it needs.
run_forecaster <- function(
  snapshot, forecaster, aheads, params, id,
  target_date_shift = 0L,
  join_extra_data = FALSE, extra_data = NULL,
  filter_sources = NULL, excluded_geos = NULL,
  primary_source = NULL,
  sort_quantiles = FALSE
) {
  if (!is.null(filter_sources) && "source" %in% colnames(snapshot)) {
    snapshot <- snapshot %>% filter(source %in% filter_sources)
  }
  # Inject primary_source only for forecasters that accept it; others route it
  # through ... into default_args_list(), which errors on unknown args.
  if (!is.null(primary_source) && "primary_source" %in% names(formals(forecaster))) {
    params <- c(params, list(primary_source = primary_source))
  }
  if (join_extra_data) {
    snapshot <- snapshot %>% left_join(extra_data, by = join_by(geo_value, time_value))
  }
  forecaster_fn <- get_partially_applied_forecaster(forecaster, aheads, params)
  out <- forecaster_fn(snapshot)
  if (join_extra_data && "source" %in% colnames(out)) {
    out <- out %>% select(-source)
  }
  if (sort_quantiles) {
    # TODO: Hack fix because whitening has edge cases. Remove when fixed.
    out <- out %>% sort_by_quantile()
  }
  out %>%
    mutate(target_end_date = target_end_date + target_date_shift) %>%
    filter(geo_value %nin% excluded_geos) %>%
    mutate(forecaster = id) %>%
    validate_forecast_output(id)
}

# Output-shape contract for everything run_forecaster ships: the storage-format
# key columns present and NA-free, non-negative values, and quantiles monotone
# within each (geo, forecast_date, target) task. Monotonicity is asserted here
# (not silently re-sorted) so a forecaster with crossing quantiles surfaces as
# an error unless its grid row opts into the sort_quantiles workaround.
validate_forecast_output <- function(forecast, id = "<unknown>") {
  required <- c("geo_value", "forecast_date", "target_end_date", "quantile", "value")
  missing_cols <- setdiff(required, colnames(forecast))
  if (length(missing_cols) > 0) {
    cli::cli_abort("forecaster {id}: output is missing column{?s} {.field {missing_cols}}.")
  }
  na_cols <- required[vapply(forecast[required], anyNA, logical(1))]
  if (length(na_cols) > 0) {
    cli::cli_abort("forecaster {id}: output has NA values in {.field {na_cols}}.")
  }
  if (any(forecast$value < 0)) {
    cli::cli_abort("forecaster {id}: output has {sum(forecast$value < 0)} negative value{?s}.")
  }
  crossed <- forecast %>%
    arrange(quantile) %>%
    group_by(geo_value, forecast_date, target_end_date) %>%
    summarize(crossed = is.unsorted(value), .groups = "drop") %>%
    filter(crossed)
  if (nrow(crossed) > 0) {
    example <- crossed[1, ]
    cli::cli_abort(
      "forecaster {id}: quantiles cross in {nrow(crossed)} task{?s},
       e.g. ({example$geo_value}, {example$forecast_date}, {example$target_end_date}).
       Fix the forecaster or set sort_quantiles = TRUE on its grid row."
    )
  }
  forecast
}
