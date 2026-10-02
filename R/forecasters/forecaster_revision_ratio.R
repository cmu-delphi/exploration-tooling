#' Revision-ratio nowcast for weeks that are already reported.
#'
#' A simple baseline for negative aheads (h−1 and earlier), where the target
#' week already has a value that will still be revised. The prediction is that
#' reported value times the revision ratios seen recently: for each of the last
#' `window_weeks` settled weeks, the finalized value divided by the value that
#' week had at the same reporting lag the target week has now. Each geo uses its
#' own median log ratio as the center, and the spread comes from residuals around
#' those centers, pooled across all geos.
#'
#' Non-negative aheads (target not yet reported) return a null forecast.
#'
#' @param epi_data an `epi_archive` (the runner passes the truncated archive when
#'   the grid row sets `needs_archive = TRUE`).
#' @param outcome the outcome column.
#' @param ahead days from the forecast date (see [archive_forecast_date]) to the
#'   target week; must be negative.
#' @param primary_source if the archive has a `source` key, only this source is
#'   used.
#' @param window_weeks number of most recent settled weeks to learn ratios from.
#' @param settled_days a week counts as finalized once it is this many days
#'   older than `versions_end`.
#' @param min_geo_obs geos with fewer ratios than this use the pooled median as
#'   their center.
#' @param quantile_levels quantile levels to output.
#' @seealso [scaled_pop_seasonal_revision]
#' @export
revision_ratio_nowcast <- function(
  epi_data,
  outcome,
  ahead = -7,
  primary_source = "nhsn",
  window_weeks = 10L,
  settled_days = 42L,
  min_geo_obs = 4L,
  quantile_levels = covidhub_probs(),
  ...
) {
  if (!inherits(epi_data, "epi_archive")) {
    cli::cli_abort("revision_ratio_nowcast() expects an epi_archive; did the runner set needs_archive = TRUE?")
  }
  if (ahead >= 0) {
    return(make_null_forecast())
  }
  versions_end <- epi_data$versions_end
  forecast_date <- archive_forecast_date(epi_data)
  target_tv <- forecast_date + ahead
  # How long the target week has been reported; past weeks are compared at this lag.
  report_lag <- as.integer(versions_end - target_tv)

  dt <- data.table::as.data.table(epi_data$DT)
  grp_keys <- setdiff(key_colnames(epi_data), c("time_value", "version"))
  if ("source" %in% names(dt)) {
    dt <- dt[source == primary_source]
  }
  dt <- dt[!is.na(get(outcome))]
  if (nrow(dt) == 0) {
    return(make_null_forecast())
  }
  geo_rows <- unique(dt[, grp_keys, with = FALSE])

  # Value of each (geo, week) as of a version; rolls back to the last release on
  # or before it.
  asof <- function(time_values, versions) {
    q <- geo_rows[, .(time_value = time_values, version = versions), by = grp_keys]
    q[, value := roll_asof_value(dt, outcome, grp_keys, q)]
    q
  }

  # Ratios for the most recent settled weeks, each at the same reporting lag.
  hist_tv <- sort(unique(dt$time_value[dt$time_value <= versions_end - settled_days]), decreasing = TRUE)
  hist_tv <- head(hist_tv, window_weeks)
  if (length(hist_tv) == 0) {
    return(make_null_forecast())
  }
  early <- asof(hist_tv, hist_tv + report_lag)
  final <- asof(hist_tv, versions_end)
  ratios <- as_tibble(early) %>%
    rename(reported = value) %>%
    left_join(as_tibble(final) %>% select(-version) %>% rename(final = value), by = c(grp_keys, "time_value")) %>%
    filter(reported > 0, final > 0) %>%
    mutate(log_ratio = log(final / reported))
  if (nrow(ratios) == 0) {
    return(make_null_forecast())
  }

  pooled_center <- median(ratios$log_ratio)
  centers <- ratios %>%
    group_by(geo_value) %>%
    summarize(center = if (n() >= min_geo_obs) median(log_ratio) else pooled_center, .groups = "drop")
  resid_q <- ratios %>%
    left_join(centers, by = "geo_value") %>%
    mutate(resid = log_ratio - center) %>%
    pull(resid) %>%
    quantile(quantile_levels, names = FALSE)

  # The target week's value as reported now.
  current <- asof(target_tv, versions_end) %>%
    as_tibble() %>%
    filter(!is.na(value)) %>%
    left_join(centers, by = "geo_value") %>%
    mutate(center = coalesce(center, pooled_center))
  missing_geos <- setdiff(geo_rows$geo_value, current$geo_value)
  if (length(missing_geos) > 0) {
    cli::cli_warn("revision_ratio_nowcast: {target_tv} isn't reported yet for {missing_geos}; no forecast for them.")
  }
  if (nrow(current) == 0) {
    return(make_null_forecast())
  }

  current %>%
    select(geo_value, reported = value, center) %>%
    tidyr::expand_grid(tibble(quantile = quantile_levels, resid = resid_q)) %>%
    transmute(
      geo_value,
      forecast_date = forecast_date,
      target_end_date = target_tv,
      quantile,
      value = pmax(0, reported * exp(center + resid))
    )
}
