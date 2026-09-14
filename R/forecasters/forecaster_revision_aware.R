#' Flag (geo_value, version) pairs with anomalous late revisions.
#'
#' A `(geo_value, version)` is flagged if there are at least `min_obs` archive
#' rows with `lag_weeks > n_weeks` for that pair, AND at least one of those rows
#' has a deviation from the finalized value exceeding `threshold` for a
#' `time_value` whose finalized count is at least `min_value` (low-count
#' periods are excluded because `threshold`% of a tiny count is just noise).
#'
#' @param archive_dt data.table of the archive's DT slot.
#' @param outcome name of the outcome column.
#' @param n_weeks revisions after this many weeks are considered "late".
#' @param threshold fractional deviation from finalized that counts as anomalous.
#' @param min_value absolute finalized value below which the threshold check is
#'   skipped.
#' @param min_obs minimum number of late-window rows required before a pair can
#'   be flagged (guards against sparse early snapshots).
#' @return a data.table with columns `geo_value` and `version`.
#' @keywords internal
flag_revision_outlier_versions <- function(
  archive_dt,
  outcome,
  n_weeks,
  threshold = 0.10,
  min_value = 30,
  min_obs = 5L
) {
  # All non-NA (geo, time_value, version) triples for the outcome column.
  vintage_obs <- as_tibble(archive_dt) %>%
    filter(!is.na(.data[[outcome]])) %>%
    select(geo_value, time_value, version, val = all_of(outcome))

  # Most-recent-version value per (geo, time_value)
  recent_values <- vintage_obs %>%
    group_by(geo_value, time_value) %>%
    slice_max(version, n = 1) %>%
    ungroup() %>%
    select(geo_value, time_value, value_latest = val)

  vintage_obs %>%
    left_join(recent_values, by = c("geo_value", "time_value")) %>%
    mutate(lag_weeks = as.numeric(version - time_value) / 7) %>%
    filter(lag_weeks > n_weeks) %>%
    group_by(geo_value, version) %>%
    summarise(
      is_outlier = any(
        abs(val - value_latest) / (abs(value_latest) + 1e-6) > threshold &
          abs(value_latest) >= min_value
      ),
      n_late_obs = n(),
      .groups = "drop"
    ) %>%
    filter(is_outlier, n_late_obs >= min_obs) %>%
    select(geo_value, version)
}

#' Empirical finalization lag at a given coverage level.
#'
#' Returns the minimum lag (weeks, measured from time_value) such that at least
#' `coverage` of (geo, time_value) pairs have their last >convergence_threshold
#' revision within that lag. Used to drop under-finalized training targets from
#' the revision-aware forecaster.
#'
#' Only time_values >= first release date are included so historical backfill
#' rows (pre-NHSN) are excluded. The initial release version itself is excluded
#' from the revision history.
#'
#' @param archive epi_archive used as-is (before any weekday or source filter).
#' @param outcome name of the outcome column.
#' @param convergence_threshold fractional deviation from final that counts as
#'   not yet converged (default 0.05).
#' @param coverage target coverage level (default 0.95).
#' @param min_final_value pairs with |final_value| below this are excluded.
#' @return a single numeric: the lag in weeks at the given coverage level.
#' @keywords internal
compute_finalization_lag_weeks <- function(
  archive,
  outcome,
  convergence_threshold = 0.05,
  coverage = 0.95,
  min_final_value = 10L
) {
  arch_dt <- as_tibble(data.table::as.data.table(archive$DT)) %>%
    filter(!is.na(.data[[outcome]]))
  if (nrow(arch_dt) == 0L) {
    return(0)
  }
  first_release <- min(arch_dt$version)
  arch_dt %>%
    filter(version > first_release, time_value >= first_release) %>%
    group_by(geo_value, time_value) %>%
    mutate(
      final_value  = .data[[outcome]][which.max(version)],
      lag_weeks    = as.numeric(version - time_value) / 7,
      revision_pct = abs(.data[[outcome]] - final_value) / (abs(final_value) + 1e-6)
    ) %>%
    filter(abs(final_value) >= min_final_value) %>%
    summarise(
      convergence_wks = {
        unsettled <- lag_weeks[revision_pct > convergence_threshold]
        if (length(unsettled) == 0L) 0 else max(unsettled)
      },
      .groups = "drop"
    ) %>%
    pull(convergence_wks) %>%
    quantile(probs = coverage, na.rm = TRUE) %>%
    unname()
}

#' Scaled pop seasonal, revision-aware
#'
#' A variant of [scaled_pop_seasonal] that is aware of data revisions. Instead of
#' being handed a single as-of `epi_df` snapshot, it receives the whole archive
#' (truncated to the forecast date, main and auxiliary columns alike) and builds
#' its training design with [archive_to_revision_predictors]: every training
#' row's lags are the vintage that a real-time run at that row's `time_value`
#' would have seen, while its target is the finalized value. The forecast row --
#' the archive's most recent `time_value` -- carries the lags as of the forecast
#' date, exactly what a live run would predict from.
#'
#' It augments the design the same ways `scaled_pop_seasonal` does under the
#' `"window"` seasonal method only: population scaling to rates, per-(source,
#' geo) whitening, and a seasonal training window around the forecast's phase of
#' season. The PCA / climatological / indicator seasonal methods and the
#' residual-training path are intentionally dropped -- this forecaster is the
#' `"window"` method made revision-aware, nothing else.
#'
#' @param epi_data an `epi_archive` (not an `epi_df`), already truncated to the
#'   forecast date by the runner. The forecast is made as of its `versions_end`.
#' @param outcome the name of the target column in the archive (e.g. `"value"`).
#' @param extra_sources auxiliary predictor columns to lag alongside the outcome.
#' @param primary_source the source key the forecast is made for: the forecast
#'   row is this source's latest-version vintage, and predictions are colored back
#'   with its whitening params.
#' @param train_sources sources to pool into training (defaults to just
#'   `primary_source`). `primary_source` is always included. On a mixed archive
#'   this is the include/exclude-faux-revisions knob: `primary_source` alone
#'   ("nhsn") trains on genuinely version-aware history only, while adding
#'   faux-versioned sources (e.g. "ILI+", "flusurv", whose `version ==
#'   time_value`) buys a longer training window at the cost of those rows not
#'   being truly revision-aware.
#' @param ahead forecast horizon, relative to the archive's `versions_end`, in
#'   the same `time_value` units as the archive (days for the weekly-Wednesday
#'   archives, so a multiple of 7).
#' @param lags integer lag vector applied to the outcome, or a list of per-column
#'   lag vectors parallel to `c(outcome, extra_sources)`.
#' @param pop_scaling whether to population-scale counts to rates.
#' @param scale_method,center_method,nonlin_method whitening parameters, as in
#'   [scaled_pop_seasonal].
#' @param seasonal_backward_window,seasonal_forward_window the seasonal training
#'   window (in days) kept around the forecast's season week.
#' @param trainer the (quantile) trainer; must be an [epipredict::quantile_reg].
#' @param quantile_levels the quantile levels to predict.
#' @param clip_lower whether to clip predictions at zero.
#' @seealso [archive_to_revision_predictors], [scaled_pop_seasonal]
#'
#' @importFrom epipredict quantile_reg
#' @importFrom parsnip fit
#' @importFrom rlang arg_match
#' @export
scaled_pop_seasonal_revision <- function(
  epi_data,
  outcome,
  extra_sources = character(),
  primary_source = "nhsn",
  train_sources = NULL,
  ahead = 7,
  lags = c(0, 7, 14),
  pop_scaling = TRUE,
  scale_method = c("quantile", "std", "none"),
  center_method = c("median", "mean", "none"),
  nonlin_method = c("quart_root", "none"),
  use_seasonal_window = TRUE,
  seasonal_backward_window = 5 * 7,
  seasonal_forward_window = 3 * 7,
  trainer = epipredict::quantile_reg(method = "fn"),
  quantile_levels = covidhub_probs(),
  clip_lower = TRUE,
  outlier_n_weeks = NULL,
  outlier_threshold = 0.10,
  outlier_min_value = 30,
  outlier_min_obs = 5L,
  return_fit = FALSE,
  ...
) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  if (!inherits(epi_data, "epi_archive")) {
    cli::cli_abort("scaled_pop_seasonal_revision() expects an epi_archive; did the runner set needs_archive = TRUE?")
  }
  if (!inherits(trainer, "quantile_reg")) {
    cli::cli_abort("scaled_pop_seasonal_revision() only supports a quantile_reg trainer.")
  }
  extra_sources <- unlist(extra_sources) %||% character()
  base_cols <- c(outcome, extra_sources)
  train_sources <- union(primary_source, unlist(train_sources) %||% primary_source)


  # For negative aheads, the most recent anchor week is still being actively
  # revised. Drop it so the model anchors on the previous (more finalized) week,
  # effectively adding one week of latency per negative-ahead step.
  # For negative aheads, the most recent anchor week is still being actively
  # revised and its lag-0 value directly covers the target week. Drop one week
  # per negative-ahead step so the model predicts from genuinely prior data.
  archive_for_design <- epi_data
  max_tv <- max(archive_for_design$DT$time_value, na.rm = TRUE)
  # Reporting latency: gap between forecast date and most recent data. Both
  # max_tv and versions_end are Wednesdays (nhsn_prod_archive shifts time_values
  # via floor_date + 3), so this is always a multiple of 7 in normal operation.
  # Rounded up to the nearest week for robustness against off-schedule runs.
  reporting_latency_days <- as.integer(epi_data$versions_end - max_tv)
  design_ahead <- ahead + ceiling(reporting_latency_days / 7L) * 7L

  # Revision-aware design: as-of lags for every base column plus the finalized
  # outcome target, then restrict to the genuinely-revised primary source.
  # cache the (ahead-independent) predictor design so a date's aheads, run in
  # separate workers, share one slide via the on-disk cache.
  message(format(epi_data$versions_end), " ahead=", ahead, " building design")
  design <- archive_to_revision_predictors(
    archive_for_design,
    lags = lags,
    cols = base_cols,
    ahead = design_ahead,
    target_col = outcome,
    cache_key = "revision_design"
  )
  message(format(epi_data$versions_end), " ahead=", ahead, " design nrow=", nrow(design))
  # Archives without a source key (e.g. the clean nhsn prod archive) are stamped
  # so the source-keyed whitening/coloring below still matches.
  if (!("source" %in% names(design))) {
    design$source <- primary_source
  }
  design <- design %>% filter(source %in% train_sources)
  target_name <- paste0(outcome, "_target")
  lag_cols <- grep("_lag_", names(design), value = TRUE)

  null_result <- tibble(
    geo_value = character(),
    forecast_date = as.Date(character()),
    target_end_date = as.Date(character()),
    quantile = numeric(),
    value = numeric()
  )
  if (nrow(design) == 0 || !(target_name %in% names(design))) {
    return(null_result)
  }

  # Whitening, learned per base column: the outcome from its finalized target,
  # exogenous columns from their contemporaneous (lag 0) observation. The same
  # per-(source, geo) params are then applied to every lag of that column (they
  # are the same underlying variable) so training and forecast rows are whitened
  # identically. The outcome's params are kept to color the prediction back.
  params_by_base <- list()
  for (base in base_cols) {
    learn_col <- if (base == outcome) target_name else paste0(base, "_lag_0")
    learn_df <- design %>% transmute(source, geo_value, !!base := .data[[learn_col]])
    params_by_base[[base]] <- calculate_whitening_params(learn_df, base, scale_method, center_method, nonlin_method)
  }
  for (base in base_cols) {
    cols_b <- grep(paste0("^", base, "_lag_"), names(design), value = TRUE)
    if (base == outcome) {
      cols_b <- c(cols_b, target_name)
    }
    design <- data_whitening(
      design,
      cols_b,
      replicate_whitening_params(params_by_base[[base]], base, cols_b),
      nonlin_method,
      join_cols = c("source", "geo_value")
    )
  }

  # Population scaling to rates, matching scaled_pop_seasonal's order (whiten
  # first, then scale). Reversed on the prediction below.
  if (pop_scaling) {
    census <- epidatasets::state_census %>% select(geo_value = abbr, pop)
    design <- design %>%
      left_join(census, by = "geo_value") %>%
      mutate(across(all_of(c(lag_cols, target_name)), ~ .x / pop * 1e5)) %>%
      select(-pop)
  }

  design <- design %>% add_season_info()

  # The forecast row is the primary source's latest-version vintage anchor -- the
  # live snapshot a real run at the forecast date would predict from.
  latest_primary_version <- design %>%
    filter(source == primary_source) %>%
    pull(version) %>%
    max()
  forecast_rows <- design %>%
    filter(source == primary_source, version == latest_primary_version) %>%
    drop_na(all_of(lag_cols))

  if (use_seasonal_window) {
    # Seasonal training window: keep training rows whose time_value sits within the
    # backward/forward window of any year's copy of the forecast anchor's season
    # week. Centering on the anchor (the last week actually observed), not the
    # calendar as-of, mirrors bake.step_epi_training_window()'s "last_data_season_week"
    # and guarantees the window has data even when the outcome lags the as-of date.
    forecast_season_week <- forecast_rows %>%
      filter(time_value == max(time_value)) %>%
      pull(season_week) %>%
      max()
    window_dates <- design %>%
      filter(season_week == forecast_season_week) %>%
      pull(time_value) %>%
      unique() %>%
      map(~ c(.x - seq_len(seasonal_backward_window), .x + 0:(seasonal_forward_window + ahead))) %>%
      unlist() %>%
      as.Date() %>%
      unique()
    train <- design %>%
      filter(time_value %in% window_dates) %>%
      drop_na(all_of(c(lag_cols, target_name)))
  } else {
    train <- design %>% drop_na(all_of(c(lag_cols, target_name)))
  }

  # Drop training rows whose target hasn't had enough time to finalize.
  # The 95th-percentile convergence lag (from the full pre-filter archive)
  # gives a data-driven cutoff: targets this recent are often still actively
  # revised. forecast_rows is built separately above and is not filtered here.
  finalization_cutoff_days <- as.integer(
    ceiling(compute_finalization_lag_weeks(epi_data, outcome) * 7)
  )
  message(
    format(epi_data$versions_end), " ahead=", ahead,
    " finalization_cutoff=", finalization_cutoff_days, "d"
  )
  train <- train %>%
    filter(
      as.integer(epi_data$versions_end - (time_value + design_ahead)) >= finalization_cutoff_days
    )

  if (!is.null(outlier_n_weeks) && !is.na(outlier_n_weeks)) {
    flagged_versions <- flag_revision_outlier_versions(
      data.table::as.data.table(epi_data$DT),
      outcome,
      n_weeks    = outlier_n_weeks,
      threshold  = outlier_threshold,
      min_value  = outlier_min_value,
      min_obs    = outlier_min_obs
    )
    train <- anti_join(train, flagged_versions, by = c("geo_value", "version"))
  }

  n_geos <- n_distinct(train$geo_value)
  if (nrow(train) < max(n_geos * 3L, 20L, length(lag_cols) + 1L) || nrow(forecast_rows) == 0) {
    return(null_result)
  }

  message(format(epi_data$versions_end), " ahead=", ahead, " fitting nrow(train)=", nrow(train), " nrow(forecast_rows)=", nrow(forecast_rows))
  # One pooled quantile regression across geos (pop scaling makes them
  # comparable); predict the forecast row per geo.
  form <- reformulate(lag_cols, response = target_name)
  trainer$args$quantile_levels <- rlang::enquo(quantile_levels)
  fitted <- fit(trainer, form, data = train)
  message(format(epi_data$versions_end), " ahead=", ahead, " fit done")
  if (return_fit) {
    return(fitted)
  }
  preds <- predict(fitted, forecast_rows)$.pred
  quantile_mat <- as.matrix(preds)
  levels_out <- hardhat::extract_quantile_levels(preds)

  out <- purrr::map(seq_len(nrow(forecast_rows)), function(ii) {
    tibble(
      geo_value = forecast_rows$geo_value[[ii]],
      source = forecast_rows$source[[ii]],
      forecast_date = lubridate::floor_date(epi_data$versions_end, "week", week_start = 7L) + 3L,
      target_end_date = epi_data$versions_end + ahead,
      quantile = levels_out,
      value = quantile_mat[ii, ]
    )
  }) %>%
    bind_rows()

  # Undo the whitening/scaling in reverse order: un-scale to counts, then color.
  if (pop_scaling) {
    out <- out %>%
      left_join(epidatasets::state_census %>% select(geo_value = abbr, pop), by = "geo_value") %>%
      mutate(value = value * pop / 1e5) %>%
      select(-pop)
  }
  out <- out %>%
    rename({{ outcome }} := value) %>%
    data_coloring(
      outcome,
      replicate_whitening_params(params_by_base[[outcome]], outcome, outcome),
      nonlin_method = nonlin_method,
      join_cols = c("source", "geo_value")
    ) %>%
    rename(value = {{ outcome }})

  if (clip_lower) {
    out <- out %>% mutate(value = pmax(0, value))
  }
  out %>% select(geo_value, forecast_date, target_end_date, quantile, value)
}
