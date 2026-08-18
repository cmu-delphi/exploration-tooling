#' As-of value of a column via a data.table rolling join.
#'
#' For each query row `(keys, time_value, version)` returns the value of `col`
#' at that `(keys, time_value)` as observed in the latest archive `version <=`
#' the query version -- i.e. `epix_as_of(version)[time_value]`, but as a single
#' vectorized rolling join over the compact archive rather than a per-version
#' materialization. `queries` order is preserved.
#' @keywords internal
roll_asof_value <- function(archive_dt, col, grp_keys, queries) {
  col_dt <- archive_dt[!is.na(get(col)), c(grp_keys, "time_value", "version"), with = FALSE]
  col_dt[, .val := archive_dt[!is.na(get(col)), get(col)]]
  data.table::setkeyv(col_dt, c(grp_keys, "time_value", "version"))
  col_dt[queries, on = c(grp_keys, "time_value", "version"), roll = TRUE][[".val"]]
}

#' Date/ahead-independent revision-aware predictor design.
#'
#' The expensive, cacheable core of [archive_to_revision_predictors]: for every
#' as-of `version`, the vintage a real-time run then would have seen. It anchors
#' on the latest week `target_col` was reported by that version (respecting
#' reporting latency rather than assuming the current week is in), and reads each
#' `(col, lag)` at `anchor - lag` from that vintage. Built with data.table
#' rolling joins over the compact archive -- O(n log n) versus the quadratic
#' per-version `epix_as_of` materialization -- so it is cheap enough to run per
#' forecast date, and cacheable across forecast horizons and reruns.
#'
#' @param cache_key if non-NULL, cache the (date/ahead-independent) design to
#'   `cache/revision_cache/` keyed on this plus the archive hash and the
#'   lags/cols spec, so repeat calls on the same archive (e.g. across aheads)
#'   skip recomputation.
#' @param archive_hash precomputed `rlang::hash(archive)`; computed here when
#'   NULL and `cache_key` is set.
#' @inheritParams archive_to_revision_predictors
#' @keywords internal
#'
#' @importFrom epiprocess key_colnames
revision_predictor_design <- function(
  archive,
  lags,
  cols = "value",
  target_col = cols[[1]],
  versions = NULL,
  cache_key = NULL,
  archive_hash = NULL
) {
  grp_keys <- setdiff(key_colnames(archive), c("time_value", "version"))
  if (!is.list(lags)) {
    lags <- rep(list(lags), length(cols))
  }
  if (length(lags) != length(cols)) {
    cli::cli_abort("`lags` must be a single vector or a list parallel to `cols` (length {length(cols)}).")
  }
  lag_spec <- purrr::imap(cols, \(col, ii) tibble(col = col, lag = lags[[ii]])) %>%
    bind_rows() %>%
    mutate(name = paste0(col, "_lag_", lag))

  cache_path <- NULL
  if (!is.null(cache_key)) {
    hash <- archive_hash %||% rlang::hash(archive)
    spec_hash <- rlang::hash(list(lags, cols, target_col, versions))
    dir.create("cache/revision_cache", showWarnings = FALSE, recursive = TRUE)
    cache_path <- glue::glue("cache/revision_cache/{cache_key}_{hash}_{spec_hash}.qs")
    if (file.exists(cache_path)) {
      return(qs::qread(cache_path))
    }
  }

  archive_dt <- data.table::as.data.table(archive$DT)
  if (is.null(versions)) {
    versions <- sort(unique(archive_dt$version))
  }

  # Anchor per (keys, version): the latest week whose target was first reported
  # by that version. `first_v` = each week's first-reporting version; a running
  # max of time_value over first_v gives the latest reported week at any cutoff,
  # then a rolling join carries it to each requested version. Collapsing ties in
  # first_v (a backfill block first-reports many weeks at one version) to their
  # running-max keeps the roll join from picking an arbitrary member of the block.
  reported <- archive_dt[!is.na(get(target_col)), c(grp_keys, "time_value", "version"), with = FALSE]

  if (nrow(reported) == 0) {
    null_design <- tibble(
      !!!setNames(lapply(grp_keys, \(k) character()), grp_keys),
      version = as.Date(character()),
      time_value = as.Date(character()),
      !!!setNames(lapply(lag_spec$name, \(n) numeric()), lag_spec$name)
    )
    if (!is.null(cache_path)) qs::qsave(null_design, cache_path)
    return(null_design)
  }
  first_rep <- reported[, .(first_v = min(version)), by = c(grp_keys, "time_value")]
  data.table::setorderv(first_rep, c(grp_keys, "first_v"))
  first_rep[, anchor := as.Date(cummax(as.integer(time_value))), by = grp_keys]
  first_rep <- first_rep[, .(anchor = as.Date(max(anchor))), by = c(grp_keys, "first_v")]
  ref <- unique(archive_dt[, grp_keys, with = FALSE])[, .(version = versions), by = grp_keys]
  data.table::setkeyv(first_rep, c(grp_keys, "first_v"))
  design <- first_rep[ref, on = c(grp_keys, "first_v" = "version"), roll = TRUE][!is.na(anchor)]
  data.table::setnames(design, "first_v", "version")

  # Each (col, lag): the vintage value at anchor - lag as of the row's version.
  for (jj in seq_len(nrow(lag_spec))) {
    spec <- lag_spec[jj, ]
    queries <- design[, c(grp_keys, "version"), with = FALSE]
    queries[, time_value := design$anchor - spec$lag]
    design[[spec$name]] <- roll_asof_value(archive_dt, spec$col, grp_keys, queries)
  }
  data.table::setnames(design, "anchor", "time_value")
  design <- as_tibble(design) %>%
    relocate(all_of(c(grp_keys, "version", "time_value")))

  if (!is.null(cache_path)) {
    qs::qsave(design, cache_path)
  }
  design
}

#' Build a revision-aware predictor tibble from an archive.
#'
#' For every as-of `version` this reconstructs the vintage a real-time run made
#' at that version would have seen, anchors on the most recent observation
#' available then (`time_value`, the latest week `target_col` reports, so
#' reporting latency is respected rather than pretending the current week is
#' already in), and reads each column in `cols` at the requested `lags` back from
#' that anchor. When `ahead` is given it also attaches the finalized value of
#' `target_col` `ahead` units past the anchor, taken from the latest version in
#' the archive. The result is the design matrix a revision-aware forecaster
#' trains on: each row is a past forecast opportunity whose lags carry the
#' vintage then available and whose target carries the value that vintage was
#' eventually revised to.
#'
#' Built with data.table rolling joins (see [revision_predictor_design]) rather
#' than a per-version `epix_as_of`, so it scales to long archives.
#'
#' @param archive an `epi_archive`.
#' @param lags integer lags (in `time_value` units, counted back from each
#'   version's anchor week) to build for every column in `cols`, or a list of
#'   per-column lag vectors parallel to `cols`.
#' @param cols predictor columns to build lags of (e.g. the outcome plus any
#'   exogenous signals). Defaults to `"value"`.
#' @param ahead finalized-target horizon in `time_value` units past the anchor.
#'   `NULL` omits the target column entirely (predictors only).
#' @param target_col the column the finalized target is drawn from; defaults to
#'   the first of `cols`.
#' @param versions the as-of dates to slice at; defaults to every `version`
#'   present in the archive.
#' @param cache_key,archive_hash forwarded to [revision_predictor_design] to
#'   cache the (date/ahead-independent) predictor design.
#' @return a tibble of the archive's key columns, `version` (the as-of date),
#'   `time_value` (the anchor week), one `{col}_lag_{lag}` column per requested
#'   (column, lag), and -- when `ahead` is set -- `{target_col}_target`.
#'
#' @importFrom epiprocess key_colnames
#' @export
archive_to_revision_predictors <- function(
  archive,
  lags,
  cols = "value",
  ahead = NULL,
  target_col = cols[[1]],
  versions = NULL,
  cache_key = NULL,
  archive_hash = NULL
) {
  grp_keys <- setdiff(key_colnames(archive), c("time_value", "version"))
  design <- revision_predictor_design(archive, lags, cols, target_col, versions, cache_key, archive_hash)
  if (is.null(ahead) || nrow(design) == 0) {
    return(design)
  }

  # Finalized target: value of target_col at anchor + ahead, as of the latest
  # version (a rolling join to versions_end), joined back onto each anchor row.
  target_name <- paste0(target_col, "_target")
  archive_dt <- data.table::as.data.table(archive$DT)
  queries <- data.table::as.data.table(design)[, c(grp_keys, "time_value"), with = FALSE]
  queries[, time_value := time_value + ahead]
  queries[, version := archive$versions_end]
  design[[target_name]] <- roll_asof_value(archive_dt, target_col, grp_keys, queries)
  design
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
  seasonal_backward_window = 5 * 7,
  seasonal_forward_window = 3 * 7,
  trainer = epipredict::quantile_reg(),
  quantile_levels = covidhub_probs(),
  clip_lower = TRUE,
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


  # Revision-aware design: as-of lags for every base column plus the finalized
  # outcome target, then restrict to the genuinely-revised primary source.
  # cache the (ahead-independent) predictor design so a date's aheads, run in
  # separate workers, share one slide via the on-disk cache.
  design <- archive_to_revision_predictors(
    epi_data,
    lags = lags,
    cols = base_cols,
    ahead = ahead,
    target_col = outcome,
    cache_key = "revision_design"
  )
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

  if (nrow(train) < length(lag_cols) + 1 || nrow(forecast_rows) == 0) {
    return(null_result)
  }

  # One pooled quantile regression across geos (pop scaling makes them
  # comparable); predict the forecast row per geo.
  form <- reformulate(lag_cols, response = target_name)
  model <- quantile_reg(quantile_levels = quantile_levels)
  fitted <- fit(model, form, data = train)
  preds <- predict(fitted, forecast_rows)$.pred
  quantile_mat <- as.matrix(preds)
  levels_out <- hardhat::extract_quantile_levels(preds)

  out <- purrr::map(seq_len(nrow(forecast_rows)), function(ii) {
    tibble(
      geo_value = forecast_rows$geo_value[[ii]],
      source = forecast_rows$source[[ii]],
      forecast_date = epi_data$versions_end,
      target_end_date = forecast_rows$time_value[[ii]] + ahead,
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


#' Replicate a base column's whitening params under a set of design-matrix
#' column names, so [data_whitening]/[data_coloring] can transform every lag of a
#' variable with the parameters learned once for that variable.
#' @keywords internal
replicate_whitening_params <- function(base_params, base_col, cols) {
  if (is.null(base_params)) {
    return(NULL)
  }
  out <- base_params %>% select(source, geo_value)
  for (col in cols) {
    out[[paste0(col, "_center")]] <- base_params[[paste0(base_col, "_center")]]
    out[[paste0(col, "_scale")]] <- base_params[[paste0(base_col, "_scale")]]
  }
  out
}
