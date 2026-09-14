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
      tryCatch(
        return(qs::qread(cache_path)),
        error = function(e) message("revision_cache: corrupt cache at ", cache_path, ", recomputing")
      )
    }
  }

  archive_dt <- data.table::as.data.table(archive$DT)
  if (is.null(versions)) {
    versions <- sort(unique(archive_dt$version))
  }
  message("revision_predictor_design: nrow(archive_dt)=", nrow(archive_dt), " n_versions=", length(versions))

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
    if (!is.null(cache_path)) {
      tmp <- paste0(cache_path, ".tmp.", Sys.getpid())
      qs::qsave(null_design, tmp)
      file.rename(tmp, cache_path)
    }
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
    tmp <- paste0(cache_path, ".tmp.", Sys.getpid())
    qs::qsave(design, tmp)
    file.rename(tmp, cache_path)
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
