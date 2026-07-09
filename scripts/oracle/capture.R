#!/usr/bin/env Rscript
# Oracle capture (REFACTOR.md, Exp 0): build a targets project and snapshot its
# archives (frozen inputs) + forecast/ensemble frames (golden outputs) to parquet
# so refactors can be diffed for behavior preservation.
#
# Usage (run through distrobox per CLAUDE.md):
#   distrobox enter rocker -- \
#     env TAR_RUN_PROJECT=flu_hosp_prod ORACLE_LABEL=baseline \
#     Rscript scripts/oracle/capture.R
#
# Env:
#   TAR_RUN_PROJECT   targets project (default flu_hosp_prod)
#   ORACLE_LABEL      label subdir, e.g. baseline | refactored (default baseline)
#   ORACLE_OUT_DIR    output root (default cache/oracle)
#   ORACLE_SKIP_MAKE  "TRUE" to read an existing store without rebuilding
#   BACKTEST_MODE     forwarded to the covid pipeline (FALSE = prod-latest)
#   EVALUATION_N_DATES  for the flu_hosp_evaluation project, keep only the last N dates
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

project <- Sys.getenv("TAR_RUN_PROJECT", "flu_hosp_prod")
label <- Sys.getenv("ORACLE_LABEL", "baseline")
out_root <- Sys.getenv("ORACLE_OUT_DIR", "cache/oracle")
skip_make <- toupper(Sys.getenv("ORACLE_SKIP_MAKE", "FALSE")) %in% c("TRUE", "1", "YES")

store <- targets::tar_config_get("store", project = project)
script <- targets::tar_config_get("script", project = project)
out_dir <- file.path(out_root, project, label)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cli::cli_inform(c(
  "i" = "Oracle capture",
  "*" = "project   = {project}",
  "*" = "store     = {store}",
  "*" = "label     = {label}",
  "*" = "out_dir   = {out_dir}",
  "*" = "skip_make = {skip_make}"
))

if (!skip_make) {
  cli::cli_alert_info("Building pipeline (hits the network; may take a while)...")
  targets::tar_make(store = store, script = script)
}

# Frozen INPUTS (epi_archives / epi_dfs) and golden OUTPUTS (forecast/ensemble/
# score frames). Mode-dependent targets that are absent are skipped, not fatal.
snapshot_targets <- c(
  # archives
  "joined_archive_data",
  "nhsn_archive_data",
  "nssp_archive_data",
  "nhsn_latest_data",
  "nssp_latest_data",
  "joined_latest_extra_data",
  # forecasts + ensembles
  "forecast_nhsn_full",
  "forecast_nssp_full",
  "local_forecasts_and_ensembles_nhsn",
  "local_forecasts_and_ensembles_nssp",
  # scores (backtest mode)
  "scores_nhsn",
  "scores_nssp"
)

# Coerce a target value to a flat data.frame nanoparquet can write.
to_frame <- function(x) {
  if (inherits(x, "epi_archive")) {
    df <- as.data.frame(x$DT)
  } else if (is.data.frame(x)) {
    df <- as.data.frame(x)
  } else if (is.list(x) && !is.null(names(x)) && all(vapply(x, is.data.frame, logical(1)))) {
    # e.g. list(nhsn=, nssp=); tag rows with the list key and bind
    df <- dplyr::bind_rows(lapply(names(x), function(nm) {
      dplyr::mutate(as.data.frame(x[[nm]]), .list_key = nm)
    }))
  } else {
    stop("unsupported target type: ", paste(class(x), collapse = "/"))
  }
  # nanoparquet is strict: drop factors to character, list-cols are unsupported.
  df[] <- lapply(df, function(col) if (is.factor(col)) as.character(col) else col)
  df
}

manifest <- list()
for (name in snapshot_targets) {
  val <- tryCatch(targets::tar_read_raw(name, store = store), error = function(e) e)
  if (inherits(val, "condition")) {
    cli::cli_alert_warning("skip {name}: {conditionMessage(val)}")
    next
  }
  df <- tryCatch(to_frame(val), error = function(e) e)
  if (inherits(df, "condition")) {
    cli::cli_alert_warning("skip {name}: {conditionMessage(df)}")
    next
  }
  path <- file.path(out_dir, paste0(name, ".parquet"))
  nanoparquet::write_parquet(df, path)
  manifest[[name]] <- data.frame(target = name, nrow = nrow(df), ncol = ncol(df))
  cli::cli_alert_success("wrote {name}: {nrow(df)} x {ncol(df)}")
}

man <- dplyr::bind_rows(manifest)
readr::write_csv(man, file.path(out_dir, "_manifest.csv"))
cli::cli_alert_success("Captured {nrow(man)} target(s) to {out_dir}")
