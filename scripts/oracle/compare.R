#!/usr/bin/env Rscript
# Oracle compare (REFACTOR.md, Exp 0): diff two capture labels target-by-target.
# Exact on key columns, relative tolerance on value columns. Empty diff = the
# refactor preserved behavior. Exits non-zero if anything differs.
#
# Usage:
#   distrobox enter rocker -- \
#     Rscript scripts/oracle/compare.R flu_hosp_prod baseline refactored [tol]
suppressPackageStartupMessages({
  library(dplyr)
  library(cli)
})

# Each spec is "<project>:<label>" (or bare "<label>" implying flu_hosp_prod).
args <- commandArgs(trailingOnly = TRUE)
label_a <- if (length(args) >= 1) args[[1]] else "flu_hosp_prod:baseline"
label_b <- if (length(args) >= 2) args[[2]] else "flu_hosp_prod:refactored"
tol <- if (length(args) >= 3) as.numeric(args[[3]]) else 1e-9
out_root <- Sys.getenv("ORACLE_OUT_DIR", "cache/oracle")

spec_dir <- function(spec) {
  parts <- strsplit(spec, ":", fixed = TRUE)[[1]]
  if (length(parts) == 2) file.path(out_root, parts[[1]], parts[[2]]) else file.path(out_root, "flu_hosp_prod", spec)
}
dir_a <- spec_dir(label_a)
dir_b <- spec_dir(label_b)

# Columns treated as numeric measurements (tolerance); everything else is a key.
# Miss one and it becomes a sort key: a tiny numeric change then scrambles row
# alignment and surfaces as a bogus "key columns differ" instead of a value diff.
value_cols <- c(
  "value", "prediction", "oracle_value", "scale",
  # scoringutils metrics
  "wis", "ae_median", "interval_coverage_50", "interval_coverage_90"
)

files_a <- list.files(dir_a, pattern = "\\.parquet$")
files_b <- list.files(dir_b, pattern = "\\.parquet$")
only_a <- setdiff(files_a, files_b)
only_b <- setdiff(files_b, files_a)
if (length(only_a)) cli_alert_warning("only in {label_a}: {paste(only_a, collapse=', ')}")
if (length(only_b)) cli_alert_warning("only in {label_b}: {paste(only_b, collapse=', ')}")

any_diff <- FALSE
for (f in intersect(files_a, files_b)) {
  name <- sub("\\.parquet$", "", f)
  a <- as.data.frame(nanoparquet::read_parquet(file.path(dir_a, f)))
  b <- as.data.frame(nanoparquet::read_parquet(file.path(dir_b, f)))

  if (!identical(sort(names(a)), sort(names(b)))) {
    cli_alert_danger("{name}: column sets differ")
    any_diff <- TRUE
    next
  }
  if (nrow(a) != nrow(b)) {
    cli_alert_danger("{name}: nrow {nrow(a)} vs {nrow(b)}")
    any_diff <- TRUE
    next
  }

  keys <- setdiff(names(a), value_cols)
  a <- a %>% arrange(across(all_of(keys)))
  b <- b %>% arrange(across(all_of(keys)))

  if (!identical(a[keys], b[keys])) {
    # Name the offending columns: "key columns differ" alone forces a manual bisect.
    bad <- keys[!vapply(keys, function(k) identical(a[[k]], b[[k]]), logical(1))]
    cli_alert_danger("{name}: key columns differ after sort: {paste(bad, collapse=', ')}")
    for (k in bad) {
      ua <- setdiff(unique(as.character(a[[k]])), unique(as.character(b[[k]])))
      ub <- setdiff(unique(as.character(b[[k]])), unique(as.character(a[[k]])))
      if (length(ua) || length(ub)) {
        cli_alert_info("  {k}: only in {label_a}: [{paste(head(ua, 5), collapse=', ')}]; only in {label_b}: [{paste(head(ub, 5), collapse=', ')}]")
      } else {
        cli_alert_info("  {k}: same value set, different row alignment")
      }
    }
    any_diff <- TRUE
    next
  }

  worst <- 0
  for (v in intersect(value_cols, names(a))) {
    d <- abs(a[[v]] - b[[v]])
    rel <- d / pmax(abs(a[[v]]), 1e-12)
    worst <- max(worst, max(rel, na.rm = TRUE), 0)
  }
  if (worst > tol) {
    cli_alert_danger("{name}: max rel diff {signif(worst, 3)} > tol {tol}")
    any_diff <- TRUE
  } else {
    cli_alert_success("{name}: match (max rel diff {signif(worst, 3)})")
  }
}

if (any_diff) {
  cli_alert_danger("DIFFERENCES FOUND")
  quit(status = 1)
}
cli_alert_success("ALL MATCH")
