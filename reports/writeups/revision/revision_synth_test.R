suppressPackageStartupMessages(source("R/load_all.R"))

# Sanity-check scaled_pop_seasonal_revision on synthetic data with a known,
# large revision structure.
#
# Design: finals are iid lognormal per (geo, tv); each Saturday time_value is
# first published FIRST_VERSION_OFFSET days later (Wednesday) at PRELIM_FRAC of
# the final, converging linearly to the final over CONVERGE_DAYS.
#
# versions_end is set to max_Saturday + FIRST_VERSION_OFFSET (the first
# Wednesday after the last Saturday), giving reporting_latency_days = 4.
# ceiling(4/7)*7 = 7, so design_ahead = ahead + 7.
#
# For ahead=-7: design_ahead = 0 → target = finalized(anchor), lag-0 =
# preliminary(anchor). Model should learn coef(value_lag_0) ≈ 1/PRELIM_FRAC.
#
# For ahead=0: design_ahead = 7 → target = finalized(anchor + 7) = next
# Saturday. In iid data lag-0 carries zero information about a different week,
# so coef(value_lag_0) ≈ 0. This verifies the model does not inject spurious
# signal across weeks.

set.seed(42)

# --- Parameters ---
CONVERGE_DAYS <- 14L
FIRST_VERSION_OFFSET <- 4L  # Saturday + 4 days = Wednesday (first publication)
PRELIM_FRAC <- as.numeric(FIRST_VERSION_OFFSET) / CONVERGE_DAYS

cat(sprintf(
  "Preliminary fraction at first release (lag %d days): %.3f\n",
  FIRST_VERSION_OFFSET, PRELIM_FRAC
))
cat(sprintf(
  "Expected coef(value_lag_0) ahead=-7 ≈ %.1f  (same-week revision)\n",
  1 / PRELIM_FRAC
))
cat("Expected coef(value_lag_0) ahead=0  ≈ 0.0  (different week in iid data)\n\n")

# --- Build synthetic archive ---
n_geos <- 15L
geo_values <- paste0("s", sprintf("%02d", seq_len(n_geos)))
geo_pops   <- runif(n_geos, 1e6, 1e7)
names(geo_pops) <- geo_values

tv_start <- as.Date("2022-10-01")
tv_end   <- as.Date("2025-04-01")

all_dates    <- seq(tv_start, tv_end, by = 1L)
all_saturdays <- all_dates[lubridate::wday(all_dates) == 7L]

# Finals are iid lognormal — no week-to-week correlation — so lag-7 (a
# different Saturday's preliminary) carries zero information about the target.
n_tv <- length(all_saturdays)
noise_mat    <- matrix(exp(rnorm(n_geos * n_tv, 0, 0.4)), nrow = n_geos, ncol = n_tv)
final_matrix <- (geo_pops / 1e5) * noise_mat
rownames(final_matrix) <- geo_values
colnames(final_matrix) <- as.character(all_saturdays)

rows <- purrr::map_dfr(seq_along(all_saturdays), function(ti) {
  tv <- all_saturdays[[ti]]
  lag_seq  <- seq(FIRST_VERSION_OFFSET, CONVERGE_DAYS + 14L, by = 7L)
  # Keep only Wednesday versions (wday == 4 in R's default Sunday-origin)
  versions <- (tv + lag_seq)[lubridate::wday(tv + lag_seq) == 4L]
  if (length(versions) == 0L) return(tibble())
  purrr::map_dfr(geo_values, function(geo) {
    final_v    <- final_matrix[geo, as.character(tv)]
    rev_factor <- pmin(1, as.numeric(versions - tv) / CONVERGE_DAYS)
    tibble(geo_value = geo, time_value = tv, version = versions, value = final_v * rev_factor)
  })
})

synth_archive <- as_epi_archive(
  rows %>% arrange(geo_value, time_value, version),
  geo_type  = "custom",
  time_type = "date",
  other_keys = character()
)
# Set versions_end to max_Saturday + FIRST_VERSION_OFFSET (first Wednesday after
# the last Saturday). This gives reporting_latency_days = FIRST_VERSION_OFFSET =
# 4, so ceiling(4/7)*7 = 7 and design_ahead = ahead + 7.
synth_archive$versions_end <- max(all_saturdays) + FIRST_VERSION_OFFSET

cat("Archive summary:\n")
cat("  rows:", nrow(rows), "\n")
cat("  time_values:", n_tv, "\n")
cat("  geos:", n_geos, "\n")
cat("  versions_end:", format(synth_archive$versions_end), "\n")
cat("  max time_value:", format(max(all_saturdays)), "\n")
cat(sprintf("  reporting_latency_days: %d  →  design_ahead offset = 7\n\n",
  as.integer(synth_archive$versions_end - max(all_saturdays))))

run_synth <- function(ahead_val, label, expected_lag0) {
  fit <- scaled_pop_seasonal_revision(
    synth_archive,
    outcome = "value",
    extra_sources = character(),
    ahead = ahead_val,
    lags = c(0L, 7L, 14L),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = FALSE,
    return_fit = TRUE
  )
  if (!inherits(fit, "model_fit")) {
    cat(sprintf("\n%s: null_result (too few training rows)\n", label))
    return(invisible(NULL))
  }
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("expected coef(value_lag_0) ≈ %.1f\n", expected_lag0))
  coefs <- coef(fit$fit)
  med_col <- which(abs(as.numeric(sub("tau= ", "", colnames(coefs))) - 0.5) < 0.01)
  if (length(med_col) == 1L) {
    cat("Median (tau=0.5) coefficients:\n")
    print(round(coefs[, med_col, drop = FALSE], 3))
  }
  cat("Full coefficient table:\n")
  print(round(coefs, 3))
  invisible(fit)
}

# ahead=-7: design_ahead = -7 + 7 = 0.
# target = finalized(anchor + 0) = finalized(anchor).
# lag-0 = preliminary(anchor, age=4 days) = anchor's final * (4/14).
# Expected: coef(lag_0) = 14/4 = 3.5.  coef(lag_7), coef(lag_14) ≈ 0 (iid).
run_synth(-7L,
  "ahead=-7 (design_ahead=0: target=finalized(anchor), lag_0=preliminary(anchor))",
  expected_lag0 = 1 / PRELIM_FRAC
)

# ahead=0: design_ahead = 0 + 7 = 7.
# target = finalized(anchor + 7) = next Saturday's final (different, iid week).
# Expected: coef(lag_0) ≈ 0.  No artificial signal across weeks.
run_synth(0L,
  "ahead=0  (design_ahead=7: target=next-Saturday final, lag_0=current preliminary; iid → coef≈0)",
  expected_lag0 = 0
)
