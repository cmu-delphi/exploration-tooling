suppressPackageStartupMessages(source("R/load_all.R"))

# Diagnostic: how "finalized" are the revision-aware training targets?
#
# The revision-aware forecaster reads the finalized target from versions_end.
# For recent training rows, however, versions_end - (anchor + ahead) may be
# only a few weeks, meaning the "finalized" target is still actively revised.
# This script exposes that staleness and compares it to the implicit staleness
# in a non-revision-aware (as-of snapshot) forecaster.

ahead_days <- 7L # one week ahead; change to 14, 21, etc. to check other horizons
lags <- c(0L, 7L, 14L)

# --- Load archive via targets (flu_hosp_prod store) ---
Sys.setenv(TAR_PROJECT = "covid_hosp_prod")
archive <- targets::tar_read(nhsn_archive_data)
forecast_date <- archive$versions_end

cat("versions_end:", format(archive$versions_end), "\n")
cat("max time_value in DT:", format(max(archive$DT$time_value)), "\n\n")

# --- Build the revision-aware design matrix ---
# Apply weekday filter before building the design: only keep versions on the
# same weekday as versions_end (Wednesday), matching what the forecaster does.
forecast_wday <- lubridate::wday(archive$versions_end)
archive_filtered <- archive
archive_filtered$DT <- archive$DT[lubridate::wday(version) == forecast_wday]
cat("Versions after weekday filter:", length(unique(archive_filtered$DT$version)), "\n\n")

design <- archive_to_revision_predictors(
  archive_filtered,
  lags = lags,
  cols = "value",
  ahead = ahead_days,
  target_col = "value"
)

cat("Design nrow:", nrow(design), "\n")
cat("Non-NA value_target:", sum(!is.na(design$value_target)), "\n\n")

# Finalization lag: how many weeks elapsed between the target week and
# versions_end when we "read" the finalized value?
#
# Rows where target_week > versions_end have NA value_target (target is in the
# future and can't be observed yet). These are the most-recent anchors.
design <- design %>%
  mutate(
    target_week       = time_value + ahead_days,
    finalization_days = as.integer(archive$versions_end - target_week),
    finalization_wks  = finalization_days / 7
  )

cat("=== Finalization lag distribution (target_week -> versions_end) ===\n")
print(summary(design$finalization_wks))
cat("\n")

# Tabulate how many training rows fall into staleness buckets
buckets <- design %>%
  mutate(bucket = case_when(
    finalization_wks < 2 ~ "< 2 wks (very raw)",
    finalization_wks < 4 ~ "2-4 wks (raw)",
    finalization_wks < 6 ~ "4-6 wks (lightly revised)",
    finalization_wks < 12 ~ "6-12 wks (mostly settled)",
    TRUE ~ ">= 12 wks (finalized)"
  )) %>%
  count(bucket) %>%
  arrange(factor(bucket, levels = c(
    "< 2 wks (very raw)", "2-4 wks (raw)",
    "4-6 wks (lightly revised)", "6-12 wks (mostly settled)", ">= 12 wks (finalized)"
  )))
print(buckets)
cat("\n")

# Show the most recent (stalest) training rows
cat("=== Most recent training rows (least finalized targets) ===\n")
design %>%
  arrange(desc(target_week)) %>%
  select(geo_value, version, time_value, target_week, finalization_wks, value_lag_0, value_target) %>%
  head(20) %>%
  print()
cat("\n")

# --- Compare to what a non-revision-aware forecaster sees ---
#
# scaled_pop_seasonal gets epix_as_of(forecast_date), then epipredict slides
# a training window. For each training row at time t, the target is the as-of
# value at t + ahead — ALSO read from the same single snapshot. The "implicit
# finalization lag" is identical to the revision-aware case: it is just
# versions_end - (t + ahead). The key difference is that X (the predictor) is
# also the current-snapshot value for all t, not the vintage known at time t.
#
# So both forecasters face the same un-finalized target problem for recent rows.
# The revision-aware forecaster additionally uses historically-correct X values
# (a genuine advantage for older rows), but for recent rows its training
# mismatch is worse: the X correctly uses the vintage from the past, but the Y
# pretends data from 3-4 weeks ago is finalized when it isn't.

snap <- epix_as_of(archive, forecast_date)
cat("=== Non-revision-aware snapshot: recent values ===\n")
cat("Rows available in as-of snapshot:", nrow(snap), "\n")
snap_recent <- snap %>%
  as_tibble() %>%
  arrange(desc(time_value)) %>%
  select(geo_value, time_value, value) %>%
  head(20)
print(snap_recent)
cat("\n")

# For ahead=ahead_days, the non-revision-aware forecaster's most recent
# training target would be at time_value = max(snap$time_value) - ahead_days,
# which was first published roughly at that time_value and has only had
# (forecast_date - (max(snap$time_value))) days to revise since then.
max_tv <- max(snap$time_value)
implicit_final_lag_wks <- as.numeric(forecast_date - max_tv) / 7
cat(sprintf(
  "Non-revision-aware: most recent usable training target is at %s\n",
  format(max_tv)
))
cat(sprintf(
  "Implicit finalization lag for that target: %.1f weeks\n", implicit_final_lag_wks
))
cat("\n")

# --- Staleness-filtered revision-aware design ---
#
# How much training data survives if we require the target to be at least
# N weeks old before treating it as "finalized"?
min_final_wks_to_test <- c(2, 4, 6, 8, 12)
cat("=== Surviving training rows (per geo) under finalization cutoffs ===\n")
n_geos <- n_distinct(design$geo_value)
for (mf in min_final_wks_to_test) {
  n_rows <- design %>%
    filter(!is.na(value_target), finalization_wks >= mf) %>%
    nrow()
  cat(sprintf(
    "  >= %2d wks: %d rows / %d geos (%.0f rows/geo avg)\n",
    mf, n_rows, n_geos, n_rows / n_geos
  ))
}
cat("\n")

cat("Done. The revision-aware forecaster should filter out rows where\n")
cat("finalization_wks < [some threshold], e.g. 6, before fitting.\n\n")

# --- Apply the seasonal window filter (mirrors the forecaster's logic) ---
seasonal_backward_window <- 5 * 7
seasonal_forward_window <- 3 * 7

design_seasoned <- design %>% add_season_info()

# Forecast anchor: most recent time_value in the design (the "now" of this run).
# The forecaster centers the window on the anchor's season_week.
forecast_anchor <- max(design_seasoned$time_value)
forecast_season_week <- design_seasoned %>%
  filter(time_value == forecast_anchor) %>%
  pull(season_week) %>%
  max()

cat(sprintf("Forecast anchor: %s  season_week: %d\n", format(forecast_anchor), forecast_season_week))

window_dates <- design_seasoned %>%
  filter(season_week == forecast_season_week) %>%
  pull(time_value) %>%
  unique() %>%
  purrr::map(~ c(.x - seq_len(seasonal_backward_window), .x + 0:(seasonal_forward_window + ahead_days))) %>%
  unlist() %>%
  as.Date() %>%
  unique()

lag_cols <- grep("_lag_", names(design_seasoned), value = TRUE)
target_col <- "value_target"

train <- design_seasoned %>%
  filter(time_value %in% window_dates) %>%
  drop_na(all_of(c(lag_cols, target_col)))

cat(sprintf(
  "After seasonal window + drop_na: %d rows / %d geos (%.0f rows/geo avg)\n",
  nrow(train), n_distinct(train$geo_value), nrow(train) / n_distinct(train$geo_value)
))

# Same finalization breakdown on the post-filter training set
train_with_lag <- train %>%
  mutate(
    target_week      = time_value + ahead_days,
    finalization_wks = as.numeric(archive$versions_end - target_week) / 7
  )
cat("\nFinalization lag after seasonal filter:\n")
print(summary(train_with_lag$finalization_wks))

buckets_train <- train_with_lag %>%
  mutate(bucket = case_when(
    finalization_wks < 2 ~ "< 2 wks (very raw)",
    finalization_wks < 4 ~ "2-4 wks (raw)",
    finalization_wks < 6 ~ "4-6 wks (lightly revised)",
    finalization_wks < 12 ~ "6-12 wks (mostly settled)",
    TRUE ~ ">= 12 wks (finalized)"
  )) %>%
  count(bucket) %>%
  arrange(factor(bucket, levels = c(
    "< 2 wks (very raw)", "2-4 wks (raw)",
    "4-6 wks (lightly revised)", "6-12 wks (mostly settled)", ">= 12 wks (finalized)"
  )))
print(buckets_train)

# --- Heatmap: revision-aware vs non-revision-aware training matrix ---
#
# For each (geo, time_value) in the seasonal window, compute:
#   first_value  = the earliest-version lag_0 (what the model knew in real time)
#   current_value = the as-of snapshot value (what the non-revision-aware model uses)
#   revision_pct  = (current - first) / (|current| + 1) * 100
#
# The design has multiple version rows per (geo, time_value); the earliest one
# carries the first-ever report. We join the current as-of value from the
# snapshot.
snap_values <- epix_as_of(archive, archive$versions_end) %>%
  as_tibble() %>%
  select(geo_value, time_value, current_value = value)

revision_map <- design_seasoned %>%
  filter(time_value %in% window_dates) %>%
  drop_na(value_lag_0) %>%
  group_by(geo_value, time_value) %>%
  slice_min(version, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(first_value = value_lag_0) %>%
  left_join(snap_values, by = c("geo_value", "time_value")) %>%
  mutate(
    revision_pct = (current_value - first_value) / (abs(current_value) + 1) * 100,
    season_label = if_else(
      time_value >= as.Date("2026-01-01"), "2025-26", "2024-25"
    )
  )

# Order geos by total absolute revision (most revised at top).
geo_order <- revision_map %>%
  group_by(geo_value) %>%
  summarise(total_abs_rev = sum(abs(revision_pct), na.rm = TRUE)) %>%
  arrange(desc(total_abs_rev)) %>%
  pull(geo_value)

p <- revision_map %>%
  mutate(geo_value = factor(geo_value, levels = rev(geo_order))) %>%
  ggplot(aes(x = time_value, y = geo_value, fill = revision_pct)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0,
    limits = c(-100, 100), oob = scales::squish,
    name = "revision %\n(current − first)"
  ) +
  facet_wrap(~season_label, scales = "free_x", nrow = 1) +
  labs(
    title = "Revision-aware vs non-revision-aware training matrix",
    subtitle = sprintf(
      "Seasonal window ±%d/+%d weeks around season week %d | forecast as of %s",
      seasonal_backward_window / 7, seasonal_forward_window / 7,
      forecast_season_week, format(archive$versions_end)
    ),
    x = "anchor week (time_value)", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

out_path <- "scripts/reports/revision_heatmap.png"
ggsave(out_path, p, width = 14, height = 8, dpi = 150)
cat(sprintf("\nHeatmap saved to %s\n", out_path))

# --- Ahead=-7 prediction matrix: lag-7 vs finalized target ---
#
# For ahead=-7, the target week is anchor - 7, which is the same week as lag-7.
# lag-7 = current revision of that week; value_target = what it converges to.
# The heatmap shows how much revision correction the model needs to learn.
ahead_neg <- -7L
archive_neg <- archive_filtered
# Mirror the forecaster: drop the most recent time_value for negative ahead.
all_tv_sorted <- sort(unique(archive_neg$DT$time_value), decreasing = TRUE)
archive_neg$DT <- archive_neg$DT[!(time_value %in% all_tv_sorted[seq_len(abs(ahead_neg) %/% 7L)])]

design_neg <- archive_to_revision_predictors(
  archive_neg,
  lags = lags,
  cols = "value",
  ahead = ahead_neg,
  target_col = "value"
)

design_neg_seasoned <- design_neg %>%
  add_season_info() %>%
  filter(time_value %in% window_dates) %>%
  group_by(geo_value, time_value) %>%
  slice_max(version, n = 1, with_ties = FALSE) %>%
  ungroup()

var_levels <- c("value_target", "value_lag_0", "value_lag_7", "value_lag_14")
var_labels <- c("y (finalized)", "lag-0", "lag-7", "lag-14")

# Aggregate across geos (national sum), then pivot long for the 4-row heatmap.
design_neg_national <- design_neg_seasoned %>%
  group_by(time_value) %>%
  summarise(across(all_of(var_levels), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(all_of(var_levels), names_to = "variable", values_to = "value") %>%
  mutate(
    variable = factor(variable, levels = var_levels, labels = var_labels),
    season_label = if_else(time_value >= as.Date("2026-01-01"), "2025-26", "2024-25")
  )

p_neg <- design_neg_national %>%
  ggplot(aes(x = time_value, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "national\nsum") +
  facet_wrap(~season_label, scales = "free_x", nrow = 1) +
  labs(
    title = "Ahead=-7 prediction matrix: (y, lag-0, lag-7, lag-14) by anchor date",
    subtitle = sprintf("national sum across geos | forecast as of %s", format(archive$versions_end)),
    x = "anchor week (time_value)", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

out_path_neg <- "scripts/reports/revision_neg_ahead_heatmap.png"
ggsave(out_path_neg, p_neg, width = 14, height = 8, dpi = 150)
cat(sprintf("Ahead=-7 correction heatmap saved to %s\n", out_path_neg))

# --- Fit coefficients via the actual forecaster (production parameters) ---
#
# Use return_fit = TRUE to get the parsnip fit object with full whitening,
# finalization cutoff, and seasonal window applied identically to production.
# Production params from scripts/covid_hosp_prod.R revision_aware entry:
#   archive: nhsn_nssp_revision_archive (nhsn merged with nssp, source key dropped)
#   lags:    list(c(0,7,14,21), c(0,7)) for (value, nssp)
#   aheads:  -4L and 3L (days, from component_ahead_days)
revision_archive <- targets::tar_read(nhsn_nssp_revision_archive)

print_forecaster_coefs <- function(fit, label) {
  cat(sprintf("\n=== %s ===\n", label))
  print(round(coef(fit$fit), 3))
}

# g_aheads = -1:3, ahead_multiplier = 7 → aheads in days: -7, 0, 7, 14, 21.
# The ensemble uses aheads -1 and 0 (weeks), i.e. -7 and 0 days.
prod_lags <- list(c(0L, 7L, 14L, 21L), c(0L, 7L))

for (prod_ahead in c(-7L, 0L)) {
  fit_prod <- scaled_pop_seasonal_revision(
    revision_archive,
    outcome = "value",
    extra_sources = "nssp",
    ahead = prod_ahead,
    lags = prod_lags,
    pop_scaling = TRUE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    seasonal_backward_window = 35L,
    seasonal_forward_window = 21L,
    return_fit = TRUE
  )
  if (inherits(fit_prod, "model_fit")) {
    print_forecaster_coefs(fit_prod, sprintf("ahead=%+d days (prod params, with drop)", prod_ahead))
  } else {
    cat(sprintf("\nahead=%+d days: returned null_result (too few training rows)\n", prod_ahead))
  }
}

# --- Output date inspection ---
# Run the forecaster normally (no return_fit) and show what forecast_date and
# target_end_date values actually come out, before and after target_date_shift.
cat("\n--- Output dates from forecaster (before target_date_shift is applied) ---\n")
for (prod_ahead in c(-7L, 0L)) {
  out <- scaled_pop_seasonal_revision(
    revision_archive,
    outcome = "value",
    extra_sources = "nssp",
    ahead = prod_ahead,
    lags = prod_lags,
    pop_scaling = TRUE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    seasonal_backward_window = 35L,
    seasonal_forward_window = 21L
  )
  if (nrow(out) == 0L) {
    cat(sprintf("ahead=%+d: empty output\n", prod_ahead))
    next
  }
  dates <- out %>%
    distinct(forecast_date, target_end_date) %>%
    arrange(target_end_date)
  cat(sprintf("\nahead=%+d days:\n", prod_ahead))
  print(dates)
}

# --- Empirical time to convergence ---
#
# For each (geo, time_value), track the full version history and find the lag
# (in weeks from time_value) at which the value last exceeded a threshold
# deviation from the final value. Everything after that is "settled".
# Rows with final_value near zero are excluded (threshold % of near-zero is noise).
convergence_thresholds <- c(0.01, 0.05, 0.10) # 1%, 5%, 10%
min_final_value <- 10L # skip near-zero final values

arch_dt <- data.table::as.data.table(archive$DT) %>%
  filter(!is.na(value)) %>%
  as_tibble()

# Exclude the very first NHSN release date globally: NHSN launched in late 2024
# and backfilled all historical data in one shot. That initial release is not a
# revision event; treating it as one inflates convergence lags for all pre-2024
# time_values.
first_nhsn_release <- min(arch_dt$version)
cat(sprintf("Excluding initial NHSN release: %s\n", format(first_nhsn_release)))

# Base per-(geo, time_value) version history: lag_weeks and revision_pct per version.
# Compute once; convergence_wks is derived per threshold below.
arch_history <- arch_dt %>%
  filter(version > first_nhsn_release, time_value >= first_nhsn_release) %>%
  group_by(geo_value, time_value) %>%
  mutate(
    final_value  = value[which.max(version)],
    lag_weeks    = as.numeric(version - time_value) / 7,
    revision_pct = abs(value - final_value) / (abs(final_value) + 1e-6)
  ) %>%
  filter(abs(final_value) >= min_final_value) %>%
  summarise(
    final_value = first(final_value),
    n_versions  = n(),
    lag_weeks   = list(lag_weeks),
    revision_pct = list(revision_pct),
    .groups = "drop"
  )

bucket_levels <- c(
  "< 2 wks", "2-4 wks", "4-6 wks", "6-10 wks", "10-20 wks",
  "20-40 wks", "40-80 wks", "80-160 wks", ">= 160 wks"
)
to_bucket <- function(wks) {
  case_when(
    wks <   2 ~ "< 2 wks",
    wks <   4 ~ "2-4 wks",
    wks <   6 ~ "4-6 wks",
    wks <  10 ~ "6-10 wks",
    wks <  20 ~ "10-20 wks",
    wks <  40 ~ "20-40 wks",
    wks <  80 ~ "40-80 wks",
    wks < 160 ~ "80-160 wks",
    TRUE      ~ ">= 160 wks"
  )
}

# Compute convergence_wks for each threshold; stack then pivot wide.
conv_wide <- purrr::map_dfr(convergence_thresholds, function(thr) {
  arch_history %>%
    mutate(convergence_wks = purrr::map2_dbl(lag_weeks, revision_pct, function(lw, rp) {
      unsettled <- lw[rp > thr]
      if (length(unsettled) == 0) 0 else max(unsettled)
    })) %>%
    mutate(bucket = factor(to_bucket(convergence_wks), levels = bucket_levels)) %>%
    count(bucket, .drop = FALSE) %>%
    mutate(
      pct = round(100 * n / sum(n), 1),
      col = paste0(n, " (", pct, "%)"),
      thr = paste0(round(thr * 100), "%")
    ) %>%
    select(bucket, thr, col)
}) %>%
  pivot_wider(names_from = thr, values_from = col)

# Use the 5% threshold as primary for summary stats and heatmap.
convergence <- arch_history %>%
  mutate(convergence_wks = purrr::map2_dbl(lag_weeks, revision_pct, function(lw, rp) {
    unsettled <- lw[rp > 0.05]
    if (length(unsettled) == 0) 0 else max(unsettled)
  })) %>%
  select(-lag_weeks, -revision_pct)

cat("\n=== Empirical convergence: weeks until last revision exceeds threshold ===\n")
cat(sprintf("(final value >= %d; initial NHSN backfill release excluded)\n\n", min_final_value))

# Summary stats for 5% threshold
cat("Summary (5% threshold):\n")
print(summary(convergence$convergence_wks))

cat("\nConvergence lag distribution across thresholds:\n")
conv_wide %>%
  rename_with(~ gsub("threshold_", "", .x)) %>%
  print(n = Inf)

# Heatmap of convergence lag: geo x time_value, color = weeks to settle.
# Restrict to (geo, time_value) pairs with enough version history to be
# meaningful (need at least 8 weeks of follow-up).
conv_plot <- convergence %>%
  filter(as.numeric(archive$versions_end - time_value) / 7 >= 8) %>%
  mutate(
    season_label  = if_else(time_value >= as.Date("2026-01-01"), "2025-26", "2024-25"),
    geo_value     = factor(geo_value, levels = rev(geo_order))
  )

p2 <- conv_plot %>%
  ggplot(aes(x = time_value, y = geo_value, fill = convergence_wks)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "plasma", direction = -1,
    name = "wks to\nsettle"
  ) +
  facet_wrap(~season_label, scales = "free_x", nrow = 1) +
  labs(
    title = "Empirical convergence: weeks until last >5% revision",
    subtitle = sprintf("final value >= %d | forecast as of %s", min_final_value, format(archive$versions_end)),
    x = "week (time_value)", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

out_path2 <- "scripts/reports/revision_convergence_heatmap.png"
ggsave(out_path2, p2, width = 14, height = 8, dpi = 150)
cat(sprintf("Convergence heatmap saved to %s\n", out_path2))
