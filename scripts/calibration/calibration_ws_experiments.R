# One forecaster across eras: does online calibration of `windowed_seasonal`
# behave the same on the ILI+ decade (2011-2023, percent scale, no revisions)
# as on the NHSN seasons (2023-2026, counts, real vintages), and how does that
# compare with calibrating the submitted ensemble? Prints markdown tables;
# results are recorded in notes/CALIBRATION.md.
#
# Inputs (all cached in cache/calibration/):
#   scripts/calibration_ili_backfill.R  -> windowed_seasonal on ILI+, 2010-2024
#   scripts/calibration_ws_backfill.R   -> windowed_seasonal on NHSN, 2023-2026
#   hub_read_forecasts()/hub_read_truth() -> the submitted ensemble and hub truth
#
# Usage: distrobox enter rocker -- Rscript scripts/calibration_ws_experiments.R

suppressPackageStartupMessages(source(here::here("R/load_all.R")))
source(here::here("scripts/calibration_ili_backfill.R"))
source(here::here("scripts/calibration_ws_backfill.R"))

ili <- ili_read_pseudo_hub()
ws <- ws_read_pseudo_hub()
hub_fc <- hub_read_forecasts()
hub_truth <- {
  arch <- get_nhsn_data_archive("flu")
  arch %>%
    epix_as_of(arch$versions_end) %>%
    mutate(geo_value = ifelse(geo_value == "usa", "us", geo_value)) %>%
    left_join(
      get_population_data() %>% select("state_id", location = "state_code"),
      by = c("geo_value" = "state_id")
    ) %>%
    select(target_end_date = time_value, location, truth = value) %>%
    filter(!is.na(truth)) %>%
    arrange(location, target_end_date)
}
LIVE_SEASONS <- c("2024-2025", "2025-2026")
ERA_SPLIT <- as.Date("2023-08-01") # ILI+ history before, NHSN after

# ILI+ seasons kept: drop 2010-11 (too little training data for h3), the two
# pandemic non-seasons, and 2023-24 (the NHSN era owns those rounds).
ili_rounds <- hub_label_seasons(unique(ili$forecasts$reference_date))
ili_keep <- ili_rounds %>% filter(!season %in% c("2010-2011", "2020-2021", "2021-2022", "2023-2024"))
ili_fc <- ili$forecasts %>% filter(reference_date %in% ili_keep$round_date)

# Locations present in every source, so eras are comparable per location.
common_loc <- Reduce(intersect, list(unique(ili_fc$location), unique(ws$location), unique(hub_fc$location)))
ili_fc <- ili_fc %>% filter(location %in% common_loc)
# The replay ran every Wednesday, summer included; keep only the hub's
# submission rounds so seasons, gaps and round counts match the ensemble's.
ws_fc <- ws %>%
  filter(location %in% common_loc, reference_date %in% unique(hub_fc$reference_date)) %>%
  select(-as_of_policy)
hub_fc <- hub_fc %>% filter(location %in% common_loc)

combined_fc <- bind_rows(ili_fc, ws_fc)
combined_truth <- bind_rows(
  ili$truth %>% filter(target_end_date < ERA_SPLIT),
  hub_truth %>% filter(target_end_date >= ERA_SPLIT)
) %>% filter(location %in% common_loc)

# Whitening across eras: divide each location's rounds by the 90th percentile of
# its in-season (Oct-May) truth for that era, so offsets and eta live in
# "fraction of a typical season peak" units in both eras.
in_season <- function(d) as.integer(format(d, "%m")) %in% c(10:12, 1:5)
era_scale <- function(truth, from, to) {
  truth %>%
    filter(target_end_date >= from, target_end_date < to, in_season(target_end_date)) %>%
    group_by(location) %>%
    summarize(scale = stats::quantile(truth, 0.9, names = FALSE), .groups = "drop")
}
scales <- bind_rows(
  era_scale(ili$truth, as.Date("2011-08-01"), ERA_SPLIT) %>% mutate(from = as.Date("2000-01-01")),
  era_scale(hub_truth, as.Date("2023-10-01"), as.Date("2024-06-01")) %>% mutate(from = ERA_SPLIT)
) %>% filter(location %in% common_loc, scale > 0)

variants <- function(floor) list(
  single = list(),
  leaky = list(fast_decay = 0.1),
  operating = list(slow_init = "burn_in_quantile", lr_slow = list(mult = 0.003), fast_decay = 0.1)
)
run_all <- function(fc, truth, burn_in, floor, ...) {
  purrr::map(variants(floor), function(v) {
    do.call(calibrate_hub_forecasts, c(
      list(fc, truth, burn_in_seasons = burn_in, transform = "sqrt", lr_window = 20,
        lr_args = list(mult = 0.03, floor = floor), progress = FALSE),
      v, list(...)
    ))
  })
}

cli::cli_h1("A. submitted ensemble, NHSN only, burn-in 2023-24")
A <- run_all(hub_fc, hub_truth, "2023-2024", 1e-3)
cli::cli_h1("B. windowed_seasonal, NHSN only, burn-in 2023-24 (finalized-data replay)")
B <- run_all(ws_fc, hub_truth, "2023-2024", 1e-3)
cli::cli_h1("C. windowed_seasonal, ILI+ decade then NHSN, burn-in 2011-12, whitened")
C <- run_all(combined_fc, combined_truth, "2011-2012", 1e-4, scales = scales)

season_table <- function(cals, seasons = LIVE_SEASONS) {
  purrr::imap(cals, function(cal, nm) {
    left_join(
      hub_quantile_loss(cal, by = "season") %>% select(season, horizon, wis_pct = pct_improvement),
      hub_coverage_summary(cal, by = "season") %>% select(season, horizon, cal_err_base = cal_error_base, cal_err_cal = cal_error_cal),
      by = c("season", "horizon")
    ) %>% mutate(variant = nm)
  }) %>%
    bind_rows() %>%
    filter(season %in% seasons)
}
wide <- function(tbl, col, digits = 1) {
  tbl %>% select(variant, season, horizon, value = all_of(col)) %>%
    mutate(value = round(value, digits)) %>%
    tidyr::pivot_wider(names_from = horizon, values_from = value, names_prefix = "h") %>%
    arrange(season, variant)
}
report <- function(label, cals) {
  tbl <- season_table(cals)
  cat("\n\n### ", label, "\n\nWIS change % vs base (positive is better)\n\n", sep = "")
  print(knitr::kable(wide(tbl, "wis_pct")))
  cat("\nCalibration error, calibrated (base in first row)\n\n")
  base <- tbl %>% filter(variant == "single") %>% transmute(variant = "base", season, horizon, cal_err_cal = cal_err_base)
  print(knitr::kable(wide(bind_rows(base, tbl), "cal_err_cal", 3)))
}
report("A. ensemble, NHSN only", A)
report("B. windowed_seasonal, NHSN only", B)
report("C. windowed_seasonal, ILI+ decade + NHSN, whitened", C)
cat("\n\n### C, the 2023-24 NHSN season (tracked live here; burn-in in A and B)\n\n")
print(knitr::kable(wide(season_table(C, seasons = "2023-2024"), "wis_pct")))

cat("\n\n### Base bias: fraction of truth above the base median, live seasons\n\n")
bias <- bind_rows(
  A$single$forecasts %>% mutate(base = "ensemble"),
  B$single$forecasts %>% mutate(base = "windowed_seasonal")
) %>%
  filter(level == 0.5, !is.na(truth), !is.na(value_base), season %in% c("2023-2024", LIVE_SEASONS)) %>%
  group_by(base, season, horizon) %>%
  summarize(p = round(mean(truth > value_base), 2), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = horizon, values_from = p, names_prefix = "h")
print(knitr::kable(bias))

cat("\n\n### C, ILI+ seasons: WIS change % (single / operating)\n\n")
print(knitr::kable(wide(season_table(C, seasons = unique(ili_keep$season)) %>% filter(variant != "leaky"), "wis_pct")))

cat("\n\n### Base WIS per season and horizon (mean pinball x 2), ensemble vs windowed_seasonal\n\n")
bw <- bind_rows(
  hub_quantile_loss(A$single, by = "season") %>% mutate(base = "ensemble"),
  hub_quantile_loss(B$single, by = "season") %>% mutate(base = "windowed_seasonal")
) %>% filter(season %in% LIVE_SEASONS) %>%
  transmute(base, season, horizon, wis = round(2 * loss_base, 1)) %>%
  tidyr::pivot_wider(names_from = horizon, values_from = wis, names_prefix = "h")
print(knitr::kable(bw))
