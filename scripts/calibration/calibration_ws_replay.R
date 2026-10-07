# Calibrate the clean evaluation replay of `windowed_seasonal` (no data
# substitutions, ROADMAP 1a) for flu and covid, and print base vs calibrated WIS
# and coverage per season and horizon as markdown tables.
#
# Forecasts come straight from the evaluation stores through the harness
# (ch_use() + ch_read_store()); truth and vintages from the same store's NHSN
# archive. Rounds are the hub's CMU-TimeSeries submission rounds, so the round
# axis matches the hub-based experiments in notes/calibration-ledger.md. The E12
# configs use no burn-in season.
#
# With `burn_in`, warm-started configs also run with 2023-24 as a burn-in
# season: those rounds are backfilled weekly from 2023-10-11 to 2024-04-24 on
# the HHS-stitched archive (ch_inputs_with_burn_in(), ROADMAP 1b). Scores are
# reported over all locations, states only and US only.
#
# Usage: distrobox enter rocker -- Rscript scripts/calibration/calibration_ws_replay.R [workers] [burn_in]

source(here::here("scripts/calibration/calibration_harness.R"))

# Command-line arguments apply only when run as a script, not when sourced by a notebook.
args <- if (sys.nframe() == 0L) commandArgs(trailingOnly = TRUE) else character(0)
WORKERS <- if (length(args) >= 1) as.integer(args[[1]]) else 12L
BURN_IN <- length(args) >= 2 && args[[2]] == "burn_in"
BURN_IN_DATES <- c(from = "2023-10-11", through = "2024-04-24")
HUB_DIRS <- c(flu = "../FluSight-forecast-hub", covid = "../covid19-forecast-hub")
LIVE <- c("2024-2025", "2025-2026")

# Seasons are August-July years, as in E10 (covid has no off-season gap).
season_of <- function(d) {
  y <- as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) < 8)
  paste0(y, "-", y + 1)
}

# E11's constant rate (0.1 admissions per 100k per step, rate scale) and the
# covid prod config (REF-op without its warm start), both with no burn-in. With
# `burn_in`, also E05's sqrt single term cold and warm, REF-op with its warm
# start, and E11's constant rate warm-started.
ws_configs <- function(rate_scales, burn_in = BURN_IN) {
  no_burn_in <- list(burn_in_seasons = character(0), slow_init = NULL)
  warm <- list(burn_in_seasons = "2023-2024", slow_init = "burn_in_quantile")
  sqrt_single <- list(ref = "paper", transform = "sqrt", lr_args = list(mult = 0.03, floor = 1e-3))
  cold <- list(
    `E11 constant 0.1` = c(list(ref = "paper", scales = rate_scales, lr = 0.1), no_burn_in),
    `REF-op cold` = c(list(ref = "op"), no_burn_in)
  )
  if (!burn_in) {
    return(cold)
  }
  c(cold, list(
    `E05 single cold` = c(sqrt_single, no_burn_in),
    `E05 warm + single` = c(sqrt_single, warm),
    `REF-op warm` = c(list(ref = "op"), warm),
    `E11 constant 0.1 warm` = c(list(ref = "paper", scales = rate_scales, lr = 0.1), warm)
  ))
}

# 2023-24 burn-in rounds, backfilled on the HHS-stitched archive (cached).
ws_burn_in_forecasts <- function(disease) {
  sched <- ch_schedule(through = BURN_IN_DATES[["through"]], from = as.Date(BURN_IN_DATES[["from"]]))
  ch_to_hub(ch_backfill(ch_inputs_with_burn_in(), schedule = sched), disease)
}

#' The replay's hub-round forecasts, truth and vintages, and per-100k rate
#' scales for one disease. Also used by the E14/E15 notebooks.
ws_inputs <- function(disease) {
  ch_use(paste0(disease, "_windowed_seasonal"))
  inp <- ch_inputs()
  tv <- ch_hub_truth(inp)
  hub_rounds <- suppressMessages(hub_read_forecasts(
    hub_dir = here::here(HUB_DIRS[[disease]]),
    target = c(flu = HUB_FLU_TARGET, covid = HUB_COVID_TARGET)[[disease]]
  )) %>% distinct(reference_date) %>% pull()
  fc <- ch_to_hub(ch_read_store(), disease) %>% filter(reference_date %in% hub_rounds)
  cli::cli_alert_info("{disease}: {n_distinct(fc$reference_date)} rounds, {n_distinct(fc$location)} locations")
  rate_scales <- get_population_data() %>%
    distinct(state_code, .keep_all = TRUE) %>%
    filter(state_code %in% unique(fc$location)) %>%
    transmute(location = state_code, from = as.Date("2000-01-01"), scale = population / 1e5)
  list(fc = fc, truth = tv$truth, vintages = tv$vintages, rate_scales = rate_scales)
}

ws_run_disease <- function(disease) {
  wi <- ws_inputs(disease)
  fc <- wi$fc
  tv <- wi[c("truth", "vintages")]
  # Every backfilled 2023-24 round is kept, not only hub rounds (covid has none).
  fc_burn_in <- if (BURN_IN) bind_rows(ws_burn_in_forecasts(disease), fc)
  cals <- purrr::map(ws_configs(wi$rate_scales), function(cfg) {
    t0 <- Sys.time()
    fc_cfg <- if (length(cfg$burn_in_seasons) > 0) fc_burn_in else fc
    cal <- do.call(cal_run_cached, c(list(fc_cfg, tv$truth), cfg, list(learn = "exact", vintages = tv$vintages, workers = WORKERS)))
    cli::cli_alert_success("run took {format(round(Sys.time() - t0, 1))}")
    cal$forecasts %>% mutate(season = season_of(reference_date))
  })
  list(cals = cals, truth = tv$truth)
}

#' Base vs calibrated WIS (2 x mean pinball), 50%/90% interval coverage and L1
#' coverage bias, per (season, horizon); season "both" pools the live seasons.
ws_scores <- function(fc) {
  pinball <- function(y, q, tau) ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))
  fc <- fc %>% filter(!is.na(truth), !is.na(value_base), season %in% LIVE)
  fc <- bind_rows(fc, fc %>% mutate(season = "both"))
  long <- fc %>%
    tidyr::pivot_longer(c(value_base, value_cal), names_to = "which", values_to = "q") %>%
    mutate(which = sub("value_", "", which))
  wis <- long %>%
    group_by(season, horizon, which) %>%
    summarize(wis = 2 * mean(pinball(truth, q, level)), .groups = "drop")
  cov_int <- long %>%
    filter(level %in% c(0.05, 0.25, 0.75, 0.95)) %>%
    select(season, horizon, which, location, reference_date, truth, level, q) %>%
    tidyr::pivot_wider(names_from = level, values_from = q, names_prefix = "q") %>%
    group_by(season, horizon, which) %>%
    summarize(
      cov50 = mean(truth >= q0.25 & truth <= q0.75),
      cov90 = mean(truth >= q0.05 & truth <= q0.95),
      .groups = "drop"
    )
  bias <- long %>%
    group_by(season, horizon, which, level) %>%
    summarize(gap = mean(truth <= q) - first(level), .groups = "drop") %>%
    group_by(season, horizon, which) %>%
    summarize(cal_err = mean(abs(gap)), .groups = "drop")
  above <- fc %>%
    filter(level == 0.5) %>%
    group_by(season, horizon) %>%
    summarize(above_median = mean(truth > value_base), .groups = "drop")
  wis %>%
    left_join(cov_int, by = c("season", "horizon", "which")) %>%
    left_join(bias, by = c("season", "horizon", "which")) %>%
    left_join(above, by = c("season", "horizon"))
}

#' V-month per season: WIS reduction % (positive = calibrated WIS lower than base), L1 coverage bias and
#' the share of truth below the median, base vs calibrated, by calendar month of
#' the reference date. `share` is the month's share of the season's base WIS.
#' Groups by `config` too when present. Used by the E16/E17 notebooks.
ws_month_scores <- function(fc) {
  pinball <- function(y, q, tau) ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))
  by <- intersect(c("config", "season", "horizon"), names(fc))
  fc <- fc %>%
    filter(!is.na(truth), !is.na(value_base), season %in% LIVE) %>%
    mutate(month = factor(format(reference_date, "%b"), levels = month.abb[c(8:12, 1:7)]))
  per_level <- fc %>%
    group_by(across(all_of(c(by, "month", "level")))) %>%
    summarize(
      base = sum(pinball(truth, value_base, level)), cal = sum(pinball(truth, value_cal, level)),
      gap_base = mean(truth <= value_base) - first(level), gap_cal = mean(truth <= value_cal) - first(level),
      below_base = mean(truth < value_base), below_cal = mean(truth < value_cal),
      n = n(), .groups = "drop"
    )
  per_level %>%
    group_by(across(all_of(c(by, "month")))) %>%
    summarize(
      wis_base = sum(base), wis_cal = sum(cal),
      cal_err_base = mean(abs(gap_base)), cal_err_cal = mean(abs(gap_cal)),
      below_med_base = below_base[level == 0.5], below_med_cal = below_cal[level == 0.5],
      n = first(n), .groups = "drop"
    ) %>%
    group_by(across(all_of(by))) %>%
    mutate(pct = 100 * (wis_base - wis_cal) / wis_base, share = 100 * wis_base / sum(wis_base)) %>%
    ungroup()
}

# The finalists' inputs for one disease: the submitted ensemble and the replay
# on the (round, location, horizon, level) rows both have, h0–h3. Cold runs see
# the live seasons only; warm runs also see the 2023-24 burn-in season (covid
# submissions start 2024-11-23, so covid has warm runs on the replay only).
fin_inputs <- function(disease) {
  key <- c("reference_date", "location", "horizon", "level")
  wi <- ws_inputs(disease)
  # Right after ws_inputs(): both read the harness globals that ch_use() sets.
  burn <- ws_burn_in_forecasts(disease)
  hub <- suppressMessages(hub_read_forecasts(
    hub_dir = here::here(HUB_DIRS[[disease]]), target = c(flu = HUB_FLU_TARGET, covid = HUB_COVID_TARGET)[[disease]]
  )) %>% filter(horizon %in% 0:3)
  ws <- bind_rows(burn, wi$fc)
  common <- inner_join(distinct(hub, across(all_of(key))), distinct(ws, across(all_of(key))), by = key)
  fcs <- list(ensemble = semi_join(hub, common, by = key), replay = semi_join(ws, common, by = key))
  warm <- fcs
  if (!any(season_of(fcs$ensemble$reference_date) == "2023-2024")) {
    warm <- list(replay = bind_rows(burn, fcs$replay))
  }
  list(wi = wi, cold = purrr::map(fcs, \(fc) filter(fc, season_of(reference_date) %in% LIVE)), warm = warm)
}

# The finalist configs: REF-op and sqrt constant 0.018 per 100k (E16), each cold and warm.
fin_candidates <- function(rate_scales) {
  const <- ws_configs(rate_scales, burn_in = FALSE)[["E11 constant 0.1"]]
  sqrt_018 <- utils::modifyList(const, list(lr = 0.018, transform = "sqrt"))
  list(
    `REF-op warm` = list(cfg = list(ref = "op"), warm = TRUE),
    `REF-op cold` = list(cfg = list(ref = "op", burn_in_seasons = character(0), slow_init = NULL), warm = FALSE),
    `sqrt 0.018` = list(cfg = sqrt_018, warm = FALSE),
    `sqrt 0.018 warm` = list(cfg = utils::modifyList(sqrt_018, list(burn_in_seasons = "2023-2024", slow_init = "burn_in_quantile")), warm = TRUE)
  )
}

# Every finalist on each of `bases` that has its inputs, cached. Returns the
# inputs, the runs named "<base>, <candidate>", their grid and the candidate names.
fin_runs <- function(disease, bases = c("ensemble", "replay"), workers = WORKERS) {
  inp <- fin_inputs(disease)
  cands <- fin_candidates(inp$wi$rate_scales)
  grid <- tidyr::expand_grid(base = bases, cand = names(cands)) %>%
    filter(!purrr::map_lgl(cands[cand], "warm") | base %in% names(inp$warm))
  cals <- purrr::pmap(grid, function(base, cand) {
    cc <- cands[[cand]]
    fc <- if (cc$warm) inp$warm[[base]] else inp$cold[[base]]
    do.call(cal_run_cached, c(list(fc, inp$wi$truth), cc$cfg, list(learn = "exact", vintages = inp$wi$vintages, workers = workers)))
  })
  list(inputs = inp, cals = setNames(cals, paste(grid$base, grid$cand, sep = ", ")), grid = grid, cands = names(cands))
}

# Location subsets scored separately: raw-count pooling makes US about half of
# the all-locations WIS.
WS_GEOS <- list(all = \(loc) rep(TRUE, length(loc)), states = \(loc) loc != "US", us = \(loc) loc == "US")

ws_report <- function(disease, res) {
  scores <- purrr::imap(WS_GEOS, function(keep, geos) {
    purrr::imap(res$cals, function(fc, nm) ws_scores(fc %>% filter(keep(location))) %>% mutate(config = nm)) %>%
      bind_rows() %>%
      mutate(geos = geos)
  }) %>% bind_rows()
  base <- scores %>% filter(which == "base", config == names(res$cals)[1]) %>% mutate(config = "base")
  tbl <- bind_rows(base, scores %>% filter(which == "cal")) %>%
    group_by(geos, season, horizon) %>%
    mutate(wis_pct = 100 * (wis[config == "base"] - wis) / wis[config == "base"]) %>%
    ungroup() %>%
    mutate(
      config = factor(config, levels = c("base", names(res$cals))), season = factor(season, levels = c(LIVE, "both")),
      geos = factor(geos, levels = names(WS_GEOS))
    )
  suffix <- if (BURN_IN) "_burn_in" else ""
  readr::write_csv(tbl, file.path(CH_CACHE_DIR, glue::glue("ws_replay_scores{suffix}_{disease}.csv")))
  wide <- function(col, digits, g) {
    tbl %>%
      filter(geos == g) %>%
      transmute(season, config, horizon, v = round(.data[[col]], digits)) %>%
      tidyr::pivot_wider(names_from = horizon, values_from = v, names_prefix = "h") %>%
      arrange(season, config)
  }
  for (g in names(WS_GEOS)) {
    cat("\n\n## ", disease, ", ", g, " locations\n", sep = "")
    for (spec in list(
      c("wis", "WIS (2 x mean pinball)", 1), c("wis_pct", "WIS reduction % (positive = calibrated WIS lower than base)", 1),
      c("cov50", "50% interval coverage", 3), c("cov90", "90% interval coverage", 3),
      c("cal_err", "L1 coverage bias (lower is better)", 3)
    )) {
      cat("\n### ", spec[2], "\n\n", sep = "")
      print(knitr::kable(wide(spec[1], as.integer(spec[3]), g)))
    }
    cat("\n### Fraction of truth above the base median\n\n")
    print(knitr::kable(tbl %>% filter(config == "base", geos == g) %>% transmute(season, horizon, p = round(above_median, 2)) %>%
      tidyr::pivot_wider(names_from = horizon, values_from = p, names_prefix = "h")))
  }
  invisible(tbl)
}

if (sys.nframe() == 0L) {
  for (disease in c("flu", "covid")) ws_report(disease, ws_run_disease(disease))
}
