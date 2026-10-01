# Calibrate the clean evaluation replay of `windowed_seasonal` (no data
# substitutions, ROADMAP 1a) for flu and covid, and print base vs calibrated WIS
# and coverage per season and horizon as markdown tables.
#
# Forecasts come straight from the evaluation stores through the harness
# (ch_use() + ch_read_store()); truth and vintages from the same store's NHSN
# archive. Rounds are the hub's CMU-TimeSeries submission rounds, so the round
# axis matches the hub-based experiments in notes/CALIBRATION.md. No config
# here uses a burn-in season (none exists before 2024-11-20 in the replay).
#
# Usage: distrobox enter rocker -- Rscript scripts/calibration/calibration_ws_replay.R [workers]

source(here::here("scripts/calibration/calibration_harness.R"))

args <- commandArgs(trailingOnly = TRUE)
WORKERS <- if (length(args) >= 1) as.integer(args[[1]]) else 12L
HUB_DIRS <- c(flu = "../FluSight-forecast-hub", covid = "../covid19-forecast-hub")
LIVE <- c("2024-2025", "2025-2026")

# Seasons are August-July years, as in E10 (covid has no off-season gap).
season_of <- function(d) {
  y <- as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) < 8)
  paste0(y, "-", y + 1)
}

# E11's constant rate (0.1 admissions per 100k per step, rate scale) and the
# covid prod config (REF-op without its warm start), both with no burn-in.
ws_configs <- function(rate_scales) {
  no_burn_in <- list(burn_in_seasons = character(0), slow_init = NULL)
  list(
    `E11 constant 0.1` = c(list(ref = "paper", scales = rate_scales, lr = 0.1), no_burn_in),
    `REF-op cold` = c(list(ref = "op"), no_burn_in)
  )
}

ws_run_disease <- function(disease) {
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
  cals <- purrr::map(ws_configs(rate_scales), function(cfg) {
    t0 <- Sys.time()
    cal <- do.call(cal_run_cached, c(list(fc, tv$truth), cfg, list(learn = "exact", vintages = tv$vintages, workers = WORKERS)))
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

ws_report <- function(disease, res) {
  scores <- purrr::imap(res$cals, function(fc, nm) ws_scores(fc) %>% mutate(config = nm)) %>% bind_rows()
  base <- scores %>% filter(which == "base", config == names(res$cals)[1]) %>% mutate(config = "base")
  tbl <- bind_rows(base, scores %>% filter(which == "cal")) %>%
    group_by(season, horizon) %>%
    mutate(wis_pct = 100 * (wis[config == "base"] - wis) / wis[config == "base"]) %>%
    ungroup() %>%
    mutate(config = factor(config, levels = c("base", names(res$cals))), season = factor(season, levels = c(LIVE, "both")))
  readr::write_csv(tbl, file.path(CH_CACHE_DIR, glue::glue("ws_replay_scores_{disease}.csv")))
  wide <- function(col, digits) {
    tbl %>%
      transmute(season, config, horizon, v = round(.data[[col]], digits)) %>%
      tidyr::pivot_wider(names_from = horizon, values_from = v, names_prefix = "h") %>%
      arrange(season, config)
  }
  cat("\n\n## ", disease, "\n", sep = "")
  for (spec in list(
    c("wis", "WIS (2 x mean pinball)", 1), c("wis_pct", "WIS change % vs base (positive is better)", 1),
    c("cov50", "50% interval coverage", 3), c("cov90", "90% interval coverage", 3),
    c("cal_err", "L1 coverage bias", 3)
  )) {
    cat("\n### ", spec[2], "\n\n", sep = "")
    print(knitr::kable(wide(spec[1], as.integer(spec[3]))))
  }
  cat("\n### Fraction of truth above the base median\n\n")
  print(knitr::kable(tbl %>% filter(config == "base") %>% transmute(season, horizon, p = round(above_median, 2)) %>%
    tidyr::pivot_wider(names_from = horizon, values_from = p, names_prefix = "h")))
  invisible(tbl)
}

if (sys.nframe() == 0L) {
  for (disease in c("flu", "covid")) ws_report(disease, ws_run_disease(disease))
}
