# E19: per-round learning rate and offsets for REF-op cold, sqrt adaptive
# without the leak, and sqrt constant 0.018 on the clean flu windowed_seasonal
# replay, h1–h3, states only. Learns from final truth (one run per config), so
# each round's internals come from a single tracker. Eta and offsets are
# divided by the base 90% interval width, in each config's working units, so
# configs with and without `scales` compare. Writes a per-round CSV, a monthly
# CSV and a plot to cache/calibration/tracker_internals/. Findings: E19 in
# notes/calibration-ledger.md.
#
# Usage: distrobox enter rocker -- Rscript scripts/calibration/calibration_tracker_internals.R

suppressPackageStartupMessages({
  source(here::here("scripts/calibration/calibration_ws_replay.R"))
  library(ggplot2)
})
out_dir <- file.path(CH_CACHE_DIR, "tracker_internals")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

wi <- ws_inputs("flu")
no_burn_in <- list(burn_in_seasons = character(0), slow_init = NULL)
CONFIGS <- list(
  `REF-op cold` = c(list(ref = "op"), no_burn_in),
  `sqrt adaptive, no leak` = c(list(ref = "paper", transform = "sqrt", lr_args = list(mult = 0.03, floor = 1e-3)), no_burn_in),
  `sqrt constant 0.018` = c(list(ref = "paper", transform = "sqrt", scales = wi$rate_scales, lr = 0.018), no_burn_in)
)
scale_of <- wi$rate_scales %>% select(location, scale)

rows <- purrr::imap(CONFIGS, function(cfg, nm) {
  cli::cli_alert_info("running {nm}")
  cal <- do.call(cal_run, c(list(wi$fc, wi$truth), cfg, list(learn = "final")))
  fc <- cal$forecasts %>%
    filter(location != "US", !is_burn_in, horizon >= 1, !is.na(value_base))
  # Working-unit 90% width: sqrt of (count / scale), scale 1 unless the config sets `scales`.
  s <- if (is.null(cfg$scales)) fc %>% distinct(location) %>% mutate(scale = 1) else scale_of
  fc %>%
    left_join(s, by = "location") %>%
    group_by(location, horizon, reference_date) %>%
    summarize(
      width_count = value_base[level == 0.95] - value_base[level == 0.05],
      width_work = sqrt(value_base[level == 0.95] / scale[1]) - sqrt(value_base[level == 0.05] / scale[1]),
      eta = mean(lr_level),
      fast_mid = fast[level == 0.5],
      slow_mid = slow[level == 0.5],
      offset_mid = offset[level == 0.5],
      offset_abs = mean(abs(offset)),
      truth = truth[1],
      base_mid = value_base[level == 0.5],
      .groups = "drop"
    ) %>%
    mutate(
      config = nm,
      eta = ifelse(eta == 0, NA_real_, eta),
      # Unit-free: step size and offsets relative to the base 90% width.
      eta_rel = eta / width_work,
      fast_rel = fast_mid / width_work,
      slow_rel = slow_mid / width_work,
      offset_rel = offset_mid / width_count,
      offset_abs_rel = offset_abs / width_count
    )
}) %>% bind_rows()

per_round <- rows %>%
  group_by(config, horizon, reference_date) %>%
  summarize(
    eta_rel = median(eta_rel, na.rm = TRUE),
    fast_rel = median(fast_rel, na.rm = TRUE),
    offset_rel = median(offset_rel, na.rm = TRUE),
    offset_abs_rel = median(offset_abs_rel, na.rm = TRUE),
    above_base_median = mean(truth > base_mid, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(per_round, file.path(out_dir, "tracker_internals_per_round.csv"))

month_tbl <- per_round %>%
  mutate(season = season_of(reference_date), month = format(reference_date, "%b")) %>%
  group_by(config, horizon, season, month) %>%
  summarize(across(c(eta_rel, offset_rel, offset_abs_rel, above_base_median), \(x) mean(x, na.rm = TRUE)), first = min(reference_date), .groups = "drop") %>%
  arrange(config, horizon, first)
readr::write_csv(month_tbl, file.path(out_dir, "tracker_internals_by_month.csv"))

long <- per_round %>%
  select(config, horizon, reference_date, `eta / width` = eta_rel, `median-level offset / width` = offset_rel, `mean |offset| / width` = offset_abs_rel) %>%
  tidyr::pivot_longer(-c(config, horizon, reference_date))
p <- ggplot(long, aes(reference_date, value, colour = config)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line() +
  facet_grid(name ~ paste0("h", horizon), scales = "free_y") +
  labs(x = NULL, y = "median over states", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "tracker_internals.png"), p, width = 13, height = 8)
cli::cli_alert_success("wrote {out_dir}")
