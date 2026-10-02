# Named reference configs and standard views for the calibration experiment
# notebooks. The experiments, their reference configs and the view ids (V-head,
# V-month, ...) are indexed in notes/calibration-ledger.md.


#' Arguments to [calibrate_hub_forecasts()] for a named reference config.
#'
#' `"paper"` is REF-paper (count space, single term); `"op"` is REF-op, the
#' operating point in prod. Experiments vary one axis from one of these via
#' `...` in [cal_run()].
#' @export
cal_ref_args <- function(ref = c("paper", "op")) {
  ref <- rlang::arg_match(ref)
  paper <- list(
    burn_in_seasons = "2023-2024", settle_days = 14L, transform = "identity",
    lr = "adaptive+", lr_window = 20, lr_args = list(mult = 0.03),
    season_policy = "carry"
  )
  switch(ref,
    paper = paper,
    op = utils::modifyList(paper, list(
      transform = "sqrt", lr_args = list(mult = 0.03, floor = 1e-3),
      slow_init = "burn_in_quantile", lr_slow = list(mult = 0.003), fast_decay = 0.1
    ))
  )
}


#' Run a reference config with some arguments overridden.
#'
#' @param learn `"final"`, `"vintage"` (value as of the reveal round) or
#'   `"exact"` (re-run per round on that round's vintage, as prod does). The
#'   last two need `vintages`; `workers` parallelizes `"exact"`.
#' @param ... overrides of [cal_ref_args()]. `lr_args` replaces the reference's
#'   `lr_args` whole rather than merging into it.
#' @export
cal_run <- function(forecasts, truth, ref = "paper", ..., learn = c("final", "vintage", "exact"), vintages = NULL, workers = 1L) {
  learn <- rlang::arg_match(learn)
  args <- cal_ref_args(ref)
  overrides <- list(...)
  args[names(overrides)] <- overrides
  if (learn != "final" && is.null(vintages)) {
    cli::cli_abort("{.arg learn} = {.val {learn}} needs {.arg vintages}.")
  }
  suppressMessages(switch(learn,
    final = do.call(calibrate_hub_forecasts, c(list(forecasts, truth, progress = FALSE), args)),
    vintage = do.call(calibrate_hub_forecasts, c(list(forecasts, truth, learn_truth = vintages, progress = FALSE), args)),
    exact = do.call(calibrate_hub_forecasts_exact, c(list(forecasts, truth, vintages), args, workers = workers))
  ))
}


#' V-head: WIS reduction % and L1 coverage bias by horizon, one row per
#' (variant, `by`, horizon).
#'
#' @param cals named list of [calibrate_hub_forecasts()] outputs.
#' @param from only score rounds with reference date on or after this date.
#' @export
cal_headline <- function(cals, by = character(0), from = NULL) {
  purrr::imap(cals, function(cal, nm) {
    fc <- cal$forecasts
    if (!is.null(from)) fc <- fc %>% filter(.data$reference_date >= from)
    left_join(
      hub_quantile_loss(fc, by = by),
      hub_coverage_summary(fc, by = by) %>%
        select(all_of(c(by, "horizon")), "cal_error_base", "cal_error_cal"),
      by = c(by, "horizon")
    ) %>% mutate(variant = nm, .before = 1)
  }) %>%
    bind_rows() %>%
    mutate(variant = factor(.data$variant, levels = names(cals)))
}


#' One column of a [cal_headline()] table, horizons across.
#' @export
cal_wide <- function(tbl, col, digits = 1, id = "variant") {
  tbl %>%
    select(all_of(id), "horizon", value = all_of(col)) %>%
    mutate(value = round(.data$value, digits)) %>%
    tidyr::pivot_wider(names_from = "horizon", values_from = "value", names_prefix = "h")
}


#' V-month: WIS reduction % by month of reference date, with each month's share
#' of the base WIS, per horizon.
#' @export
cal_by_month <- function(cal) {
  pinball <- function(y, q, tau) ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))
  cal$forecasts %>%
    filter(!.data$is_burn_in, !is.na(.data$truth), !is.na(.data$value_base)) %>%
    mutate(month = factor(format(.data$reference_date, "%b"), levels = month.abb[c(7:12, 1:6)])) %>%
    group_by(.data$horizon, .data$month) %>%
    summarize(
      base = sum(pinball(.data$truth, .data$value_base, .data$level)),
      cal = sum(pinball(.data$truth, .data$value_cal, .data$level)),
      .groups = "drop"
    ) %>%
    group_by(.data$horizon) %>%
    mutate(pct = 100 * (.data$base - .data$cal) / .data$base, share = 100 * .data$base / sum(.data$base)) %>%
    ungroup()
}


#' [cal_run()], cached on disk under a hash of its inputs.
#'
#' Keeps only the forecast columns the views use. Clear `cache_dir` after
#' changing code in `R/calibration/`: the key covers the arguments and data,
#' not the code.
#' @export
cal_run_cached <- function(forecasts, truth, ref = "paper", ..., learn = "exact", vintages = NULL, workers = 1L,
                           cache_dir = here::here("cache/calibration/experiments/runs")) {
  key <- rlang::hash(list(forecasts, truth, ref, list(...), learn, if (learn != "final") vintages))
  path <- file.path(cache_dir, paste0(key, ".rds"))
  if (file.exists(path)) {
    return(readRDS(path))
  }
  cal <- cal_run(forecasts, truth, ref, ..., learn = learn, vintages = vintages, workers = workers)
  cal <- list(forecasts = cal$forecasts %>% select(
    "location", "horizon", "reference_date", "target_end_date", "season", "level",
    "value_base", "value_cal", "truth", "is_burn_in"
  ))
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(cal, path)
  cal
}


#' V-ae: summed absolute error of the median, change % vs base (positive is
#' better), per (variant, `by`, horizon).
#' @export
cal_ae <- function(cals, by = character(0)) {
  purrr::imap(cals, function(cal, nm) {
    cal$forecasts %>%
      filter(!.data$is_burn_in, .data$level == 0.5, !is.na(.data$truth), !is.na(.data$value_base)) %>%
      group_by(across(all_of(c(by, "horizon")))) %>%
      summarize(
        ae_base = sum(abs(.data$truth - .data$value_base)),
        ae_cal = sum(abs(.data$truth - .data$value_cal)),
        .groups = "drop"
      ) %>%
      mutate(variant = nm, .before = 1, ae_pct = 100 * (.data$ae_base - .data$ae_cal) / .data$ae_base)
  }) %>%
    bind_rows() %>%
    mutate(variant = factor(.data$variant, levels = names(cals)))
}


#' The fixed V-state locations, so state panels are comparable across
#' experiments: national, then states from large to very small.
#' @export
CAL_STATES <- c(US = "US", CA = "06", GA = "13", MN = "27", VT = "50")

CAL_FAN_COLS <- c(base = "#0072B2", calibrated = "#D55E00")


#' Each round's NHSN snapshot for one location, as the forecaster saw it
#' (published by `reference_date - 3`), limited to `weeks` before that.
#' @keywords internal
cal_round_snapshots <- function(vintages, loc, ref_dates, weeks = 12L) {
  v <- vintages %>% filter(.data$location == loc)
  purrr::map(ref_dates, function(rd) {
    hub_vintage_snapshot(v, hub_round_asof(rd)) %>%
      filter(.data$target_end_date >= rd - 7L * weeks) %>%
      mutate(reference_date = rd)
  }) %>% bind_rows()
}


#' V-state: one location over one season. Every other round's fan (h−1–h3,
#' base 80% band and median, calibrated 10/50/90%), finalized truth in grey and,
#' in black, the last two reported weeks of the snapshot each plotted round saw,
#' with a dot on the latest. That is normally the fan's h−1 week, so each black
#' tail leads into its own fan; after a late release it ends a week earlier.
#' @export
cal_state_panel <- function(cal, loc, season, truth, vintages, title = NULL) {
  refs <- cal$forecasts %>%
    filter(.data$season == !!season, .data$location == loc, !is.na(.data$value_base)) %>%
    distinct(.data$reference_date) %>%
    arrange(.data$reference_date) %>%
    pull()
  refs <- refs[seq(1L, length(refs), by = 2L)]
  fan <- cal$forecasts %>%
    filter(.data$location == loc, .data$reference_date %in% refs, .data$horizon %in% -1:3, .data$level %in% c(0.1, 0.5, 0.9)) %>%
    mutate(lvl = c(`0.1` = "lo", `0.5` = "med", `0.9` = "hi")[as.character(.data$level)]) %>%
    select("reference_date", "target_end_date", "lvl", base = "value_base", calibrated = "value_cal") %>%
    tidyr::pivot_longer(c("base", "calibrated"), names_to = "which") %>%
    tidyr::pivot_wider(names_from = "lvl", values_from = "value")
  fin <- truth %>%
    filter(.data$location == loc, .data$target_end_date >= min(refs) - 21L, .data$target_end_date <= max(refs) + 28L)
  snap <- cal_round_snapshots(vintages, loc, refs, weeks = 4L) %>%
    group_by(.data$reference_date) %>%
    slice_max(.data$target_end_date, n = 2L) %>%
    ungroup()
  snap_end <- snap %>% group_by(.data$reference_date) %>% slice_max(.data$target_end_date, n = 1L) %>% ungroup()
  ggplot2::ggplot() +
    ggplot2::geom_line(data = fin, ggplot2::aes(.data$target_end_date, .data$truth), colour = "grey65") +
    ggplot2::geom_ribbon(
      data = filter(fan, .data$which == "base"),
      ggplot2::aes(.data$target_end_date, ymin = .data$lo, ymax = .data$hi, fill = .data$which, group = .data$reference_date),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = filter(fan, .data$which == "calibrated") %>% tidyr::pivot_longer(c("lo", "hi"), names_to = "edge", values_to = "q"),
      ggplot2::aes(.data$target_end_date, .data$q, colour = .data$which, group = interaction(.data$reference_date, .data$edge)),
      linewidth = 0.3
    ) +
    ggplot2::geom_line(
      data = fan,
      ggplot2::aes(.data$target_end_date, .data$med, colour = .data$which, group = interaction(.data$reference_date, .data$which))
    ) +
    ggplot2::geom_line(data = snap, ggplot2::aes(.data$target_end_date, .data$truth, group = .data$reference_date), colour = "black") +
    ggplot2::geom_point(data = snap_end, ggplot2::aes(.data$target_end_date, .data$truth), colour = "black", size = 1) +
    ggplot2::scale_fill_manual(values = CAL_FAN_COLS, aesthetics = c("fill", "colour")) +
    ggplot2::labs(x = NULL, y = "admissions", colour = NULL, fill = NULL, subtitle = title %||% paste(names(CAL_STATES)[CAL_STATES == loc], season))
}


#' V-gallery selection: a few forecasts, the best and the worst per season
#' phase and location size, each from a different location.
#'
#' Ranking by a single metric fills a gallery with small states late in the
#' season, where a stale offset on a tiny base inflates relative error, and
#' ranking by WIS per 100k favours small places too (their rates are noisier).
#' Here each forecast is scored by its WIS reduction as a share of its own
#' location-season's base WIS, so a late-season forecast on a tiny base only
#' ranks high if it moved a real part of that location's season error. Small
#' places still vary more, so slots are split by size (population above or
#' below the median) as well as by phase (before, around, after the location's
#' peak in finalized truth).
#'
#' @param population tibble of `location`, `population`.
#' @param peak_weeks half-width of the "peak" phase.
#' @return one row per picked forecast: `location`, `reference_date`, `phase`,
#'   `size`, `kind` (gain/loss), `share` (% of the location-season's base WIS),
#'   `wis_pct`.
#' @export
cal_gallery_pick <- function(cal, population, peak_weeks = 3L) {
  pinball <- function(y, q, tau) ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))
  fc <- cal$forecasts %>% filter(!.data$is_burn_in, !is.na(.data$truth), !is.na(.data$value_base))
  median_pop <- stats::median(population$population[population$location %in% fc$location])
  peaks <- fc %>%
    distinct(.data$location, .data$season, .data$target_end_date, .data$truth) %>%
    group_by(.data$location, .data$season) %>%
    slice_max(.data$truth, n = 1L, with_ties = FALSE) %>%
    ungroup() %>%
    select("location", "season", peak = "target_end_date")
  scored <- fc %>%
    group_by(.data$location, .data$season, .data$reference_date) %>%
    summarize(
      wis_base = sum(pinball(.data$truth, .data$value_base, .data$level)),
      wis_cal = sum(pinball(.data$truth, .data$value_cal, .data$level)),
      .groups = "drop"
    ) %>%
    group_by(.data$location, .data$season) %>%
    mutate(share = 100 * (.data$wis_base - .data$wis_cal) / sum(.data$wis_base)) %>%
    ungroup() %>%
    inner_join(peaks, by = c("location", "season")) %>%
    inner_join(population, by = "location") %>%
    mutate(
      size = if_else(.data$population >= median_pop, "large", "small"),
      weeks_from_peak = as.numeric(.data$reference_date - .data$peak) / 7,
      phase = factor(
        dplyr::case_when(
          .data$weeks_from_peak < -peak_weeks ~ "before peak",
          .data$weeks_from_peak > peak_weeks ~ "after peak",
          TRUE ~ "around peak"
        ),
        levels = c("before peak", "around peak", "after peak")
      ),
      wis_pct = 100 * (.data$wis_base - .data$wis_cal) / .data$wis_base
    )
  # Greedy, one slot at a time, skipping locations already picked.
  slots <- tidyr::expand_grid(phase = levels(scored$phase), size = c("large", "small"), kind = c("gain", "loss"))
  used <- character(0)
  picks <- purrr::map(seq_len(nrow(slots)), function(i) {
    cand <- scored %>% filter(.data$phase == slots$phase[i], .data$size == slots$size[i], !(.data$location %in% used))
    cand <- if (slots$kind[i] == "gain") arrange(cand, desc(.data$share)) else arrange(cand, .data$share)
    pick <- cand[1, ] %>% mutate(kind = slots$kind[i])
    used <<- c(used, pick$location)
    pick
  })
  bind_rows(picks) %>%
    select("location", "reference_date", "phase", "size", "kind", "share", "wis_pct")
}


#' V-gallery panel: one forecast (all horizons), base and calibrated 50% and
#' 90% bands, the snapshot the forecaster saw (black) and finalized truth (grey).
#' @export
cal_gallery_panel <- function(cal, loc, ref_date, truth, vintages, subtitle = NULL) {
  levels_map <- c("0.05" = "lo90", "0.25" = "lo50", "0.5" = "med", "0.75" = "hi50", "0.95" = "hi90")
  fin <- truth %>% filter(.data$location == loc, .data$target_end_date >= ref_date - 84L, .data$target_end_date <= ref_date + 28L)
  snap <- cal_round_snapshots(vintages, loc, ref_date)
  fan <- cal$forecasts %>%
    filter(.data$location == loc, .data$reference_date == ref_date, .data$level %in% as.numeric(names(levels_map))) %>%
    mutate(lvl = levels_map[as.character(.data$level)]) %>%
    select("target_end_date", "lvl", base = "value_base", calibrated = "value_cal") %>%
    tidyr::pivot_longer(c("base", "calibrated"), names_to = "which") %>%
    tidyr::pivot_wider(names_from = "lvl", values_from = "value")
  ggplot2::ggplot() +
    ggplot2::geom_line(data = fin, ggplot2::aes(.data$target_end_date, .data$truth), colour = "grey65") +
    ggplot2::geom_point(data = fin, ggplot2::aes(.data$target_end_date, .data$truth), colour = "grey65", size = 0.7) +
    ggplot2::geom_ribbon(data = fan, ggplot2::aes(.data$target_end_date, ymin = .data$lo90, ymax = .data$hi90, fill = .data$which), alpha = 0.16) +
    ggplot2::geom_ribbon(data = fan, ggplot2::aes(.data$target_end_date, ymin = .data$lo50, ymax = .data$hi50, fill = .data$which), alpha = 0.3) +
    ggplot2::geom_line(data = fan, ggplot2::aes(.data$target_end_date, .data$med, colour = .data$which)) +
    ggplot2::geom_line(data = snap, ggplot2::aes(.data$target_end_date, .data$truth), colour = "black") +
    ggplot2::geom_point(data = snap, ggplot2::aes(.data$target_end_date, .data$truth), colour = "black", size = 0.7) +
    ggplot2::geom_vline(xintercept = hub_round_asof(ref_date), linetype = "dashed", colour = "grey40") +
    ggplot2::scale_fill_manual(values = CAL_FAN_COLS, aesthetics = c("fill", "colour")) +
    ggplot2::labs(x = NULL, y = "admissions", colour = NULL, fill = NULL, subtitle = subtitle)
}
