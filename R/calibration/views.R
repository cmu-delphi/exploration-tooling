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


#' V-head: WIS change % and L1 coverage bias by horizon, one row per
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


#' V-month: WIS change % by month of reference date, with each month's share
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
