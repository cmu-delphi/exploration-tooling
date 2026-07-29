# Driver: run the quantile tracker over every (location, horizon) series of a
# hub model's submitted forecasts.
#
# All the date and schema reasoning lives here; qt_track() sees only matrices.
# The two things this layer owns that are easy to get wrong:
#
#   1. A single GLOBAL round axis shared by every series, taken from the union of
#      submission dates. Per-series axes would let one location's round 40 mean a
#      different week from another's, which would make the season/burn-in vectors
#      and every diagnostic silently incomparable.
#   2. Pruning the delay for rounds whose truth is missing. qt_delay_from_dates()
#      only knows the calendar; it will happily reveal a round whose outcome does
#      not exist yet (the live tail of a season) and qt_track() refuses to learn
#      from NA, so the pruning has to happen in between.


#' Reshape one (location, horizon) series onto the global round axis as the
#' levels x rounds matrix qt_track() wants.
#'
#' Rounds the series does not cover become all-NA columns -- "inactive" rounds,
#' which qt_track carries the offsets across without playing or scoring anything.
#' This is the common case, not an edge case: CMU-TimeSeries' 2023-24 rounds omit
#' horizon -1 in 23 of 28 rounds and drop locations in and out (78% cell
#' completeness), so only 52 of 265 series span all 83 rounds. Restricting to the
#' complete rectangle instead would throw away most of the burn-in season.
#'
#' A round that is present but incomplete is still an error: that is a partial
#' submission, which we want to hear about rather than silently pad.
#' @keywords internal
hub_series_matrix <- function(series, round_date, n_levels) {
  n <- length(round_date)
  ri <- match(series$reference_date, round_date)
  if (anyNA(ri)) {
    cli::cli_abort("Series has rounds absent from the global round axis.")
  }
  if (!all(series$level_index %in% seq_len(n_levels))) {
    cli::cli_abort("Series carries a level index outside {.code 1:{n_levels}}.")
  }
  out <- matrix(NA_real_, nrow = n_levels, ncol = n)
  out[cbind(series$level_index, ri)] <- series$value

  filled <- colSums(!is.na(out))
  partial <- which(filled > 0L & filled < n_levels)
  if (length(partial) > 0L) {
    cli::cli_abort(
      "Round{?s} {.val {format(round_date[partial])}} carr{?ies/y} only some
       quantile levels; expected {n_levels} or none."
    )
  }
  out
}


#' Online-calibrate a hub model's submitted forecasts, per (location, horizon).
#'
#' @param forecasts [hub_read_forecasts()] output.
#' @param truth [hub_read_truth()] output.
#' @param settle_days how long after `target_end_date` the outcome is trusted.
#'   14 gives exactly `horizon + 2` rounds of delay on hub coordinates.
#' @param burn_in_seasons season labels (as produced by [hub_label_seasons()])
#'   used only to warm up the learning rate: their residuals enter the `eta`
#'   pool, but no gradient step is taken and the offsets stay at zero. Pass
#'   `"2023-2024"` to reproduce the intended design.
#' @param season_policy what happens to the offsets across an off-season gap.
#'   `"carry"` keeps them, `"reset"` zeroes them, `"shrink"` multiplies by
#'   `shrink_factor`. Not a free choice: the base forecaster changed between
#'   seasons, so carrying is a bet that the miscalibration outlived the model.
#' @param lr,lr_window,projection,nonneg passed to [qt_track()].
#' @param progress show a progress bar.
#' @return list with
#'   `forecasts` long tibble, one row per (round, horizon, location, level), with
#'     `value_base`, `value_cal`, `offset`, `truth`, plus round metadata;
#'   `rounds` the global round axis with season labels;
#'   `series` per-series diagnostics: the `lr` trajectory, `n_observed`, and how
#'     many rounds actually produced an update.
#' @export
calibrate_hub_forecasts <- function(
  forecasts,
  truth,
  settle_days = 14L,
  burn_in_seasons = character(0),
  season_policy = c("carry", "reset", "shrink"),
  shrink_factor = 0.5,
  lr = "adaptive+",
  lr_window = 50,
  lr_args = list(),
  projection = c("isotonic", "sort", "none"),
  nonneg = TRUE,
  progress = TRUE
) {
  season_policy <- rlang::arg_match(season_policy)
  projection <- rlang::arg_match(projection)

  rounds <- hub_label_seasons(unique(forecasts$reference_date))
  round_date <- rounds$round_date
  n <- nrow(rounds)
  levels <- sort(unique(forecasts$level))
  m <- length(levels)

  # Shared across every series: burn-in gating and the season-gap policy are
  # properties of the calendar, not of a location.
  update_from <- !(rounds$season %in% burn_in_seasons)
  hidden_scale <- rep(1, n)
  boundary <- which(rounds$is_season_start & rounds$round_index > 1L)
  hidden_scale[boundary] <- switch(season_policy,
    carry = 1,
    reset = 0,
    shrink = shrink_factor
  )

  if (all(!update_from)) {
    cli::cli_abort(
      "Every season is a burn-in season; nothing would be calibrated.
       Seasons present: {.val {unique(rounds$season)}}."
    )
  }
  cli::cli_alert_info(
    "Calibrating {.val {n}} round{?s} across season{?s} {.val {unique(rounds$season)}};
     burn-in {.val {if (length(burn_in_seasons)) burn_in_seasons else 'none'}},
     season gap policy {.val {season_policy}}."
  )

  truth_lookup <- stats::setNames(
    truth$truth, paste(truth$location, truth$target_end_date)
  )

  grid <- forecasts %>%
    distinct(.data$location, .data$horizon) %>%
    arrange(.data$location, .data$horizon)
  keyed <- forecasts %>% group_by(.data$location, .data$horizon)
  keys <- keyed %>% group_keys()
  chunks <- keyed %>% group_split()
  ord <- order(keys$location, keys$horizon)
  keys <- keys[ord, ]
  chunks <- chunks[ord]

  if (progress) cli::cli_progress_bar("tracking", total = nrow(keys))
  per_series <- vector("list", nrow(keys))
  diagnostics <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    if (progress) cli::cli_progress_update(set = i)
    loc <- keys$location[i]
    h <- keys$horizon[i]
    Yhat <- hub_series_matrix(chunks[[i]], round_date, m)

    target_end_date <- round_date + 7L * h
    Y <- unname(truth_lookup[paste(loc, target_end_date)])

    # Calendar rule first, then drop anything that cannot be learned from: no
    # truth yet (the live tail of a season), or no base forecast at that round
    # (nothing was played, so there is no coverage). A round pruned here is not
    # "revealed late", it is never learned from at all.
    learnable <- !is.na(Y) & !apply(is.na(Yhat), 2, any)
    delay <- qt_delay_from_dates(round_date, target_end_date, settle_days)
    delay <- lapply(delay, function(idx) idx[learnable[idx]])

    res <- qt_track(
      Y = Y, Yhat = Yhat, levels = levels, delay = delay,
      lr = lr, lr_window = lr_window, lr_args = lr_args, projection = projection,
      nonneg = nonneg,
      update_from = update_from, hidden_scale = hidden_scale
    )

    per_series[[i]] <- tibble(
      location = loc,
      horizon = h,
      round_index = rep(seq_len(n), each = m),
      level_index = rep(seq_len(m), times = n),
      level = rep(levels, times = n),
      value_base = as.vector(Yhat),
      value_cal = as.vector(res$played),
      offset = as.vector(res$offset),
      truth = rep(Y, each = m)
    )
    diagnostics[[i]] <- tibble(
      location = loc,
      horizon = h,
      round_index = seq_len(n),
      lr = res$lr,
      n_observed = res$n_observed,
      n_revealed = lengths(res$delay)
    )
    if (progress) NULL
  }
  if (progress) cli::cli_progress_done()

  round_meta <- rounds %>%
    select("round_index", "round_date", "season", "season_round", "gap_weeks")

  out_forecasts <- bind_rows(per_series) %>%
    left_join(round_meta, by = "round_index") %>%
    mutate(
      reference_date = .data$round_date,
      target_end_date = .data$round_date + 7L * .data$horizon,
      is_burn_in = .data$season %in% burn_in_seasons
    ) %>%
    select(
      "location", "horizon", "reference_date", "target_end_date", "season",
      "season_round", "round_index", "level_index", "level",
      "value_base", "value_cal", "offset", "truth", "is_burn_in"
    )

  list(
    forecasts = out_forecasts,
    rounds = rounds %>% mutate(
      is_burn_in = .data$season %in% burn_in_seasons,
      hidden_scale = hidden_scale,
      updates = update_from
    ),
    series = bind_rows(diagnostics) %>% left_join(round_meta, by = "round_index")
  )
}


#' Geo-pooled coverage per (horizon, level), base vs calibrated.
#'
#' Pooling across locations is not a convenience: per (location, horizon, level)
#' a season is only ~25-30 rounds minus delay, so a single series cannot resolve
#' the 1% or 99% levels at all (~0.3 expected exceedances). Pooled over 53
#' locations there are ~1500 observations per (horizon, level) per season, which
#' resolves the middle of the distribution and marginally the 2.5%/97.5% levels.
#' Nothing should be tuned on per-series coverage.
#'
#' @param cal [calibrate_hub_forecasts()] output, or its `forecasts` element.
#' @param drop_burn_in exclude burn-in-season rounds, which by construction have
#'   zero offsets and would dilute the comparison toward "no effect".
#' @export
hub_coverage <- function(cal, drop_burn_in = TRUE, by = character(0)) {
  fc <- if (is.list(cal) && !is.data.frame(cal)) cal$forecasts else cal
  fc <- fc %>% filter(!is.na(.data$truth))
  if (drop_burn_in) fc <- fc %>% filter(!.data$is_burn_in)
  fc %>%
    group_by(across(all_of(c(by, "horizon", "level")))) %>%
    summarize(
      n = dplyr::n(),
      coverage_base = mean(.data$truth <= .data$value_base),
      coverage_cal = mean(.data$truth <= .data$value_cal),
      .groups = "drop"
    ) %>%
    mutate(
      gap_base = .data$coverage_base - .data$level,
      gap_cal = .data$coverage_cal - .data$level
    )
}


#' Headline number: mean |coverage - nominal| over levels, per horizon.
#' @export
hub_coverage_summary <- function(cal, drop_burn_in = TRUE, by = character(0)) {
  hub_coverage(cal, drop_burn_in = drop_burn_in, by = by) %>%
    group_by(across(all_of(c(by, "horizon")))) %>%
    summarize(
      n_obs = sum(.data$n),
      cal_error_base = mean(abs(.data$gap_base)),
      cal_error_cal = mean(abs(.data$gap_cal)),
      .groups = "drop"
    ) %>%
    mutate(
      improvement = .data$cal_error_base - .data$cal_error_cal,
      pct_improvement = 100 * .data$improvement / .data$cal_error_base
    )
}


#' Mean pinball (quantile) loss per (horizon), base vs calibrated.
#'
#' Averaged over quantile levels, so this is WIS up to the usual factor-of-two
#' convention and directly comparable between the two columns.
#' @export
hub_quantile_loss <- function(cal, drop_burn_in = TRUE, by = character(0)) {
  fc <- if (is.list(cal) && !is.data.frame(cal)) cal$forecasts else cal
  fc <- fc %>% filter(!is.na(.data$truth))
  if (drop_burn_in) fc <- fc %>% filter(!.data$is_burn_in)
  pinball <- function(y, q, tau) ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))
  fc %>%
    mutate(
      loss_base = pinball(.data$truth, .data$value_base, .data$level),
      loss_cal = pinball(.data$truth, .data$value_cal, .data$level)
    ) %>%
    group_by(across(all_of(c(by, "horizon")))) %>%
    summarize(
      n_obs = dplyr::n(),
      loss_base = mean(.data$loss_base),
      loss_cal = mean(.data$loss_cal),
      .groups = "drop"
    ) %>%
    mutate(pct_improvement = 100 * (.data$loss_base - .data$loss_cal) / .data$loss_base)
}


#' Rolling calibration error against rolling quantile loss, the trade-off curve.
#'
#' Both quantities are rolling averages over the last `window` rounds of one
#' series (or of a geo-pooled set of series):
#'   calibration error = mean over levels of |rolling coverage at a - a|
#'   quantile loss     = mean over levels and rounds of the pinball loss
#' Plotting one against the other shows whether calibration is being bought at
#' the cost of sharpness, which is the thing a tracker can quietly get wrong.
#'
#' @param cal [calibrate_hub_forecasts()] output.
#' @param window number of rounds in the rolling window.
#' @param by grouping columns; `character(0)` pools all locations, which is the
#'   default because per-series rolling coverage over 20 rounds is mostly noise.
#' @export
hub_rolling_tradeoff <- function(cal, window = 20L, by = character(0), drop_burn_in = TRUE) {
  fc <- if (is.list(cal) && !is.data.frame(cal)) cal$forecasts else cal
  fc <- fc %>% filter(!is.na(.data$truth))
  if (drop_burn_in) fc <- fc %>% filter(!.data$is_burn_in)
  pinball <- function(y, q, tau) ifelse(y >= q, tau * (y - q), (1 - tau) * (q - y))

  # Per (round, level) within each group: coverage indicator and pinball loss,
  # pooled over whatever `by` does not name (locations, by default).
  per_round <- fc %>%
    group_by(across(all_of(c(by, "horizon", "round_index", "reference_date", "level")))) %>%
    summarize(
      cov_base = mean(.data$truth <= .data$value_base),
      cov_cal = mean(.data$truth <= .data$value_cal),
      loss_base = mean(pinball(.data$truth, .data$value_base, .data$level)),
      loss_cal = mean(pinball(.data$truth, .data$value_cal, .data$level)),
      .groups = "drop"
    )

  roll <- function(x) slider::slide_dbl(x, mean, .before = window - 1L, .complete = TRUE)
  per_round %>%
    group_by(across(all_of(c(by, "horizon", "level")))) %>%
    arrange(.data$round_index, .by_group = TRUE) %>%
    mutate(
      roll_cov_base = roll(.data$cov_base),
      roll_cov_cal = roll(.data$cov_cal),
      roll_loss_base = roll(.data$loss_base),
      roll_loss_cal = roll(.data$loss_cal)
    ) %>%
    ungroup() %>%
    filter(!is.na(.data$roll_cov_base)) %>%
    # Now collapse the level axis: calibration error is the mean over levels of
    # |rolling coverage - nominal|.
    group_by(across(all_of(c(by, "horizon", "round_index", "reference_date")))) %>%
    summarize(
      cal_error_base = mean(abs(.data$roll_cov_base - .data$level)),
      cal_error_cal = mean(abs(.data$roll_cov_cal - .data$level)),
      qloss_base = mean(.data$roll_loss_base),
      qloss_cal = mean(.data$roll_loss_cal),
      .groups = "drop"
    )
}
