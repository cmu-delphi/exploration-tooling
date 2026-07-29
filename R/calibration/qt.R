# Multi-level Quantile Tracker (MultiQT), Ding, Gibbs & Tibshirani (2025),
# arXiv:2512.23671. Reference implementation: ~/repos/delphi/multiQT
# (`utils.py::projectedQT`).
#
# Deliberately pure numerics: vectors and matrices in, matrices out, no epi_df /
# data.table / date handling. Everything about hub schemas, geo keys and calendar
# lag lives in the caller. That is what makes bit-exact comparison against the
# authors' Python possible (see tests/testthat/test-qt.R), and it is the whole
# debuggability story for this method -- when a calibrated forecast looks wrong,
# the question "is it the tracker or the plumbing?" has a cheap answer.
#
# The oracle is the `delphi-fixes` branch of that clone: the as-published code
# had a handful of defects (a learning-rate window that sliced the level axis
# instead of time, an unpopulated first played column, a default lr with no
# implementation), all reported upstream and fixed on that branch. This port
# implements the corrected semantics only.


#' Learning rate for the online quantile update.
#'
#' `"adaptive+"` is the paper's heuristic: a single scale-aware step size shared
#' by every quantile level, set to a fraction of the recent absolute residual of
#' the *base* forecasts, floored so the tracker can never freeze.
#'
#' The floor is the one place raw outcome units leak into an otherwise
#' scale-equivariant algorithm. Everything else is equivariant under
#' `(Y, Yhat) -> c * (Y, Yhat)`: residuals scale by `c`, so eta scales by `c`, so
#' the offsets do too. `max(eta, floor)` does not, so a series whose typical
#' residual is below `floor / mult` gets a step size in absolute units. For
#' weekly flu admissions that only binds in the smallest geos, where it amounts
#' to a drift of `floor` admissions/week. Harmless, but it means per-capita and
#' count-space runs are not exactly equivalent.
#'
#' @param lr a positive scalar (constant step size) or `"adaptive+"`.
#' @param residuals matrix of base-forecast residuals `Y - Yhat`, levels x
#'   observed-time, already restricted to the window.
#' @param floor,mult,prob heuristic constants. The reference hardcodes
#'   `0.1, 0.1, 0.9`.
#' @keywords internal
qt_learning_rate <- function(lr, residuals, floor = 0.1, mult = 0.1, prob = 0.9) {
  if (is.numeric(lr)) {
    if (length(lr) != 1L || !is.finite(lr) || lr <= 0) {
      cli::cli_abort("A numeric {.arg lr} must be a single positive finite value.")
    }
    return(lr)
  }
  if (!identical(lr, "adaptive+")) {
    cli::cli_abort("{.arg lr} must be a positive scalar or {.val adaptive+}.")
  }
  # Pooled across levels and across the time window, matching the reference:
  # one eta per (series, round), not one per quantile level.
  max(floor, mult * stats::quantile(abs(residuals), prob, names = FALSE, type = 7L))
}


#' Project a vector of quantile values back onto the ordered cone.
#'
#' `"isotonic"` is PAVA -- the L2 projection, and the one the guarantee rests on.
#' `"sort"` also returns something monotone but is a rearrangement, not a
#' projection (on `c(3, 1, 2)`: sort gives `c(1, 2, 3)`, PAVA gives `c(2, 2, 2)`),
#' so it carries no guarantee; it is here only to quantify the difference.
#' `"none"` is plain Quantile Tracker, which is allowed to cross.
#'
#' Unweighted PAVA depends only on the ordering of the x values, not their
#' magnitudes, so passing the quantile levels as x (as the reference does) is
#' identical to using the index.
#' @keywords internal
qt_project <- function(v, projection = c("isotonic", "sort", "none")) {
  projection <- rlang::arg_match(projection)
  switch(projection,
    isotonic = stats::isoreg(v)$yf,
    sort = sort(v),
    none = v
  )
}


#' Build a delay structure from calendar dates.
#'
#' `delay[[t]]` lists the round indices whose outcome becomes learnable at round
#' `t`. This is the piece most likely to harbour an off-by-one, because the
#' availability rule is stated in calendar time while the tracker steps over
#' *rounds*, and the rounds are not contiguous -- FluSight has 2-week holiday
#' gaps, a 5-week gap in Oct/Nov 2025, and 20-29 week off-season gaps. So `d`
#' weeks of lag is not `d` rounds of lag, and after a gap the applied offset is
#' staler than nominal.
#'
#' The rule: round `s` (issued at `round_date[s]`, targeting `target_date[s]`) is
#' learnable at the earliest round `t` with
#' `round_date[t] >= target_date[s] + settle_days`. Rounds whose outcome never
#' becomes learnable within the series are simply never revealed.
#'
#' @param round_date dates indexing the tracker's time axis, strictly increasing.
#' @param target_date the date each round's forecast is about, same length.
#' @param settle_days how long after `target_date` the outcome is trusted. For
#'   NHSN, revisions land for about two weeks and the first publication
#'   consistently under-reports, hence the 14-day default.
#' @return list of length `length(round_date)` of integer vectors.
#' @export
qt_delay_from_dates <- function(round_date, target_date, settle_days = 14L) {
  round_date <- as.Date(round_date)
  target_date <- as.Date(target_date)
  n <- length(round_date)
  if (length(target_date) != n) {
    cli::cli_abort("{.arg round_date} and {.arg target_date} must be the same length.")
  }
  if (n > 1L && any(diff(round_date) <= 0)) {
    cli::cli_abort("{.arg round_date} must be strictly increasing.")
  }

  available_on <- target_date + settle_days
  # First round at or after each outcome becomes trustworthy. findInterval on
  # `available_on - 1` counts the round_dates strictly below available_on (dates
  # are whole days), so +1 is that round.
  reveal_at <- findInterval(
    as.numeric(available_on) - 1, as.numeric(round_date)
  ) + 1L
  reveal_at[reveal_at > n] <- NA_integer_

  delay <- rep(list(integer(0)), n)
  keep <- which(!is.na(reveal_at))
  if (length(keep) > 0) {
    split_by <- split(keep, reveal_at[keep])
    delay[as.integer(names(split_by))] <- lapply(split_by, as.integer)
  }
  qt_validate_delay(delay, n)
  delay
}


#' Assert the two invariants that catch essentially every delay off-by-one:
#' nothing is revealed before it happened, and nothing is revealed twice.
#' @keywords internal
qt_validate_delay <- function(delay, n) {
  if (!is.list(delay) || length(delay) != n) {
    cli::cli_abort("{.arg delay} must be a list of length {n}.")
  }
  flat <- unlist(delay, use.names = FALSE)
  if (length(flat) == 0L) {
    return(invisible(delay))
  }
  if (!all(flat %in% seq_len(n))) {
    cli::cli_abort("{.arg delay} contains indices outside {.code 1:{n}}.")
  }
  for (t in seq_len(n)) {
    bad <- delay[[t]][delay[[t]] > t]
    if (length(bad) > 0) {
      cli::cli_abort(
        "{.arg delay} reveals {cli::qty(length(bad))}future outcome{?s}
         {.val {bad}} at round {t}; an outcome cannot be learnable before it is
         observed."
      )
    }
  }
  dup <- flat[duplicated(flat)]
  if (length(dup) > 0) {
    cli::cli_abort(
      "{.arg delay} reveals {cli::qty(length(unique(dup)))}round{?s}
       {.val {unique(dup)}} more than once; each outcome must enter the update
       exactly once."
    )
  }
  invisible(delay)
}


#' Run the Multi-level Quantile Tracker over one series.
#'
#' At each round the played (i.e. actually issued) forecast is
#' `project(base + hidden)`. The hidden offsets accumulate lazily -- they are
#' never themselves projected -- and the gradient is evaluated at the *played*
#' value, not the hidden one. That combination is what buys both the coverage
#' guarantee and non-crossing quantiles; evaluating the gradient at the hidden
#' iterate, or projecting the hidden state in place, breaks it.
#'
#' The update is `hidden[i, t+1] += eta * ((Y[s] > played[i, s]) - (1 - tau_i))`
#' over the outcomes `s` revealed at `t`, which is descent on the pinball loss:
#' under-coverage pushes the offset up, over-coverage pushes it down, and the
#' fixed point is nominal coverage.
#'
#' @param Y length-T outcomes. `Y[t]` may be `NA` as long as no `delay` entry
#'   ever reveals `t`.
#' @param Yhat base forecasts, `length(levels)` x T, rows in the same order as
#'   `levels`. Rows are expected non-decreasing down the level axis; the tracker
#'   does not require it, but crossings in the base propagate.
#' @param levels strictly increasing quantile levels in (0, 1).
#' @param delay list of length T; `delay[[t]]` are the round indices whose
#'   outcomes become learnable at round `t`. `NULL` means each `Y[t]` is observed
#'   at `t` (no delay), which is unrealistic for revision-prone targets. Build it
#'   with [qt_delay_from_dates()].
#' @param lr `"adaptive+"` or a positive scalar. See [qt_learning_rate()].
#' @param lr_window number of most recent *observed* rounds whose residuals set
#'   the learning rate. `Inf` uses the full observed history (an expanding
#'   window).
#' @param init_hidden scalar or length-m starting offsets.
#' @param projection see [qt_project()]. `"none"` is plain QT.
#' @param eval_grad_at `"played"` (the method) or `"hidden"` (for diagnosis).
#' @param nonneg clamp played values at 0. Applied *after* the projection and
#'   *outside* the learning loop, so it cannot corrupt the gradient path -- the
#'   tracker always scores the value it actually played.
#' @param update_from logical of length T. `FALSE` at round `t` means outcomes
#'   revealed at `t` still enter the learning-rate residual pool but produce no
#'   gradient step. This is what a burn-in season is: use the base forecasts and
#'   their errors to warm up `eta`, but leave the offsets at zero so nothing is
#'   calibrated on data we are only using to set a step size.
#' @param hidden_scale numeric of length T, applied to the hidden offsets at the
#'   start of each round. `1` everywhere (the default) is a no-op. Setting it at
#'   a season boundary expresses the season-gap policy: `0` resets the offsets,
#'   `1` carries them across the off-season, and anything between shrinks them.
#'   Carrying is the aggressive choice -- across FluSight's 20-29 week off-season
#'   the base forecaster itself changed, so a carried offset is a correction for
#'   a model that no longer exists.
#'
#' @return list with
#'   `played` (m x T, the calibrated forecasts),
#'   `hidden` (m x T, the un-projected offsets),
#'   `offset` (m x T, `played - Yhat`, the realised total correction),
#'   `lr` (length T, the step size used at each round; 0 where no update ran),
#'   `gradient` (m x T),
#'   `n_observed` (length T, how many outcomes had been revealed by each round),
#'   `delay` (the resolved delay).
#'   The extra fields are not diagnostics-as-afterthought: the learning-rate
#'   trajectory and the hidden/played split are what the EDA notebook plots.
#' @export
qt_track <- function(
  Y,
  Yhat,
  levels,
  delay = NULL,
  lr = "adaptive+",
  lr_window = 50,
  init_hidden = 0,
  projection = c("isotonic", "sort", "none"),
  eval_grad_at = c("played", "hidden"),
  nonneg = FALSE,
  update_from = NULL,
  hidden_scale = NULL,
  lr_args = list()
) {
  projection <- rlang::arg_match(projection)
  eval_grad_at <- rlang::arg_match(eval_grad_at)

  Y <- as.numeric(Y)
  levels <- as.numeric(levels)
  Yhat <- as.matrix(Yhat)
  storage.mode(Yhat) <- "double"
  m <- length(levels)
  n <- length(Y)

  if (m < 1L || n < 1L) {
    cli::cli_abort("{.arg levels} and {.arg Y} must both be non-empty.")
  }
  if (!identical(dim(Yhat), c(m, n))) {
    cli::cli_abort(
      "{.arg Yhat} must be {m} x {n} (levels x rounds), not
       {nrow(Yhat)} x {ncol(Yhat)}."
    )
  }
  # A round with no base forecast is "inactive": nothing was played, so nothing
  # can be scored there, but the offsets carry across it and other rounds still
  # update normally. This is not a corner case -- CMU-TimeSeries' 2023-24
  # FluSight rounds omit horizon -1 almost entirely and drop locations in and
  # out, so only 52 of 265 series span every round. Half-missing columns are
  # still an error: that means a partial submission, not a skipped one.
  col_na <- colSums(is.na(Yhat) | !is.finite(Yhat))
  mixed <- which(col_na > 0L & col_na < m)
  if (length(mixed) > 0L) {
    cli::cli_abort(
      "{.arg Yhat} column{?s} {.val {mixed}} {?is/are} partially missing. A round
       must supply every quantile level or none of them."
    )
  }
  active <- col_na == 0L
  if (!any(active)) {
    cli::cli_abort("{.arg Yhat} has no round with a complete base forecast.")
  }
  if (m > 1L && any(diff(levels) <= 0)) {
    cli::cli_abort("{.arg levels} must be strictly increasing.")
  }
  if (any(levels <= 0 | levels >= 1)) {
    cli::cli_abort("{.arg levels} must lie strictly inside (0, 1).")
  }
  if (!(is.numeric(lr_window) && length(lr_window) == 1L && lr_window >= 1)) {
    cli::cli_abort("{.arg lr_window} must be a single value >= 1 (possibly Inf).")
  }

  update_from <- update_from %||% rep(TRUE, n)
  hidden_scale <- hidden_scale %||% rep(1, n)
  if (length(update_from) != n || !is.logical(update_from) || anyNA(update_from)) {
    cli::cli_abort("{.arg update_from} must be a non-missing logical of length {n}.")
  }
  if (length(hidden_scale) != n || !all(is.finite(hidden_scale))) {
    cli::cli_abort("{.arg hidden_scale} must be a finite numeric of length {n}.")
  }

  delay <- delay %||% lapply(seq_len(n), function(t) t)
  qt_validate_delay(delay, n)
  revealed <- unlist(delay, use.names = FALSE)
  na_revealed <- revealed[is.na(Y[revealed])]
  if (length(na_revealed) > 0L) {
    cli::cli_abort(
      "{.arg delay} reveals {cli::qty(length(na_revealed))}round{?s}
       {.val {na_revealed}} whose {.arg Y} is {.code NA}. A missing outcome
       cannot be learned from; drop it from {.arg delay}."
    )
  }
  inactive_revealed <- revealed[!active[revealed]]
  if (length(inactive_revealed) > 0L) {
    cli::cli_abort(
      "{.arg delay} reveals {cli::qty(length(inactive_revealed))}round{?s}
       {.val {inactive_revealed}} that {?has/have} no base forecast. Nothing was
       played there, so there is no coverage to learn from; drop {?it/them} from
       {.arg delay}."
    )
  }

  # m x n. NOT `Y - Yhat`: numpy broadcasts a length-n vector along the last
  # axis, but R recycles column-major *down* the matrix, which silently
  # transposes the outcome onto the level axis.
  residual <- matrix(Y, nrow = m, ncol = n, byrow = TRUE) - Yhat

  hidden <- matrix(0, nrow = m, ncol = n)
  played <- matrix(0, nrow = m, ncol = n)
  gradient <- matrix(0, nrow = m, ncol = n)
  lr_used <- numeric(n)
  n_observed <- integer(n)

  hidden[, 1L] <- init_hidden * hidden_scale[1L]
  played[, 1L] <- if (!active[1L]) {
    NA_real_
  } else {
    qt_project(hidden[, 1L] + Yhat[, 1L], projection)
  }

  # Observed rounds in reveal order; the learning-rate window is the tail of
  # this, so it is "the last k outcomes we saw", not "the last k calendar weeks".
  observed <- integer(0)

  for (t in seq_len(n - 1L)) {
    observed <- c(observed, delay[[t]])
    n_observed[t] <- length(observed)

    eval_point <- if (eval_grad_at == "played") {
      played[, t]
    } else {
      hidden[, t] + Yhat[, t]
    }
    # (Y > q) - (1 - tau) == -((Y <= q) - tau): a covered upper quantile pushes
    # the offset down, an exceeded one pushes it up. NA at an inactive round,
    # which delay validation guarantees is never consumed.
    gradient[, t] <- as.numeric(Y[t] > eval_point) - (1 - levels)

    hidden[, t + 1L] <- hidden[, t]
    if (length(delay[[t]]) > 0L && length(observed) > 0L) {
      window <- if (is.finite(lr_window)) {
        utils::tail(observed, lr_window)
      } else {
        observed
      }
      eta <- do.call(
        qt_learning_rate,
        c(list(lr, residual[, window, drop = FALSE]), lr_args)
      )
      # Recorded even during burn-in: watching eta warm up is the point of
      # having a burn-in season at all.
      lr_used[t] <- eta
      if (update_from[t]) {
        for (s in delay[[t]]) {
          hidden[, t + 1L] <- hidden[, t + 1L] + eta * gradient[, s]
        }
      }
    }
    hidden[, t + 1L] <- hidden[, t + 1L] * hidden_scale[t + 1L]
    played[, t + 1L] <- if (active[t + 1L]) {
      qt_project(hidden[, t + 1L] + Yhat[, t + 1L], projection)
    } else {
      NA_real_
    }
  }
  n_observed[n] <- length(c(observed, delay[[n]]))

  # Gradient at the final round is never consumed (there is no round n+1), but
  # record it so coverage diagnostics can use the whole series.
  eval_point <- if (eval_grad_at == "played") played[, n] else hidden[, n] + Yhat[, n]
  gradient[, n] <- as.numeric(Y[n] > eval_point) - (1 - levels)

  if (nonneg) {
    played <- pmax(played, 0)
  }

  list(
    played = played,
    hidden = hidden,
    offset = played - Yhat,
    lr = lr_used,
    gradient = gradient,
    n_observed = n_observed,
    delay = delay
  )
}
