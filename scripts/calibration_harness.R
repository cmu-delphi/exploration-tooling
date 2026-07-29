# Fast-iteration harness for online calibration of a prod forecaster.
#
# Why this exists: prod and explore are both targets DAGs, so every idea costs a
# rebuild. Calibration only needs (a) a long history of one forecaster's
# distributional forecasts and (b) truth. This script backfills (a) once into a
# cache, reads (b) from the evaluation store's archives, and then lets the
# calibration algorithm be iterated on in-memory in seconds.
#
# It is NOT a reimplementation of the pipeline. ch_backfill() calls the same
# make_forecast_snapshot() + run_forecaster() pair that the prod `forecast_nhsn`
# target calls, with the spec values copied from the grid row in
# scripts/covid_hosp_prod.R. ch_verify_against_store() asserts that equivalence
# against the cached prod targets (currently exact: max abs diff 0).
#
# Target forecaster: windowed_seasonal_extra_sources. This is the forecaster that
# effectively IS the CMU-TimeSeries covid submission -- `ensemble_mix` (the
# submitted ensemble) renormalizes to ~86% windowed_seasonal_extra_sources,
# ~14% windowed_seasonal, ~0.03% climate_linear.
#
# Usage, from the repo root:
#   source("scripts/calibration_harness.R")
#   inp <- ch_inputs()               # archives, geos dropped
#   raw <- ch_backfill(inp)          # ~6 min cold, instant warm (cached)
#   ch_verify_against_store(raw)     # prove the harness == the DAG
#   cal <- ch_calibrate(raw, inp)    # the part you actually iterate on
#   ch_compare(raw, cal, inp)

suppressPackageStartupMessages(source(here::here("R/load_all.R")))

# ================================ CONFIG ==================================

CH_STORE <- "covid_hosp_evaluation"
CH_DISEASE <- "covid"
CH_TARGET <- "wk inc covid hosp"
CH_FORECASTER_ID <- "windowed_seasonal_extra_sources"
# Prod submits aheads -1:3; ahead -1 is excluded here because the submitted
# ensemble drops negative aheads from its AR components (drop_negative_aheads),
# so ahead -1 of the submission is 100% climate_linear and calibrating this
# forecaster there would have no effect on what we ship. Ahead 4 is beyond what
# prod submits; it is included because it is free and useful for seeing how the
# calibration behaves as the horizon grows.
CH_AHEADS <- 0:4
# Dropped from every archive before training and from the truth data, per
# the forecaster's `excluded_geos` spec. Dropping at the archive level (rather
# than filtering the output) keeps them out of the geo-pooled seasonal window.
CH_DROP_GEOS <- c("mo", "wy")
CH_CACHE_DIR <- here::here("cache/calibration")
# How long to wait before believing a truth value enough to learn from it. NHSN
# revises, and the most recent weeks revise the most, so the online update at
# time t only consumes targets whose observation is at least this old. See
# ch_calibrate() for how this interacts with the ahead.
CH_REVISION_LAG_WEEKS <- 2

# The grid row from scripts/covid_hosp_prod.R (g_forecaster_params_grid$
# windowed_seasonal_extra_sources), split the way make_forecaster_grid() splits
# it: modeling params go to the forecaster, spec values to run_forecaster().
# Kept as a literal rather than sourced so this harness can't trigger the prod
# script's target definitions; ch_verify_against_store() is what catches drift.
CH_SPEC <- list(
  forecaster = "scaled_pop_seasonal",
  params = list(
    outcome = "value",
    extra_sources = "nssp",
    trainer = epipredict::quantile_reg(),
    seasonal_method = "window",
    drop_non_seasons = TRUE,
    pop_scaling = FALSE,
    lags = list(c(0, 7), c(0, 7))
  ),
  ahead_multiplier = 7L,
  target_date_shift = 3L,
  join_extra_data = TRUE,
  sort_quantiles = TRUE
)


# ================================ INPUTS ==================================

# Drop geos from an epi_archive, preserving its other_keys.
ch_drop_geos <- function(archive, geos = CH_DROP_GEOS) {
  other_keys <- setdiff(key(archive$DT), c("geo_value", "time_value", "version"))
  archive$DT %>%
    filter(geo_value %nin% geos) %>%
    as_epi_archive(other_keys = other_keys, compactify = TRUE)
}

#' Read the archives the harness needs out of the evaluation store.
#'
#' Three distinct roles, deliberately not collapsed:
#'   nhsn_train  - the canonical Wednesday-shifted training archive
#'                 (`nhsn_prod_archive`), the forecaster's input.
#'   nssp_exo    - the RAW nssp archive (`nssp_archive_data`), keeping its `nssp`
#'                 column, joined in as an exogenous predictor.
#'   truth       - the RAW nhsn archive (`nhsn_archive_data`), Saturday-labeled.
#'                 Forecast target_end_dates land on Saturday (Wednesday archive
#'                 + target_date_shift = 3), so this joins to forecasts on
#'                 target_end_date == time_value with no further shifting.
#' @param drop_geos geos removed from all three archives. Defaults to
#'   CH_DROP_GEOS; ch_verify_against_store() passes character(0) to reproduce
#'   prod, which excludes them from the output instead.
#' @return a list; `truth_final` is the finalized truth used for scoring.
ch_inputs <- function(store = CH_STORE, drop_geos = CH_DROP_GEOS) {
  withr::local_envvar(TAR_PROJECT = store)
  cli::cli_alert_info("Reading archives from store {.path {store}}")
  nhsn_train <- ch_drop_geos(targets::tar_read(nhsn_prod_archive), drop_geos)
  nssp_exo <- ch_drop_geos(targets::tar_read(nssp_archive_data), drop_geos)
  truth <- ch_drop_geos(targets::tar_read(nhsn_archive_data), drop_geos)
  inputs <- list(
    nhsn_train = nhsn_train,
    nssp_exo = nssp_exo,
    truth = truth,
    truth_final = ch_truth_asof(truth, truth$versions_end),
    versions_end = min(nhsn_train$versions_end, nssp_exo$versions_end),
    drop_geos = drop_geos
  )
  cli::cli_alert_success(
    "Archives loaded; usable through {.val {format(inputs$versions_end)}}"
  )
  inputs
}

#' Truth as it was published at `version`, on the forecasts' target_end_date key.
#'
#' This is the honest input for an online update at time `version`: it contains
#' exactly the revisions that had landed by then, and nothing later.
ch_truth_asof <- function(truth_archive, version) {
  truth_archive %>%
    epix_as_of(min(as.Date(version), truth_archive$versions_end)) %>%
    as_tibble() %>%
    select(geo_value, target_end_date = time_value, truth = value) %>%
    filter(!is.na(truth))
}

#' The (forecast_date, generation_date) schedule, mirroring the evaluation-mode
#' branch of scripts/covid_hosp_prod.R.
#'
#' Expressed as "generation == forecast, except for these known delays" rather
#' than as the two parallel seq.Date() vectors prod uses. That is exactly
#' equivalent for the dates prod covers but can't silently misalign the two
#' columns if the window is changed.
ch_schedule <- function(through, from = as.Date("2024-11-20")) {
  # Holiday/outage weeks where the forecast actually ran after its nominal date.
  delays <- c(
    "2024-12-25" = "2024-12-26",
    "2025-01-01" = "2025-01-02",
    "2025-12-24" = "2025-12-29"
  )
  through <- as.Date(through)
  # Back up to the last Wednesday so the weekly sequence stays on schedule.
  through <- through - ((as.integer(format(through, "%u")) - 3) %% 7)
  forecast_date <- seq.Date(from, through, by = 7L)
  generation_date <- forecast_date
  hit <- match(as.character(forecast_date), names(delays))
  generation_date[!is.na(hit)] <- as.Date(delays[hit[!is.na(hit)]])
  tibble(forecast_date = forecast_date, generation_date = generation_date)
}


# ============================== BACKFILL ==================================

#' Backfill this forecaster's full forecast history, cached.
#'
#' One (nhsn, nssp) snapshot pair per date, reused across aheads, then one
#' run_forecaster() call per (date, ahead) exactly as the prod target does --
#' including prod's semantic seeding, so the stochastic path would match too.
#'
#' A date that errors is recorded and skipped rather than aborting the run; the
#' failures are reported at the end and returned as an attribute.
#'
#' @param refresh recompute and overwrite the cache.
#' @param substitutions path to a data-substitutions csv, or NULL (the default)
#'   to take the archive as it comes. Only ch_verify_against_store() sets it, to
#'   reproduce prod.
#' @param excluded_geos geos dropped from the forecaster's OUTPUT, as prod does
#'   it. NULL by default here because the harness drops them from the archive
#'   before training instead (see ch_inputs).
ch_backfill <- function(
  inputs = ch_inputs(),
  aheads = CH_AHEADS,
  schedule = NULL,
  refresh = FALSE,
  cache_dir = CH_CACHE_DIR,
  substitutions = NULL,
  excluded_geos = NULL
) {
  schedule <- schedule %||% ch_schedule(through = inputs$versions_end)
  # Key the cache on everything that changes the output, so a re-pulled archive
  # or a changed spec can't be served a stale history.
  cache_key <- rlang::hash(list(
    CH_FORECASTER_ID, CH_SPEC, aheads, inputs$drop_geos, schedule,
    substitutions, excluded_geos,
    inputs$nhsn_train$DT, inputs$nssp_exo$DT
  ))
  cache_file <- file.path(cache_dir, glue::glue("{CH_FORECASTER_ID}_{cache_key}.qs"))
  if (file.exists(cache_file) && !refresh) {
    cli::cli_alert_success("Loading cached forecasts from {.path {cache_file}}")
    return(qs::qread(cache_file))
  }

  cli::cli_alert_info(
    "Backfilling {.val {nrow(schedule)}} dates x {.val {length(aheads)}} aheads
     for {.field {CH_FORECASTER_ID}}"
  )
  failures <- list()
  out <- purrr::map(seq_len(nrow(schedule)), .progress = "forecasting", function(i) {
    fd <- schedule$forecast_date[i]
    gd <- schedule$generation_date[i]
    res <- tryCatch(
      {
        snapshot <- make_forecast_snapshot(
          inputs$nhsn_train, forecast_date = fd, generation_date = gd,
          as_of_policy = "asof", substitutions = substitutions
        )
        extra <- make_forecast_snapshot(
          inputs$nssp_exo, forecast_date = fd, generation_date = gd, as_of_policy = "asof"
        )
        purrr::map(aheads, function(ah) {
          # Prod seeds from the semantic cell key rather than the target name; do
          # the same so a stochastic forecaster would reproduce prod exactly.
          set.seed(targets::tar_seed_create(
            paste(CH_FORECASTER_ID, "nhsn", as.character(fd), ah, sep = "/")
          ))
          run_forecaster(
            snapshot = snapshot,
            forecaster = get(CH_SPEC$forecaster),
            aheads = ah * CH_SPEC$ahead_multiplier,
            params = CH_SPEC$params,
            id = CH_FORECASTER_ID,
            target_date_shift = CH_SPEC$target_date_shift,
            join_extra_data = CH_SPEC$join_extra_data,
            extra_data = extra,
            excluded_geos = excluded_geos,
            sort_quantiles = CH_SPEC$sort_quantiles
          ) %>%
            mutate(ahead = ah)
        }) %>%
          bind_rows()
      },
      error = function(e) {
        failures[[as.character(fd)]] <<- conditionMessage(e)
        NULL
      }
    )
    res
  }) %>%
    bind_rows()

  if (nrow(out) == 0) {
    cli::cli_abort("Backfill produced no forecasts; all {nrow(schedule)} dates failed.")
  }
  # forecast_date on the output is already the nominal Wednesday:
  # make_forecast_snapshot() stamps as_of = min(forecast_date, versions_end), and
  # the forecaster reads its forecast_date from that. Carry generation_date along
  # so the delayed weeks stay traceable.
  out <- out %>%
    left_join(schedule, by = "forecast_date") %>%
    arrange(forecast_date, ahead, geo_value, quantile)

  if (length(failures) > 0) {
    cli::cli_warn(
      "{length(failures)} date{?s} failed and were skipped: {.val {names(failures)}}"
    )
    attr(out, "failures") <- failures
  }
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  qs::qsave(out, cache_file)
  cli::cli_alert_success(
    "Backfilled {.val {length(unique(out$forecast_date))}} dates
     ({.val {nrow(out)}} rows) -> {.path {cache_file}}"
  )
  out
}


# ============================== CALIBRATION ===============================

#' Placeholder online calibrator: an additive per-(geo, ahead, quantile) offset
#' updated by online quantile (pinball) gradient descent.
#'
#' Deliberately the simplest thing that exercises the interface. The gradient of
#' the pinball loss at level tau w.r.t. an additive offset o on prediction
#' q = f + o is -tau when y > q and (1 - tau) when y < q, so the descent step
#' raises the offset by lr*tau on under-prediction and lowers it by lr*(1-tau)
#' on over-prediction. That is exactly online quantile tracking, and at
#' convergence the empirical coverage at each level matches its nominal level.
#'
#' The revision lag is the part that is not boilerplate. At forecast date t the
#' update may only consume targets observed at least `revision_lag_weeks` before
#' t, using truth as published at t. Because a forecast for ahead `a` is not
#' observed until t + 7a + target_date_shift, the forecast whose error is
#' learnable at time t was issued roughly 7a + shift + lag days ago -- so the
#' effective learning delay grows with the ahead, and each ahead's offset lags
#' reality by a different amount. That asymmetry is intrinsic, not a bug.
#'
#' Offsets are in raw count units, which are incomparable across geos, so the
#' step size is scaled per geo. The scale is fixed from truth available at the
#' first forecast date -- not recomputed from later data -- so it cannot leak.
#'
#' @param forecasts ch_backfill() output.
#' @param lr step size as a fraction of the per-geo scale.
#' @return `forecasts` plus `offset` (the offset in force when that forecast was
#'   issued) and `value_calibrated`.
ch_calibrate <- function(
  forecasts,
  inputs,
  revision_lag_weeks = CH_REVISION_LAG_WEEKS,
  lr = 0.05
) {
  lag_days <- revision_lag_weeks * 7L
  dates <- sort(unique(forecasts$forecast_date))

  # Per-geo step scale, fixed from what was observable at the first date.
  geo_scale <- ch_truth_asof(inputs$truth, dates[1]) %>%
    group_by(geo_value) %>%
    summarize(scale = max(stats::median(truth, na.rm = TRUE), 1), .groups = "drop")

  # Offset state, keyed by the cells the calibrator learns independently.
  offsets <- forecasts %>%
    distinct(geo_value, ahead, quantile) %>%
    mutate(offset = 0)

  # Forecasts as actually issued, keyed by forecast date, each carrying the
  # offset that was in force at the time -- the update has to score the
  # prediction we would really have made, not the raw one.
  issued <- list()
  prev_cut <- NULL
  # Widest span from a forecast date to one of its target_end_dates, used to skip
  # issued dates that can't contribute to the current update window.
  max_span <- max(forecasts$target_end_date - forecasts$forecast_date)
  cli::cli_progress_bar("calibrating", total = length(dates))
  for (i in seq_along(dates)) {
    t <- dates[i]
    cli::cli_progress_update(set = i)

    # --- 1. apply the current offsets to this date's forecasts -------------
    today <- forecasts %>%
      filter(forecast_date == t) %>%
      left_join(offsets, by = c("geo_value", "ahead", "quantile")) %>%
      mutate(offset = coalesce(offset, 0))
    issued[[as.character(t)]] <- today

    # --- 2. learn from everything that just became believable --------------
    # Truth as published now, and only targets old enough to have settled.
    cut <- t - lag_days
    if (!is.null(prev_cut)) {
      truth_now <- ch_truth_asof(inputs$truth, t)
      # Only issued dates whose target span can overlap (prev_cut, cut].
      candidates <- as.Date(names(issued))
      relevant <- issued[candidates <= cut & candidates + max_span > prev_cut]
      newly <- if (length(relevant) == 0) {
        tibble()
      } else {
        bind_rows(relevant) %>%
          filter(target_end_date > prev_cut, target_end_date <= cut) %>%
          inner_join(truth_now, by = c("geo_value", "target_end_date"))
      }
      if (nrow(newly) > 0) {
        step <- newly %>%
          mutate(
            prediction = value + offset,
            # Descent on the pinball loss; sign is the only content here.
            grad = if_else(truth > prediction, -quantile, 1 - quantile)
          ) %>%
          left_join(geo_scale, by = "geo_value") %>%
          mutate(scale = coalesce(scale, 1)) %>%
          group_by(geo_value, ahead, quantile) %>%
          summarize(delta = -lr * mean(grad) * first(scale), .groups = "drop")
        offsets <- offsets %>%
          left_join(step, by = c("geo_value", "ahead", "quantile")) %>%
          mutate(offset = offset + coalesce(delta, 0)) %>%
          select(-delta)
      }
    }
    prev_cut <- cut
  }
  cli::cli_progress_done()

  bind_rows(issued) %>%
    mutate(value_calibrated = pmax(value + offset, 0)) %>%
    # Per-quantile offsets can cross; restore monotonicity like the forecasters do.
    group_by(geo_value, forecast_date, target_end_date) %>%
    arrange(quantile, .by_group = TRUE) %>%
    mutate(value_calibrated = sort(value_calibrated)) %>%
    ungroup()
}


# =============================== SCORING ==================================

#' Score the raw and calibrated forecasts against finalized truth.
#'
#' Uses the pipeline's own score_forecasts() (hubEvals wis / ae_median /
#' coverage) so these numbers are comparable to the prod scoring notebooks.
ch_compare <- function(calibrated, inputs) {
  latest <- inputs$truth_final %>%
    select(geo_value, time_value = target_end_date, value = truth)
  score_one <- function(df, label) {
    score_forecasts(latest, df %>% mutate(forecaster = label), CH_TARGET)
  }
  raw_scores <- score_one(calibrated %>% select(-value_calibrated), "raw")
  cal_scores <- score_one(
    calibrated %>% select(-value) %>% rename(value = value_calibrated), "calibrated"
  )
  bind_rows(raw_scores, cal_scores)
}

#' Summarize a ch_compare() result: mean skill by ahead, raw vs calibrated.
ch_summarize <- function(scores) {
  scores %>%
    group_by(forecaster, ahead) %>%
    summarize(
      across(c(wis, ae_median, interval_coverage_50, interval_coverage_90), mean),
      .groups = "drop"
    ) %>%
    arrange(ahead, forecaster)
}


# ============================ VERIFICATION ================================

#' Forecast dates for which the store has a cached prod forecast target.
ch_cached_prod_dates <- function(store = CH_STORE) {
  prefix <- glue::glue("forecast_nhsn_{CH_FORECASTER_ID}_")
  list.files(file.path(store, "objects"), pattern = glue::glue("^{prefix}")) %>%
    stringr::str_extract("\\d{4}\\.\\d{2}\\.\\d{2}") %>%
    unique() %>%
    stats::na.omit() %>%
    as.Date(format = "%Y.%m.%d") %>%
    sort()
}

#' Assert that the harness's forecast path reproduces the prod DAG bit-for-bit.
#'
#' The whole premise of iterating outside targets is that these two code paths
#' agree, so this is the load-bearing check: run after any change to CH_SPEC or
#' to the shared runner.
#'
#' It cannot compare against ch_backfill()'s normal output, because the harness
#' deliberately differs from prod in two ways -- it drops CH_DROP_GEOS from the
#' archive before training (prod filters them from the output, so prod's pooled
#' seasonal window still sees them) and it applies no data substitutions. Both
#' change the forecast legitimately. So this runs its own backfill in a faithful
#' configuration (all geos in the archive, prod's substitutions, output-level geo
#' exclusion) over a couple of the cached dates and requires exact equality
#' there. What it proves is that the plumbing is identical; the configured
#' deviations are then the only difference that remains.
ch_verify_against_store <- function(store = CH_STORE, n_dates = 2, tolerance = 0) {
  dates <- ch_cached_prod_dates(store)
  if (length(dates) == 0) {
    cli::cli_abort("No cached prod targets for {.field {CH_FORECASTER_ID}} in {.path {store}}.")
  }
  dates <- utils::tail(dates, n_dates)
  cli::cli_alert_info("Verifying against prod on {.val {format(dates)}}")

  faithful <- ch_inputs(store = store, drop_geos = character(0))
  # Prod's aheads are -1:3; overlap on the non-negative ones is enough, and the
  # harness does not model ahead -1 (see CH_AHEADS).
  mine <- ch_backfill(
    faithful,
    aheads = intersect(CH_AHEADS, 0:3),
    schedule = ch_schedule(through = max(dates)) %>% filter(forecast_date %in% dates),
    substitutions = here::here("scripts/covid_data_substitutions.csv"),
    excluded_geos = CH_DROP_GEOS,
    cache_dir = file.path(CH_CACHE_DIR, "verify")
  )

  withr::local_envvar(TAR_PROJECT = store)
  cached <- purrr::map(dates, function(d) {
    nm <- glue::glue("forecast_nhsn_{CH_FORECASTER_ID}_{format(d, '%Y.%m.%d')}")
    targets::tar_read_raw(nm, store = store)
  }) %>%
    bind_rows() %>%
    select(geo_value, forecast_date, target_end_date, quantile, cached = value)

  joined <- mine %>%
    inner_join(cached, by = c("geo_value", "forecast_date", "target_end_date", "quantile"))
  if (nrow(joined) == 0) {
    cli::cli_abort("No overlap between the harness forecasts and the cached prod targets.")
  }
  worst <- max(abs(joined$value - joined$cached))
  if (worst > tolerance) {
    cli::cli_abort(
      "Harness diverges from the prod DAG: max abs diff {.val {worst}} over
       {.val {nrow(joined)}} overlapping rows. CH_SPEC is out of sync with
       scripts/covid_hosp_prod.R, or the shared runner changed."
    )
  }
  cli::cli_alert_success(
    "Harness matches the prod DAG exactly on {.val {nrow(joined)}} rows
     across {.val {length(dates)}} date{?s}."
  )
  invisible(joined)
}
