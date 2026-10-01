suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("archive_to_revision_predictors uses as-of vintages for lags", {
  mk <- function(tv, ver, val) {
    tibble(geo_value = "ca", time_value = as.Date(tv), source = "nhsn", version = as.Date(ver), value = val)
  }
  # value at 2023-01-07 is first reported as 10, then revised up to 12
  archive <- bind_rows(
    mk("2023-01-07", "2023-01-07", 10),
    mk("2023-01-07", "2023-01-14", 12),
    mk("2023-01-14", "2023-01-14", 20),
    mk("2023-01-14", "2023-01-21", 22),
    mk("2023-01-21", "2023-01-21", 30),
    mk("2023-01-28", "2023-01-28", 40)
  ) %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(archive, lags = c(0, 7), cols = "value", ahead = 7)

  # lag 0 is the value observed for that week as of that week
  expect_equal(design$value_lag_0, c(10, 20, 30, 40))
  # lag 7 at 2023-01-14 is the *revised* 01-07 value (12), the vintage as of 01-14
  row_0114 <- design %>% filter(time_value == as.Date("2023-01-14"))
  expect_equal(row_0114$value_lag_7, 12)
  # first row has no lag-7 predictor available
  expect_true(is.na(design$value_lag_7[[1]]))
})

test_that("archive_to_revision_predictors target is the finalized value ahead", {
  mk <- function(tv, ver, val) {
    tibble(geo_value = "ca", time_value = as.Date(tv), source = "nhsn", version = as.Date(ver), value = val)
  }
  archive <- bind_rows(
    mk("2023-01-07", "2023-01-07", 10),
    mk("2023-01-14", "2023-01-14", 20),
    mk("2023-01-14", "2023-01-21", 22), # 01-14 gets revised to 22
    mk("2023-01-21", "2023-01-21", 30)
  ) %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(archive, lags = 0, cols = "value", ahead = 7)

  # target for 01-07 is the finalized 01-14 value (22), not the 20 first reported
  expect_equal(design %>% filter(time_value == as.Date("2023-01-07")) %>% pull(value_target), 22)
  # target for 01-21 is unknown (nothing at 01-28)
  expect_true(is.na(design %>% filter(time_value == as.Date("2023-01-21")) %>% pull(value_target)))
})

test_that("archive_to_revision_predictors supports per-column lags and multiple geos", {
  mk <- function(geo, tv, val, aux) {
    tibble(geo_value = geo, time_value = as.Date(tv), source = "nhsn", version = as.Date(tv), value = val, aux = aux)
  }
  archive <- bind_rows(
    mk("ca", "2023-01-07", 10, 1), mk("ca", "2023-01-14", 20, 2), mk("ca", "2023-01-21", 30, 3),
    mk("hi", "2023-01-07", 40, 4), mk("hi", "2023-01-14", 50, 5), mk("hi", "2023-01-21", 60, 6)
  ) %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(
    archive,
    lags = list(c(0, 7), 0),
    cols = c("value", "aux"),
    ahead = NULL
  )

  expect_setequal(
    names(design),
    c("version", "time_value", "geo_value", "source", "value_lag_0", "value_lag_7", "aux_lag_0")
  )
  expect_setequal(unique(design$geo_value), c("ca", "hi"))
  # aux only requested at lag 0
  hi_0121 <- design %>% filter(geo_value == "hi", time_value == as.Date("2023-01-21"))
  expect_equal(hi_0121$value_lag_7, 50)
  expect_equal(hi_0121$aux_lag_0, 6)
})

test_that("archive_to_revision_predictors anchors on the target, not the widest column", {
  # single vintage; an exogenous signal leads the outcome by a week (its 01-21
  # value is reported while the outcome's is not). The anchor must be the
  # outcome's latest reported week (01-14), not the max time_value overall.
  archive <- bind_rows(
    tibble(geo_value = "ca", time_value = as.Date("2023-01-07"), value = 10, aux = 100),
    tibble(geo_value = "ca", time_value = as.Date("2023-01-14"), value = 20, aux = 200),
    tibble(geo_value = "ca", time_value = as.Date("2023-01-21"), value = NA_real_, aux = 300)
  ) %>%
    mutate(source = "nhsn", version = as.Date("2023-01-28")) %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(archive, lags = c(0, 7), cols = c("value", "aux"), ahead = NULL)

  expect_equal(nrow(design), 1)
  expect_equal(design$time_value, as.Date("2023-01-14")) # anchor is the outcome's latest, not 01-21
  expect_equal(design$value_lag_0, 20) # would be NA (value at 01-21) under the old max()-anchor bug
  expect_equal(design$value_lag_7, 10)
  expect_equal(design$aux_lag_0, 200) # exogenous read at the anchor, not its own latest (300)
})

test_that("archive_to_revision_predictors respects reporting latency across versions", {
  # each week is first published one week after it occurs, so the anchor at any
  # version is the previous week -- the version's own week is not yet reported.
  archive <- bind_rows(
    tibble(time_value = as.Date("2023-01-07"), version = as.Date("2023-01-14"), value = 10),
    tibble(time_value = as.Date("2023-01-14"), version = as.Date("2023-01-21"), value = 20),
    tibble(time_value = as.Date("2023-01-21"), version = as.Date("2023-01-28"), value = 30)
  ) %>%
    mutate(geo_value = "ca", source = "nhsn") %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(archive, lags = 0, cols = "value", ahead = NULL) %>%
    arrange(version)

  # anchor trails the version by the one-week reporting latency
  expect_equal(design$version, as.Date(c("2023-01-14", "2023-01-21", "2023-01-28")))
  expect_equal(design$time_value, as.Date(c("2023-01-07", "2023-01-14", "2023-01-21")))
  expect_equal(design$value_lag_0, c(10, 20, 30)) # non-NA despite version != anchor week
  expect_true(all(design$version > design$time_value))
})

test_that("archive_to_revision_predictors anchors each geo at its own latency", {
  # one vintage; ca reports through 01-21, hi only through 01-14.
  archive <- bind_rows(
    tibble(geo_value = "ca", time_value = as.Date(c("2023-01-07", "2023-01-14", "2023-01-21")), value = c(10, 20, 30)),
    tibble(geo_value = "hi", time_value = as.Date(c("2023-01-07", "2023-01-14")), value = c(40, 50))
  ) %>%
    mutate(source = "nhsn", version = as.Date("2023-01-28")) %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(archive, lags = 0, cols = "value", ahead = NULL)

  expect_equal(design %>% filter(geo_value == "ca") %>% pull(time_value), as.Date("2023-01-21"))
  expect_equal(design %>% filter(geo_value == "ca") %>% pull(value_lag_0), 30)
  expect_equal(design %>% filter(geo_value == "hi") %>% pull(time_value), as.Date("2023-01-14"))
  expect_equal(design %>% filter(geo_value == "hi") %>% pull(value_lag_0), 50)
})

test_that("archive_to_revision_predictors anchors correctly across a backfill block", {
  # 2023-01-21 first-reports three weeks at once (a backfill: all share first_v),
  # 2023-01-28 adds a fourth. The anchor must be the latest backfilled week, not
  # an arbitrary member of the tied block -- the failure mode of the rolling-join
  # rewrite that the no-latency fixtures above do not exercise.
  mk <- function(tv, ver, val) {
    tibble(geo_value = "ca", time_value = as.Date(tv), source = "nhsn", version = as.Date(ver), value = val)
  }
  archive <- bind_rows(
    mk("2023-01-07", "2023-01-21", 10),
    mk("2023-01-14", "2023-01-21", 20),
    mk("2023-01-21", "2023-01-21", 30),
    mk("2023-01-28", "2023-01-28", 40)
  ) %>%
    as_epi_archive(other_keys = "source")

  design <- archive_to_revision_predictors(archive, lags = c(0, 7), cols = "value", ahead = NULL) %>%
    arrange(version)

  expect_equal(design$version, as.Date(c("2023-01-21", "2023-01-28")))
  expect_equal(design$time_value, as.Date(c("2023-01-21", "2023-01-28"))) # latest of the block, not 01-07/01-14
  expect_equal(design$value_lag_0, c(30, 40))
  expect_equal(design$value_lag_7, c(20, 30))
})

#' A stable geo's late-arriving report on a multi-source archive: `nhsn`
#' reports the same value at every version (never a genuine outlier), and so
#' does an unrelated, wildly different-magnitude `flusurv` row at the same
#' `geo_value` + `time_value`s. If the join drops `source`, `nhsn`'s late
#' report gets compared against `flusurv`'s value instead of its own -- a huge
#' spurious deviation -- and gets falsely flagged. `nhsn_late_version` is
#' earlier than `flusurv_late_version` so the contamination is directional and
#' unambiguous: with source correctly included, nothing here should ever be
#' flagged.
mk_cross_source_archive_dt <- function(include_source) {
  weeks <- seq(as.Date("2023-01-07"), as.Date("2023-02-11"), by = 7)
  nhsn_late_version <- as.Date("2023-02-20")
  flusurv_late_version <- as.Date("2023-03-01")
  mk_source <- function(source_name, stable_value, late_version) {
    rows <- bind_rows(
      tibble(geo_value = "ca", time_value = weeks, version = weeks, value = stable_value),
      tibble(geo_value = "ca", time_value = weeks, version = late_version, value = stable_value)
    )
    if (include_source) rows$source <- source_name
    rows
  }
  bind_rows(
    mk_source("nhsn", stable_value = 10, late_version = nhsn_late_version),
    if (include_source) mk_source("flusurv", stable_value = 1000, late_version = flusurv_late_version)
  )
}

test_that("flag_revision_outlier_versions doesn't flag a genuinely stable series (single-source archive)", {
  archive_dt <- mk_cross_source_archive_dt(include_source = FALSE)
  flagged <- flag_revision_outlier_versions(archive_dt, "value", n_weeks = 1, threshold = 0.2, min_value = 1, min_obs = 3)
  expect_equal(names(flagged), c("geo_value", "version"))
  expect_equal(nrow(flagged), 0)
})

test_that("flag_revision_outlier_versions does not cross-contaminate sources sharing a (geo, time_value)", {
  # Regression guard for a real bug found via a remote `make explore-flu`
  # crash (see devlog): the join used to drop `source` before joining, so a
  # multi-source archive (flu's joined_archive_data has nhsn/flusurv/nssp/...
  # sharing geo_value + time_value) compared one source's values against
  # another's -- e.g. flusurv's stable ~1000 against nhsn's stable ~10 -- and
  # over-flagged nhsn's perfectly normal late report as an outlier purely from
  # the contamination, collapsing training data to near-nothing for the whole
  # revision_aware family. Confirmed against the pre-fix implementation: it
  # flags `(ca, 2023-02-20)` here; the fix flags nothing.
  archive_dt <- mk_cross_source_archive_dt(include_source = TRUE)
  flagged <- flag_revision_outlier_versions(archive_dt, "value", n_weeks = 1, threshold = 0.2, min_value = 1, min_obs = 3)
  expect_equal(names(flagged), c("geo_value", "version", "source"))
  expect_equal(nrow(flagged), 0)
})

test_that("rolling-join design matches a naive epix_as_of reference (golden)", {
  # Independent, obviously-correct reference: loop epix_as_of per version, anchor
  # on the outcome's latest reported week, read lags off that vintage. Locks the
  # fast rolling-join implementation to epix semantics on an archive that mixes
  # revisions, reporting latency, a backfill block, and two geos.
  naive_ref <- function(archive, lags, cols, target_col) {
    grp <- setdiff(key_colnames(archive), c("time_value", "version"))
    purrr::map_dfr(sort(unique(archive$DT$version)), function(v) {
      snap <- suppressMessages(epix_as_of(archive, v)) %>% as_tibble()
      snap %>%
        group_by(across(all_of(grp))) %>%
        group_modify(function(g, key) {
          reported <- g %>% filter(!is.na(.data[[target_col]]))
          if (nrow(reported) == 0) {
            return(tibble())
          }
          anchor <- max(reported$time_value)
          row <- tibble(version = v, time_value = anchor)
          for (col in cols) {
            for (lag in lags) {
              val <- g %>% filter(time_value == anchor - lag) %>% pull(col)
              row[[paste0(col, "_lag_", lag)]] <- if (length(val)) val[[1]] else NA_real_
            }
          }
          row
        }) %>%
        ungroup()
    })
  }

  mk <- function(geo, tv, ver, value, aux) {
    tibble(geo_value = geo, time_value = as.Date(tv), source = "nhsn", version = as.Date(ver), value = value, aux = aux)
  }
  archive <- bind_rows(
    # ca: 01-07 revised across versions, 01-21 backfills two weeks at once, latency
    mk("ca", "2023-01-07", "2023-01-07", 10, 1), mk("ca", "2023-01-07", "2023-01-14", 12, 1),
    mk("ca", "2023-01-14", "2023-01-21", 20, 2), mk("ca", "2023-01-21", "2023-01-21", 30, 3),
    mk("ca", "2023-01-28", "2023-01-28", 40, 4),
    # hi: more latent, aux leads the outcome
    mk("hi", "2023-01-07", "2023-01-14", 50, 5), mk("hi", "2023-01-14", "2023-01-21", 60, 6),
    mk("hi", "2023-01-21", "2023-01-28", NA, 7)
  ) %>%
    as_epi_archive(other_keys = "source")

  fast <- archive_to_revision_predictors(archive, lags = c(0, 7), cols = c("value", "aux"), ahead = NULL)
  ref <- naive_ref(archive, lags = c(0, 7), cols = c("value", "aux"), target_col = "value")

  key_cols <- c("geo_value", "source", "version", "time_value")
  expect_equal(
    fast %>% arrange(across(all_of(key_cols))) %>% select(all_of(names(ref))),
    ref %>% arrange(across(all_of(key_cols))),
    ignore_attr = TRUE
  )
})

test_that("revision_predictor_design cache round-trips", {
  mk <- function(tv, ver, val) {
    tibble(geo_value = "ca", time_value = as.Date(tv), source = "nhsn", version = as.Date(ver), value = val)
  }
  archive <- bind_rows(
    mk("2023-01-07", "2023-01-07", 10),
    mk("2023-01-14", "2023-01-14", 20),
    mk("2023-01-21", "2023-01-21", 30)
  ) %>%
    as_epi_archive(other_keys = "source")

  withr::with_tempdir({
    uncached <- revision_predictor_design(archive, lags = c(0, 7), cols = "value")
    fresh <- revision_predictor_design(archive, lags = c(0, 7), cols = "value", cache_key = "test")
    cached <- revision_predictor_design(archive, lags = c(0, 7), cols = "value", cache_key = "test")
    expect_equal(fresh, uncached)
    expect_equal(cached, uncached)
    expect_true(file.exists(list.files("cache/revision_cache", full.names = TRUE)[[1]]))
  })
})

#' Synthetic multi-geo archive at per-100k scale (values ~3-18, like flu's
#' `hhs`), covering "us" plus several real state abbreviations of very
#' different population. No revisions (version == time_value): this fixture
#' is for output-scale sanity checks, not revision handling, which is covered
#' elsewhere in this file.
mk_per100k_sanity_archive <- function() {
  geos <- tibble(
    geo_value = c("us", "ca", "tx", "ny", "wy", "vt", "ak", "hi", "ri", "nd"),
    base = c(8, 6, 5, 5.5, 4, 4.5, 3, 3.5, 4.2, 3.8)
  )
  weeks <- seq(as.Date("2023-01-07"), as.Date("2023-07-01"), by = 7)
  geos %>%
    tidyr::expand_grid(time_value = weeks) %>%
    mutate(
      week_idx = as.numeric(time_value - min(time_value)) / 7,
      value = pmax(0.1, base + week_idx * 0.4 + rnorm(dplyr::n(), sd = 1)),
      source = "nhsn",
      version = time_value
    ) %>%
    select(geo_value, time_value, source, version, value) %>%
    as_epi_archive(other_keys = "source")
}

#' Every predicted quantile should stay within `mult`x of that geo's own most
#' recent observed value -- a loose bound that catches a geo-specific scale
#' blowup without requiring the forecast to be numerically tight.
expect_forecast_within_scale <- function(res, archive, mult, label) {
  last_observed <- archive$DT %>%
    as_tibble() %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value, value)
  joined <- res %>% left_join(last_observed, by = "geo_value", suffix = c("", "_last"))
  expect_true(all(joined$value < mult * joined$value_last), info = label)
}

test_that("scaled_pop_seasonal_revision gives sane forecasts, whitened and unwhitened", {
  set.seed(1)
  archive <- mk_per100k_sanity_archive()
  quantreg_fn <- epipredict::quantile_reg(method = "fn")
  run <- function(scale_method, center_method, nonlin_method) {
    scaled_pop_seasonal_revision(
      archive,
      outcome = "value",
      primary_source = "nhsn",
      ahead = 28,
      lags = c(0, 7),
      pop_scaling = FALSE,
      scale_method = scale_method,
      center_method = center_method,
      nonlin_method = nonlin_method,
      use_seasonal_window = FALSE,
      trainer = quantreg_fn,
      finalization_coverage = 0.8
    )
  }

  unwhitened <- run("none", "none", "none")
  whitened <- run("quantile", "median", "quart_root")

  check_sane <- function(res, label) {
    expect_true(nrow(res) > 0, info = label)
    expect_setequal(unique(res$geo_value), unique(archive$DT$geo_value))
    expect_true(all(is.finite(res$value)), info = label)
    expect_true(all(res$value >= 0), info = label)
    # 10x is generous -- both variants actually stay under ~1.3x here -- but
    # tight enough that the pop_scaling=TRUE regression below (~80x for "us")
    # trips it easily.
    expect_forecast_within_scale(res, archive, mult = 10, label)
  }

  check_sane(unwhitened, "unwhitened")
  check_sane(whitened, "whitened")
})

test_that("pop_scaling=TRUE double-normalizes an already-per-100k outcome and blows up high-population geos", {
  # Characterization test for the bug fixed by setting pop_scaling=FALSE on
  # flu's revision_aware* families (R/targets/flu_forecaster_config.R): `hhs`
  # is already per-100k at archive build, so pop_scaling=TRUE re-divides an
  # already-small (~3-18) value by each geo's population before pooling all
  # geos into one quantile_reg fit. That round-trip is an identity per row,
  # but pooling geos of wildly different real population (us vs a state) on
  # the resulting near-zero values destabilizes the shared fit and blows up
  # the high-population geo's upper quantiles by 1-2 orders of magnitude once
  # rescaled back to counts -- while small states stay comparatively sane.
  # If this test starts failing because pop_scaling=TRUE stopped blowing up,
  # that's good news: revisit whether flu's config should go back to TRUE.
  set.seed(1)
  archive <- mk_per100k_sanity_archive()
  quantreg_fn <- epipredict::quantile_reg(method = "fn")
  res <- scaled_pop_seasonal_revision(
    archive,
    outcome = "value",
    primary_source = "nhsn",
    ahead = 28,
    lags = c(0, 7),
    pop_scaling = TRUE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = FALSE,
    trainer = quantreg_fn,
    finalization_coverage = 0.8
  )

  last_observed <- archive$DT %>%
    as_tibble() %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value, value)
  ratios <- res %>%
    summarize(max_value = max(value), .by = geo_value) %>%
    left_join(last_observed, by = "geo_value") %>%
    mutate(ratio = max_value / value)

  # "us" (by far the largest population here) blows way past small states.
  expect_gt(ratios$ratio[ratios$geo_value == "us"], 20)
  expect_lt(max(ratios$ratio[ratios$geo_value != "us"]), 20)
})

#' Synthetic archive with a genuine, consistent revision pattern: each week's
#' value is first reported at `prelim_frac` of its eventual finalized value,
#' then corrected to the full finalized value `revision_lag_days` later.
#' Revised rows for weeks too recent to have been corrected yet (relative to
#' the last `time_value`) are dropped, so `versions_end` lands on the most
#' recent prelim-only report, like a real as-of snapshot.
#' Optionally adds an `aux` exogenous column, fully known at report time (no
#' revision), correlated with the finalized value.
#' Returns the archive plus `truth`, the noise-free finalized-value trend, so
#' tests can compare forecasts against the true future value directly instead
#' of just checking they're in some plausible range.
mk_revision_archive <- function(
  geo_bases = c(ca = 20, tx = 15),
  weeks = seq(as.Date("2023-01-07"), as.Date("2023-06-24"), by = 7),
  prelim_frac = 0.4,
  revision_lag_days = 14,
  include_aux = FALSE
) {
  geos <- tibble(geo_value = names(geo_bases), base = unname(geo_bases))
  # `truth` extends a few weeks past the archive's own `weeks` so a positive
  # `ahead` forecast target still has a known finalized value to compare
  # against, even though the archive itself has no data out there yet.
  truth_weeks <- c(weeks, max(weeks) + 7 * seq_len(4))
  grid <- geos %>%
    tidyr::expand_grid(time_value = truth_weeks) %>%
    mutate(
      week_idx = as.numeric(time_value - min(weeks)) / 7,
      final_value = pmax(1, base + week_idx * 0.6 + rnorm(dplyr::n(), sd = 0.5))
    )
  prelim <- grid %>% filter(time_value %in% weeks) %>% mutate(version = time_value, value = final_value * prelim_frac)
  revised <- grid %>%
    filter(time_value + revision_lag_days <= max(weeks)) %>%
    mutate(version = time_value + revision_lag_days, value = final_value)
  archive_rows <- bind_rows(prelim, revised) %>%
    mutate(source = "nhsn") %>%
    select(geo_value, time_value, source, version, value)
  if (include_aux) {
    # Correlated with, but not an exact multiple of, final_value -- an exact
    # linear multiple makes the design matrix collinear with the outcome's own
    # lags and quantreg_fn warns about a singular fit.
    archive_rows <- archive_rows %>%
      left_join(grid %>% select(geo_value, time_value, final_value), by = c("geo_value", "time_value")) %>%
      mutate(aux = final_value * 1.2 + rnorm(dplyr::n(), sd = 1)) %>%
      select(-final_value)
  }
  list(
    archive = archive_rows %>% as_epi_archive(other_keys = "source"),
    truth = grid %>% select(geo_value, time_value, final_value)
  )
}

test_that("scaled_pop_seasonal_revision corrects a systematically under-reported recent week", {
  # The whole point of revision-awareness: every training row's lag_0 shares
  # the same "just published, before correction" bias as the forecast row's
  # lag_0, so the model should learn to scale it up toward the finalized
  # target rather than parroting the raw under-reported value.
  set.seed(2)
  fixture <- mk_revision_archive()
  quantreg_fn <- epipredict::quantile_reg(method = "fn")
  res <- scaled_pop_seasonal_revision(
    fixture$archive,
    outcome = "value",
    primary_source = "nhsn",
    ahead = 7,
    lags = c(0, 7),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = FALSE,
    trainer = quantreg_fn,
    finalization_coverage = 0.8
  )

  last_raw <- fixture$archive$DT %>%
    as_tibble() %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value, raw_value = value)
  target_end_date <- unique(res$target_end_date)
  expect_length(target_end_date, 1)
  true_target <- fixture$truth %>%
    filter(time_value == target_end_date) %>%
    select(geo_value, final_value)

  medians <- res %>%
    filter(quantile == 0.5) %>%
    left_join(last_raw, by = "geo_value") %>%
    left_join(true_target, by = "geo_value")

  expect_true(all(is.finite(medians$value)))
  # Corrects upward, past the raw under-reported reading.
  expect_true(all(medians$value > 1.15 * medians$raw_value))
  # Lands in the right ballpark of the true finalized future value (loose --
  # this is a noisy short synthetic series, not a precision check).
  expect_true(all(medians$value > 0.4 * medians$final_value))
  expect_true(all(medians$value < 2.5 * medians$final_value))
})

test_that("scaled_pop_seasonal_revision handles an exogenous extra_sources column", {
  # Regression guard for the anchor-on-exogenous-lead bug (fixed during covid
  # wiring, see devlog/memory): joining an always-fresh exogenous column
  # shouldn't break the outcome's own revision-aware lags or NA out the
  # design.
  set.seed(3)
  fixture <- mk_revision_archive(include_aux = TRUE)
  quantreg_fn <- epipredict::quantile_reg(method = "fn")
  res <- scaled_pop_seasonal_revision(
    fixture$archive,
    outcome = "value",
    extra_sources = "aux",
    primary_source = "nhsn",
    ahead = 7,
    lags = c(0, 7),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = FALSE,
    trainer = quantreg_fn,
    finalization_coverage = 0.8
  )

  expect_true(nrow(res) > 0)
  expect_setequal(unique(res$geo_value), unique(fixture$truth$geo_value))
  expect_true(all(is.finite(res$value)))
  expect_true(all(res$value >= 0))
})

test_that("scaled_pop_seasonal_revision supports a negative ahead (nowcast of the still-revising anchor week)", {
  # Prod's flu/covid ensembles run this forecaster at ahead=-1 week to nowcast
  # the most recent, not-yet-finalized week (git: "add revision aware -1
  # ahead"). Confirms the negative-ahead path targets the anchor week itself
  # and still corrects its under-reported raw value upward.
  set.seed(4)
  fixture <- mk_revision_archive()
  quantreg_fn <- epipredict::quantile_reg(method = "fn")
  res <- scaled_pop_seasonal_revision(
    fixture$archive,
    outcome = "value",
    primary_source = "nhsn",
    ahead = -7,
    lags = c(0, 7),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = FALSE,
    trainer = quantreg_fn,
    finalization_coverage = 0.8
  )

  versions_end <- fixture$archive$versions_end
  expect_true(nrow(res) > 0)
  # Nowcasting the anchor week itself, one week before the nominal forecast
  # date, not further back and not forward. (forecast_date itself is a
  # separate Wed-labeling convention, not checked here.)
  expect_true(all(res$target_end_date == versions_end - 7))

  last_raw <- fixture$archive$DT %>%
    as_tibble() %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value, raw_value = value)
  medians <- res %>% filter(quantile == 0.5) %>% left_join(last_raw, by = "geo_value")
  expect_true(all(is.finite(medians$value)))
  expect_true(all(medians$value > 1.15 * medians$raw_value))
})

test_that("scaled_pop_seasonal_revision handles a seasonal window across multiple winters", {
  # The seasonal-window path (used by the base revision_aware and
  # *_beds_seasonal families) restricts training to a backward/forward window
  # around the forecast anchor's season week, across all prior seasons. Needs
  # >1 winter of data to exercise the "across seasons" part at all.
  set.seed(5)
  geos <- tibble(geo_value = c("ca", "tx"), base = c(20, 15))
  # Two winters: a trough in summer, a peak in Jan, each year.
  weeks <- seq(as.Date("2022-07-03"), as.Date("2024-01-01"), by = 7)
  archive <- geos %>%
    tidyr::expand_grid(time_value = weeks) %>%
    mutate(
      season_frac = (as.numeric(format(time_value, "%j")) %% 365) / 365,
      seasonal_shape = cos(2 * pi * (season_frac - 0.02)),
      value = pmax(1, base * (1 + seasonal_shape) + rnorm(dplyr::n(), sd = 1)),
      source = "nhsn",
      version = time_value
    ) %>%
    select(geo_value, time_value, source, version, value) %>%
    as_epi_archive(other_keys = "source")

  quantreg_fn <- epipredict::quantile_reg(method = "fn")
  res <- scaled_pop_seasonal_revision(
    archive,
    outcome = "value",
    primary_source = "nhsn",
    ahead = 7,
    lags = c(0, 7),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = TRUE,
    seasonal_backward_window = 5 * 7,
    seasonal_forward_window = 3 * 7,
    trainer = quantreg_fn,
    finalization_coverage = 0.8
  )

  last_observed <- archive$DT %>%
    as_tibble() %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value, last_value = value)

  expect_true(nrow(res) > 0)
  expect_setequal(unique(res$geo_value), geos$geo_value)
  expect_true(all(is.finite(res$value)))
  expect_true(all(res$value >= 0))
  # Forecast anchor (late Dec/Jan) sits near the seasonal peak in both
  # synthetic winters, so the training window should have plenty of similarly
  # high-value rows -- output shouldn't collapse toward the summer trough.
  joined <- res %>% filter(quantile == 0.5) %>% left_join(last_observed, by = "geo_value")
  expect_true(all(joined$value > 0.4 * joined$last_value | joined$value > 10))
})

test_that("scaled_pop_seasonal_revision keeps a prior year's seasonal window across a reporting gap on its anchor week", {
  # Mirrors the Oct 2025 NHSN shutdown: the prior year's copy of the forecast
  # anchor week (and its neighbours) was never reported. The current year's
  # window alone is too small to train on, so the forecast needs the rest of
  # the prior year's window to survive the gap.
  set.seed(6)
  geos <- tibble(geo_value = c("ca", "tx", "ny"), base = c(20, 15, 18))
  weeks <- seq(as.Date("2024-07-03"), as.Date("2025-09-24"), by = 7)
  prior_anchor <- max(weeks) - 364
  archive <- geos %>%
    tidyr::expand_grid(time_value = weeks[abs(weeks - prior_anchor) > 7]) %>%
    mutate(
      value = pmax(1, base + rnorm(dplyr::n(), sd = 1)),
      source = "nhsn",
      version = time_value
    ) %>%
    select(geo_value, time_value, source, version, value) %>%
    as_epi_archive(other_keys = "source")

  res <- scaled_pop_seasonal_revision(
    archive,
    outcome = "value",
    primary_source = "nhsn",
    ahead = 7,
    lags = c(0, 7),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    use_seasonal_window = TRUE,
    seasonal_backward_window = 5 * 7,
    seasonal_forward_window = 3 * 7,
    trainer = epipredict::quantile_reg(method = "fn"),
    finalization_coverage = 0.8
  )

  expect_true(nrow(res) > 0)
  expect_setequal(unique(res$geo_value), geos$geo_value)
})

run_nowcast <- function(archive) {
  scaled_pop_seasonal_revision(
    archive,
    outcome = "value", primary_source = "nhsn", ahead = -7, lags = c(0, 7),
    pop_scaling = FALSE, scale_method = "none", center_method = "none", nonlin_method = "none",
    use_seasonal_window = FALSE, trainer = epipredict::quantile_reg(method = "fn"),
    finalization_coverage = 0.8
  )
}

test_that("scaled_pop_seasonal_revision gives no h−1 forecast when that week isn't reported", {
  set.seed(5)
  archive <- mk_revision_archive()$archive
  # Two weeks after the last report: the h−1 week is still missing (a reporting gap).
  attr(archive, "forecast_date") <- archive$versions_end + 14
  expect_warning(res <- run_nowcast(archive), "isn't reported yet")
  expect_equal(nrow(res), 0)
})

test_that("scaled_pop_seasonal_revision gives no forecast when the target is off the weekly grid", {
  set.seed(5)
  archive <- mk_revision_archive()$archive
  attr(archive, "forecast_date") <- archive$versions_end + 8
  expect_warning(res <- run_nowcast(archive), "whole number of weeks")
  expect_equal(nrow(res), 0)
})

test_that("scaled_pop_seasonal_revision drops a geo whose latest week is behind the others", {
  set.seed(6)
  fixture <- mk_revision_archive()
  last_week <- max(fixture$archive$DT$time_value)
  archive <- fixture$archive$DT %>%
    as_tibble() %>%
    filter(!(geo_value == "tx" & time_value == last_week)) %>%
    as_epi_archive(other_keys = "source", versions_end = fixture$archive$versions_end)
  attr(archive, "forecast_date") <- last_week + 7
  expect_warning(res <- run_nowcast(archive), "no forecast for tx")
  expect_equal(unique(res$geo_value), "ca")
  expect_true(all(res$target_end_date == last_week))
})
