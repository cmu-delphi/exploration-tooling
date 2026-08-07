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
