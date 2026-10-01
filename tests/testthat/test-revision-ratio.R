suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# Weekly archive where each week is first reported at `first_frac` of its final
# value one week later, then revised to final the week after that.
make_revising_archive <- function(geos, finals, first_frac, n_weeks = 20) {
  weeks <- as.Date("2025-01-01") + 7 * (0:(n_weeks - 1))
  purrr::map2_dfr(geos, finals, function(geo, fin) {
    bind_rows(
      tibble(geo_value = geo, time_value = weeks, version = weeks + 7, value = fin * first_frac[[geo]]),
      tibble(geo_value = geo, time_value = weeks, version = weeks + 14, value = fin)
    )
  }) %>%
    as_epi_archive(compactify = FALSE)
}

test_that("revision_ratio_nowcast scales the reported value by each geo's revision ratio", {
  arch <- make_revising_archive(c("ca", "tx"), c(100, 50), first_frac = list(ca = 0.8, tx = 0.5))
  # As of the last week's first report: that week is at 80 (ca) and 25 (tx).
  arch <- epix_truncate_versions_after(arch, max(arch$DT$time_value) + 7)
  out <- revision_ratio_nowcast(arch, "value", ahead = -7, window_weeks = 8, settled_days = 21, quantile_levels = c(0.1, 0.5, 0.9))
  med <- out %>% filter(quantile == 0.5)
  expect_equal(med$value[med$geo_value == "ca"], 100)
  expect_equal(med$value[med$geo_value == "tx"], 50)
  expect_equal(unique(out$target_end_date), max(arch$DT$time_value))
})

test_that("revision_ratio_nowcast returns a null forecast for unreported targets", {
  arch <- make_revising_archive("ca", 100, first_frac = list(ca = 0.8))
  expect_equal(nrow(revision_ratio_nowcast(arch, "value", ahead = 0)), 0)
})
