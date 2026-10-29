# Data targets and helpers for the sleepcycle EDA project (pipelines/sleepcycle_eda.R).

fetch_epidata_archive <- function(source, signals) {
  bind_rows(lapply(signals, function(signal) {
    epidata_archive(source = source, signals = signal, geo_type = "state", geo_values = "*") |>
      mutate(signal = signal)
  }))
}

# Reusable by any project that wants these raw archives.
sleepcycle_data_targets <- function() {
  list(
    tar_target(sc_archive, fetch_epidata_archive(
      "sleepcycle", c("pct_cough_2_plus_7dav", "pct_cough_10_plus_7dav")
    ), format = "parquet"),
    tar_target(nhsn_archive, fetch_epidata_archive(
      "nhsn", c("confirmed_admissions_flu_ew", "confirmed_admissions_covid_ew")
    ), format = "parquet"),
    tar_target(nssp_archive, fetch_epidata_archive(
      "nssp", c("pct_ed_visits_influenza", "pct_ed_visits_covid")
    ), format = "parquet"),
    tar_target(state_pop, epidatasets::state_census |>
      transmute(geo_value = tolower(abbr), pop) |> filter(!geo_value %in% c("us")))
  )
}

latest_versions <- function(archive) {
  archive |>
    group_by(signal, geo_value, reference_time) |>
    slice_max(report_time, n = 1, with_ties = FALSE) |>
    ungroup()
}

# One row per state-week (weeks end Saturday, like NHSN/NSSP). Sleepcycle's 7-day
# average on the Saturday is the weekly value.
make_weekly_panel <- function(sc_archive, nhsn_archive, nssp_archive, state_pop) {
  sc <- latest_versions(sc_archive) |>
    filter(wday(reference_time) == 7) |>
    mutate(ci_width = ci_upper - ci_lower) |>
    select(geo_value, week = reference_time, signal, value, ci_width) |>
    pivot_wider(names_from = signal, values_from = c(value, ci_width)) |>
    rename(
      cough2 = value_pct_cough_2_plus_7dav, cough10 = value_pct_cough_10_plus_7dav,
      ci_width2 = ci_width_pct_cough_2_plus_7dav
    ) |>
    select(-ci_width_pct_cough_10_plus_7dav)
  targets <- bind_rows(latest_versions(nhsn_archive), latest_versions(nssp_archive)) |>
    transmute(geo_value, week = reference_time, signal = recode(signal,
      confirmed_admissions_flu_ew = "nhsn_flu", confirmed_admissions_covid_ew = "nhsn_covid",
      pct_ed_visits_influenza = "nssp_flu", pct_ed_visits_covid = "nssp_covid"
    ), value) |>
    pivot_wider(names_from = signal, values_from = value)
  sc |>
    left_join(targets, by = c("geo_value", "week")) |>
    left_join(state_pop, by = "geo_value") |>
    arrange(geo_value, week)
}

# National series restricted to states with cough data that week: cough and NSSP
# are population-weighted means, NHSN admissions are sums over the same states.
make_national <- function(panel) {
  panel |>
    group_by(week) |>
    summarise(
      n_states = n(),
      cough2 = weighted.mean(cough2, pop, na.rm = TRUE),
      cough10 = weighted.mean(cough10, pop, na.rm = TRUE),
      nhsn_flu = sum(nhsn_flu, na.rm = TRUE), nhsn_covid = sum(nhsn_covid, na.rm = TRUE),
      nssp_flu = weighted.mean(nssp_flu, pop, na.rm = TRUE),
      nssp_covid = weighted.mean(nssp_covid, pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(c(nssp_flu, nssp_covid), \(x) if_else(is.nan(x), NA_real_, x)))
}

# Week-over-week log change; eps keeps zeros finite.
dlog <- function(x) {
  eps <- 0.05 * median(x[x > 0], na.rm = TRUE)
  c(NA, diff(log(x + eps)))
}

# Correlation of cough at week t with target at week t + lag, for lags -6..6.
# Positive lag = cough leads the target.
lag_cor <- function(x, y, lags = -6:6, min_n = 20) {
  map_dfr(lags, \(k) {
    yk <- if (k >= 0) lead(y, k) else lag(y, -k)
    ok <- !is.na(x) & !is.na(yk)
    tibble(lag = k, n = sum(ok), cor = if (sum(ok) >= min_n) cor(x[ok], yk[ok]) else NA_real_)
  })
}

national_lag_cors <- function(national, target_cols = c("nhsn_flu", "nhsn_covid", "nssp_flu", "nssp_covid")) {
  map_dfr(target_cols, \(tc) bind_rows(
    lag_cor(national$cough2, national[[tc]]) |> mutate(transform = "level"),
    lag_cor(dlog(national$cough2), dlog(national[[tc]])) |> mutate(transform = "wow log change")
  ) |> mutate(target = tc))
}

state_lag_cors <- function(panel, target_cols = c("nhsn_flu", "nhsn_covid", "nssp_flu", "nssp_covid")) {
  panel |>
    group_by(geo_value) |>
    group_modify(\(d, key) map_dfr(target_cols, \(tc) bind_rows(
      lag_cor(d$cough2, d[[tc]]) |> mutate(transform = "level"),
      lag_cor(dlog(d$cough2), dlog(d[[tc]])) |> mutate(transform = "wow log change")
    ) |> mutate(target = tc))) |>
    ungroup()
}

# Mean relative change of a report vs the first report for the same value, by
# lag (days between report and reference date).
sc_revision_summary <- function(sc_archive) {
  sc_archive |>
    filter(signal == "pct_cough_2_plus_7dav") |>
    group_by(geo_value, reference_time) |>
    arrange(report_time, .by_group = TRUE) |>
    filter(n() > 1) |>
    mutate(rel_change = (value - first(value)) / pmax(abs(first(value)), 0.1),
           lag_days = as.numeric(as.Date(report_time) - reference_time)) |>
    ungroup() |>
    group_by(geo_value, reference_time) |>
    slice(-1) |>
    ungroup()
}

render_quarto_report <- function(qmd, output_dir, ...) {
  # `...` carries the targets the notebook reads, so targets reruns the render when they change.
  status <- system2("quarto", c("render", qmd, "--output-dir", "."), stdout = TRUE, stderr = TRUE)
  html <- sub("\\.qmd$", ".html", qmd)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(html, file.path(output_dir, basename(html)), overwrite = TRUE)
  file.path(output_dir, basename(html))
}
