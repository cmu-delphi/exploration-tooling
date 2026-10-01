# Build a weekly, versioned 2023-24 hospital-admissions archive from Delphi's
# daily `hhs` source, for use as a burn-in season ahead of the NHSN-era replay,
# and validate its finalized values against NHSN's finalized 2023-24 values.
#
# Usage: Rscript scripts/one_offs/hhs_2023_24_archive.R [out_dir] [start] [versions_from]
# `start` (default 20230701) is the first admission day fetched; the 2023-24
# burn-in uses 20200801 so a 2023-24 snapshot sees earlier seasons' history.
# Issues before `versions_from` (default 2023-07-01) collapse into one version
# at that date, which keeps the per-version loop small.
# Writes hhs_2023_24_weekly_archive.rds (epi_archive DT, both diseases),
# hhs_2023_24_weekly_archive.parquet and validation CSVs into out_dir.
#
# Decisions (see notes/ROADMAP.md item 1b):
# - Week: the hhs `time_value` is the admission day (healthdata.gov `date` - 1).
#   NHSN's week ending Saturday S equals the sum over admission days S-7..S-1
#   (Sat..Fri, i.e. collection dates Sun..Sat); other alignments match far worse.
#   Output time_value is the canonical Wednesday label, S - 3.
# - Versions: every daily issue is a version. A week's value as of v is the sum of
#   its 7 daily values as of v; a week is only emitted once all 7 days exist.
# - US: the hhs `nation` signal, which equals the sum of all states incl. as/pr/vi.

suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else file.path(tempdir(), "hhs_2023_24")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
raw_cache <- file.path(out_dir, "hhs_daily_raw.rds")

signals <- c(flu = "confirmed_admissions_influenza_1d", covid = "confirmed_admissions_covid_1d")
start <- if (length(args) >= 2) args[[2]] else "20230701"
versions_from <- as.Date(if (length(args) >= 3) args[[3]] else "2023-07-01")
time_range <- epidatr::epirange(as.integer(start), 20240531)

# ---- Fetch daily data with all issues ----
if (file.exists(raw_cache)) {
  daily <- readRDS(raw_cache)
} else {
  daily <- tidyr::expand_grid(disease = names(signals), geo_type = c("state", "nation")) %>%
    purrr::pmap(\(disease, geo_type) {
      epidatr::pub_covidcast(
        "hhs", signals[[disease]], geo_type, "day", "*",
        time_values = time_range, issues = "*"
      ) %>%
        transmute(disease = disease, geo_value, time_value, version = issue, value)
    }) %>%
    bind_rows()
  saveRDS(daily, raw_cache)
}
# Flu has missing days before mid-2021 (reporting was optional); a week with one
# becomes NA, as in NHSN. From 2023-07 on there must be none.
stopifnot(!anyNA(daily$value[daily$time_value >= as.Date("2023-07-01")]))

# Saturday week-ending for an hhs admission day: the Saturday after it.
week_ending_saturday <- function(admission_day) {
  d <- admission_day + 1
  d + (6 - lubridate::wday(d, week_start = 7) + 1) %% 7
}
stopifnot(week_ending_saturday(as.Date("2023-11-25")) == as.Date("2023-12-02")) # Sat -> next Sat
stopifnot(week_ending_saturday(as.Date("2023-12-01")) == as.Date("2023-12-02")) # Fri -> next day

daily <- daily %>% mutate(week_end = week_ending_saturday(time_value))

# ---- Daily issues -> weekly versions ----
versions <- sort(unique(pmax(daily$version, versions_from)))
weekly_long <- purrr::map(versions, \(v) {
  daily %>%
    filter(version <= v) %>%
    group_by(disease, geo_value, time_value) %>%
    slice_max(version, n = 1, with_ties = FALSE) %>%
    group_by(disease, geo_value, week_end) %>%
    summarize(value = sum(value), n_days = n(), .groups = "drop") %>%
    filter(n_days == 7L) %>%
    mutate(version = v)
}) %>%
  bind_rows()

# Weeks that were incomplete at some version, for the record.
incomplete_seen <- purrr::map(versions, \(v) {
  daily %>%
    filter(version <= v) %>%
    distinct(disease, geo_value, week_end, time_value) %>%
    count(disease, geo_value, week_end) %>%
    filter(n < 7L) %>%
    mutate(version = v)
}) %>%
  bind_rows()

# Canonical shape: Saturday season info, then the Wednesday label.
archive_dt <- weekly_long %>%
  rename(time_value = week_end) %>%
  select(disease, geo_value, time_value, version, value) %>%
  add_season_info() %>%
  mutate(time_value = time_value - 3L, source = "hhs")

archives <- purrr::map(c(flu = "flu", covid = "covid"), \(d) {
  archive_dt %>%
    filter(disease == d) %>%
    select(-disease) %>%
    as_epi_archive(other_keys = "source", compactify = TRUE)
})
saveRDS(purrr::map(archives, \(a) a$DT), file.path(out_dir, "hhs_2023_24_weekly_archive.rds"))
nanoparquet::write_parquet(
  purrr::imap(archives, \(a, d) as_tibble(a$DT) %>% mutate(disease = d)) %>% bind_rows(),
  file.path(out_dir, "hhs_2023_24_weekly_archive.parquet")
)

# ---- Validation against NHSN ----
nhsn_final <- purrr::map(c("flu", "covid"), \(d) {
  get_nhsn_data_archive(d) %>%
    epix_as_of_current() %>%
    as_tibble() %>%
    transmute(disease = d, geo_value = ifelse(geo_value == "usa", "us", geo_value), time_value = time_value - 3L, nhsn = value)
}) %>%
  bind_rows()

hhs_final <- purrr::imap(archives, \(a, d) epix_as_of_current(a) %>% as_tibble() %>% mutate(disease = d)) %>%
  bind_rows() %>%
  select(disease, geo_value, time_value, hhs = value)

cmp <- inner_join(hhs_final, nhsn_final, by = c("disease", "geo_value", "time_value")) %>%
  mutate(diff = hhs - nhsn, ratio = hhs / nhsn)
in_season <- \(x) filter(x, time_value >= as.Date("2023-10-04"), time_value <= as.Date("2024-04-24"))

overall <- cmp %>%
  in_season() %>%
  group_by(disease) %>%
  summarize(
    geo_weeks = n(),
    exact = mean(diff == 0),
    sum_ratio = sum(hhs) / sum(nhsn),
    median_ratio = median(ratio, na.rm = TRUE),
    median_abs_diff = median(abs(diff)),
    .groups = "drop"
  )
per_geo <- cmp %>%
  in_season() %>%
  group_by(disease, geo_value) %>%
  summarize(
    weeks = n(),
    exact = mean(diff == 0),
    sum_ratio = sum(hhs) / sum(nhsn),
    median_ratio = median(ratio, na.rm = TRUE),
    max_abs_diff = max(abs(diff)),
    nhsn_total = sum(nhsn),
    .groups = "drop"
  )
missing_geos <- anti_join(
  nhsn_final %>% in_season() %>% distinct(disease, geo_value),
  hhs_final %>% distinct(disease, geo_value),
  by = c("disease", "geo_value")
)

# ---- Revisions ----
rev <- purrr::imap(archives, \(a, d) as_tibble(a$DT) %>% mutate(disease = d)) %>%
  bind_rows() %>%
  in_season() %>%
  group_by(disease, geo_value, time_value) %>%
  arrange(version, .by_group = TRUE) %>%
  summarize(
    n_versions = n(),
    first_version = first(version),
    first_value = first(value),
    final_value = last(value),
    .groups = "drop"
  ) %>%
  mutate(
    lag_days = as.integer(first_version - (time_value + 3L)),
    rel_revision = (final_value - first_value) / pmax(final_value, 1)
  )
revision_summary <- rev %>%
  group_by(disease, us = geo_value == "us") %>%
  summarize(
    geo_weeks = n(),
    median_versions = median(n_versions),
    max_versions = max(n_versions),
    median_lag_days = median(lag_days),
    median_abs_rel_rev = median(abs(rel_revision)),
    p90_abs_rel_rev = quantile(abs(rel_revision), 0.9),
    .groups = "drop"
  )

readr::write_csv(cmp, file.path(out_dir, "validation_weekly.csv"))
readr::write_csv(per_geo, file.path(out_dir, "validation_per_geo.csv"))
readr::write_csv(rev, file.path(out_dir, "revisions.csv"))

print(knitr::kable(overall, digits = 3))
print(knitr::kable(per_geo %>% arrange(disease, abs(log(sum_ratio)) * -1) %>% group_by(disease) %>% slice_head(n = 8), digits = 3))
print(knitr::kable(revision_summary, digits = 3))
cat("NHSN geos with no hhs data:\n")
print(missing_geos)
cat("Week-versions withheld as incomplete:", nrow(distinct(incomplete_seen, disease, geo_value, week_end, version)), "\n")
cat("Output:", out_dir, "\n")
