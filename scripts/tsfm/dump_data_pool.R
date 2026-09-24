# Dump the flu prod targets store into parquet files that Python (TimesFM,
# DuckDB) can read. Everything a foundation-model backtest needs lives in one
# directory: versioned archives for honest as-of snapshots, finalized truth,
# the forecast schedule, and the R forecasts to compare against.
#
# Run from the repo root (see scripts/tsfm/README.md):
#   Rscript scripts/tsfm/dump_data_pool.R [store] [out_dir]
suppressPackageStartupMessages(source("R/load_all.R"))

args <- commandArgs(trailingOnly = TRUE)
store <- if (length(args) >= 1) args[[1]] else "flu_hosp_prod"
out_dir <- if (length(args) >= 2) args[[2]] else "cache/tsfm"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_pq <- function(df, name) {
  path <- file.path(out_dir, paste0(name, ".parquet"))
  arrow::write_parquet(as_tibble(df), path)
  cli::cli_alert_success("{name}: {nrow(df)} rows -> {path}")
}

# One long versioned table: geo x source x time_value (Wednesday-labeled) x
# version. nhsn rows come from the canonical prod archive (with the ILI+ and
# flusurv history rows it folds in); nssp is the raw pct-ED-visits archive.
nhsn_archive <- tar_read_raw("nhsn_prod_archive", store = store)
nssp_archive <- tar_read_raw("nssp_archive_data", store = store)
archive <- bind_rows(
  nhsn_archive$DT %>% select(geo_value, source, time_value, version, value),
  nssp_archive$DT %>% transmute(geo_value, source = "nssp", time_value, version, value = nssp)
) %>%
  arrange(geo_value, source, time_value, version)
write_pq(archive, "archive")

# Finalized truth on Saturday (target_end_date) labels, as scoring uses it.
nhsn_latest <- tar_read_raw("nhsn_latest_data", store = store)
nssp_latest <- tar_read_raw("nssp_latest_data", store = store)
truth <- bind_rows(
  nhsn_latest %>% as_tibble() %>% transmute(geo_value, source = "nhsn", target_end_date = time_value, value),
  nssp_latest %>% as_tibble() %>% transmute(geo_value, source = "nssp", target_end_date = time_value + 3, value = nssp)
) %>%
  filter(!is.na(value))
write_pq(truth, "truth")

# Weekly forecast schedule: nominal Wednesday forecast date and the day the
# forecast actually ran (the as-of). Mirrors the evaluation schedule in
# scripts/flu_hosp_prod.R; the store's latest ensemble bounds the end.
forecast_dates <- tar_meta(store = store) %>%
  filter(str_detect(name, "^ensemble_mixture_\\d{4}\\.\\d{2}\\.\\d{2}$")) %>%
  transmute(forecast_date = as.Date(str_remove(name, "ensemble_mixture_"), format = "%Y.%m.%d")) %>%
  arrange(forecast_date) %>%
  pull(forecast_date)
schedule <- tibble(forecast_date = forecast_dates) %>%
  mutate(
    generation_date = case_when(
      forecast_date == as.Date("2024-11-20") ~ as.Date("2024-11-21"),
      forecast_date == as.Date("2024-12-25") ~ as.Date("2024-12-26"),
      forecast_date == as.Date("2025-01-01") ~ as.Date("2025-01-02"),
      forecast_date == as.Date("2025-12-24") ~ as.Date("2025-12-29"),
      TRUE ~ forecast_date
    )
  )
write_pq(schedule, "forecast_schedule")

# The R forecasts produced in real time by the weekly prod runs, nhsn and nssp
# targets stacked with a `target` column.
r_forecasts <- bind_rows(
  tar_read_raw("local_forecasts_and_ensembles_nhsn", store = store) %>% mutate(target = "nhsn"),
  tar_read_raw("local_forecasts_and_ensembles_nssp", store = store) %>% mutate(target = "nssp")
) %>%
  select(target, forecaster, geo_value, forecast_date, target_end_date, quantile, value)
write_pq(r_forecasts, "r_forecasts")

write_pq(
  get_population_data() %>% select(geo_value = state_id, state_code, population),
  "population"
)
