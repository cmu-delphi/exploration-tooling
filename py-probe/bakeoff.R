#!/usr/bin/env Rscript
# Bake-off: run the REAL R forecasters (windowed_seasonal, climate_linear) on the
# same cast-api data the Python probes use, and write their quantile forecasts to
# CSV so we can diff them against the Python ports.
#
# We deliberately do NOT source R/load_all.R: its imports.R pulls the whole
# pipeline stack (targets, aws.s3, scoringutils, grf, ...), most of which isn't
# installed in this renv library. Instead we load the core modelling libs and
# source just the forecaster files. Run from the repo root:
#   distrobox enter rocker -- Rscript py-probe/bakeoff.R

suppressPackageStartupMessages({
  for (p in c(
    "dplyr", "tidyr", "purrr", "tibble", "lubridate", "magrittr", "rlang", "cli",
    "glue", "stringr", "data.table", "epipredict", "epiprocess", "epidatr",
    "epidatasets", "recipes", "parsnip", "quantreg", "slider", "MMWRweek", "zeallot"
  )) {
    if (requireNamespace(p, quietly = TRUE)) suppressWarnings(library(p, character.only = TRUE))
    else cli::cli_alert_warning("library missing: {p}")
  }
})

`%nin%` <- function(x, y) !(x %in% y)

# Source the forecaster web (everything under R/ except the dep loader). Wrap
# each in tryCatch so a file that needs an uninstalled package (plotting,
# scoring, targets helpers) doesn't abort the ones we need.
repo <- here::here()
files <- list.files(file.path(repo, "R"), recursive = TRUE, full.names = TRUE, pattern = "\\.R$")
files <- files[!basename(files) %in% c("imports.R", "load_all.R")]
for (f in files) {
  tryCatch(source(f), error = function(e) cli::cli_alert_warning("skip {basename(f)}: {conditionMessage(e)}"))
}

# ---- Data: the canonical snapshot dumped by dump_inputs.py -----------------
# Both sides read the SAME csv so the bake-off measures forecaster logic, not
# data-fetch differences. (Run `uv run py-probe/dump_inputs.py` first.)
nhsn_raw <- readr::read_csv(file.path(here::here(), "py-probe", "nhsn_input.csv"), show_col_types = FALSE) %>%
  transmute(geo_value, time_value = as.Date(time_value), value)
as_of <- max(nhsn_raw$time_value)
cli::cli_alert_info("NHSN flu adm: {nrow(nhsn_raw)} rows, {n_distinct(nhsn_raw$geo_value)} geos, as_of={as_of}")

# Build the epi_df the forecasters expect: source key + as_of metadata.
make_epi_df <- function(df, as_of) {
  df %>%
    mutate(source = "nhsn") %>%
    as_epi_df(other_keys = "source", as_of = as_of) %>%
    add_season_info()
}
nhsn <- make_epi_df(nhsn_raw, as_of)

aheads <- 0:3

# ---- Forecaster 1: windowed_seasonal (nhsn only) ---------------------------
# adjust_latency defaults to production's "extend_lags"; override to "none" to
# match the Python port, which doesn't implement latency adjustment.
adjust_latency <- Sys.getenv("ADJUST_LATENCY", "extend_lags")
ws <- map(aheads, function(a) {
  scaled_pop_seasonal(
    nhsn, outcome = "value", ahead = a * 7,
    trainer = quantile_reg(), seasonal_method = "window",
    pop_scaling = FALSE, lags = c(0, 7), adjust_latency = adjust_latency
  ) %>%
    # NB: production wrapper adds +3 (Sat->Tue) to compensate a -3 train shift we
    # don't apply here; we skip both so dates match the Python port (Saturdays).
    mutate(ahead = a)
}) %>%
  list_rbind() %>%
  mutate(forecaster = "windowed_seasonal")

# ---- Forecaster 2: climate_linear ensemble ---------------------------------
components <- map(aheads, function(a) {
  cb <- nhsn %>% climatological_model(a) %>% mutate(forecaster = "climate_base")
  cg <- nhsn %>% climatological_model(a, geo_agg = TRUE) %>% mutate(forecaster = "climate_geo_agged")
  ln <- nhsn %>% forecaster_baseline_linear(a, population_scale = FALSE) %>% mutate(forecaster = "linear")
  bind_rows(cb, cg, ln)
}) %>% list_rbind()

ensemble <- components %>%
  ensemble_climate_linear(aheads) %>%
  mutate(forecaster = "climate_linear")

# ---- Write tidy CSVs for the Python side to diff against -------------------
outdir <- file.path(repo, "py-probe")
std <- function(df) {
  df %>%
    transmute(forecaster, geo_value, forecast_date = as.Date(forecast_date),
              target_end_date = as.Date(target_end_date),
              quantile = round(quantile, 3), value) %>%
    arrange(forecaster, geo_value, target_end_date, quantile)
}
readr::write_csv(std(ws), file.path(outdir, "r_windowed_seasonal.csv"))
readr::write_csv(std(bind_rows(components, ensemble)), file.path(outdir, "r_climate_linear.csv"))
cli::cli_alert_success("wrote r_windowed_seasonal.csv, r_climate_linear.csv")
