# Pseudo-hub history for calibration: replay flu prod's `windowed_seasonal`
# forecaster (scaled_pop_seasonal, seasonal_method = "window", trained on the
# nhsn + ILI+ + flusurv prod archive) over the NHSN seasons and write the result
# in the FluSight hub schema that calibrate_hub_forecasts() consumes. Together
# with scripts/calibration_ili_backfill.R this gives one forecaster's history
# from 2011 to today, so calibration can be studied without the submitted
# ensemble's changing composition in the way.
#
# The replay is the prod cell: nhsn_prod_archive from the flu_hosp_evaluation
# store, make_forecast_snapshot() + run_forecaster() with the grid row's spec
# values (copied from scripts/flu_hosp_prod.R; keep in sync), prod's seed and
# data substitutions. NHSN vintages begin 2024-11-19, so 2024-25 and 2025-26 are
# honest as-of replays; 2023-24 is replayed with as_of_policy = "cheating"
# (finalized data cut at the forecast date) and is marked as such in the output.
#
# Usage, from the repo root (~10 min cold, cached in cache/calibration/):
#   distrobox enter rocker -- Rscript scripts/calibration_ws_backfill.R
# Then in R: ws <- ws_read_pseudo_hub()  # forecasts; truth is hub_read_truth()

suppressPackageStartupMessages(source(here::here("R/load_all.R")))

WS_CACHE_DIR <- here::here("cache/calibration")
WS_FORECASTS_FILE <- file.path(WS_CACHE_DIR, "ws_pseudo_hub_forecasts.parquet")
WS_HORIZONS <- -1:3
WS_ID <- "windowed_seasonal"
WS_STORE <- "flu_hosp_evaluation"
WS_SUBSTITUTIONS <- here::here("pipelines/flu_data_substitutions.csv")
# scripts/flu_hosp_prod.R: windowed_seasonal grid row, modeling params only.
WS_PARAMS <- list(
  outcome = "value",
  trainer = epipredict::quantile_reg(),
  seasonal_method = "window",
  pop_scaling = FALSE,
  lags = list(c(0, 7)),
  keys_to_ignore = list(list(c("source"), c("flusurv", "ILI+")))
)

#' Wednesday forecast dates with the generation-date delays prod's evaluation
#' mode uses, plus the vintage-free 2023-24 season under the cheating policy.
ws_schedule <- function(versions_end) {
  delays <- c("2024-11-20" = "2024-11-21", "2024-12-25" = "2024-12-26", "2025-01-01" = "2025-01-02", "2025-12-24" = "2025-12-29")
  through <- as.Date(versions_end)
  through <- through - ((as.integer(format(through, "%u")) - 3) %% 7)
  live <- tibble(forecast_date = seq.Date(as.Date("2024-11-20"), through, by = 7L), as_of_policy = "asof") %>%
    mutate(generation_date = forecast_date)
  hit <- match(as.character(live$forecast_date), names(delays))
  live$generation_date[!is.na(hit)] <- as.Date(delays[hit[!is.na(hit)]])
  cheat <- tibble(
    forecast_date = seq.Date(as.Date("2023-10-11"), as.Date("2024-05-29"), by = 7L),
    as_of_policy = "cheating"
  ) %>% mutate(generation_date = forecast_date)
  bind_rows(cheat, live)
}

ws_geo_map <- function() {
  gm <- get_population_data() %>%
    distinct(state_id, .keep_all = TRUE) %>%
    transmute(geo_value = state_id, location = state_code)
  if (!"us" %in% gm$geo_value) gm <- bind_rows(gm, tibble(geo_value = "us", location = "US"))
  gm
}

ws_forecast_one <- function(archive, fd, gd, policy, geo_map) {
  snapshot <- make_forecast_snapshot(
    archive, forecast_date = fd, generation_date = gd, as_of_policy = policy,
    substitutions = if (policy == "asof") WS_SUBSTITUTIONS else NULL
  )
  purrr::map(WS_HORIZONS, function(h) {
    set.seed(targets::tar_seed_create(paste(WS_ID, "nhsn", as.character(fd), h, sep = "/")))
    run_forecaster(
      snapshot = snapshot, forecaster = scaled_pop_seasonal, aheads = h * 7L,
      params = WS_PARAMS, id = WS_ID, target_date_shift = 3L, sort_quantiles = TRUE
    ) %>%
      mutate(horizon = h)
  }) %>%
    bind_rows() %>%
    inner_join(geo_map, by = "geo_value") %>%
    transmute(
      reference_date = fd + 3L,
      horizon,
      target_end_date,
      location,
      level_index = hub_level_index(quantile),
      level = quantile,
      value,
      as_of_policy = policy
    )
}

ws_backfill <- function(refresh = FALSE) {
  if (file.exists(WS_FORECASTS_FILE) && !refresh) {
    cli::cli_alert_success("Pseudo-hub cache present at {.path {WS_FORECASTS_FILE}}; pass refresh = TRUE to rebuild.")
    return(invisible(ws_read_pseudo_hub()))
  }
  withr::local_envvar(TAR_PROJECT = WS_STORE)
  archive <- targets::tar_read(nhsn_prod_archive)
  schedule <- ws_schedule(archive$versions_end)
  geo_map <- ws_geo_map()
  cli::cli_alert_info("Backfilling {nrow(schedule)} forecast dates x {length(WS_HORIZONS)} horizons of {WS_ID}")
  failures <- list()
  forecasts <- purrr::pmap(schedule, .progress = "windowed_seasonal backfill", function(forecast_date, as_of_policy, generation_date) {
    tryCatch(ws_forecast_one(archive, forecast_date, generation_date, as_of_policy, geo_map), error = function(e) {
      failures[[as.character(forecast_date)]] <<- conditionMessage(e)
      NULL
    })
  }) %>%
    bind_rows()
  if (length(failures) > 0) {
    cli::cli_warn("{length(failures)} date{?s} failed: {.val {names(failures)}}")
    print(failures)
  }
  dir.create(WS_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  nanoparquet::write_parquet(forecasts, WS_FORECASTS_FILE)
  cli::cli_alert_success("{nrow(forecasts)} forecast rows over {n_distinct(forecasts$reference_date)} rounds -> {.path {WS_FORECASTS_FILE}}")
  invisible(forecasts)
}

#' Read the cached pseudo-hub forecasts in hub_read_forecasts() shape (plus `as_of_policy`).
ws_read_pseudo_hub <- function() {
  nanoparquet::read_parquet(WS_FORECASTS_FILE) %>%
    mutate(across(c(reference_date, target_end_date), as.Date), horizon = as.integer(horizon))
}

if (sys.nframe() == 0L) {
  ws_backfill(refresh = "--refresh" %in% commandArgs(trailingOnly = TRUE))
}
