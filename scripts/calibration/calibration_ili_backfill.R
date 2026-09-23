# Pseudo-hub history for calibration burn-in: run the flu prod `windowed_seasonal`
# forecaster (scaled_pop_seasonal, seasonal_method = "window") on the ILI+
# history, season by season, and write the result in the FluSight hub schema
# that calibrate_hub_forecasts() consumes. ILI+ is the same augmentation source
# the prod forecasters train on (see scripts/flu_hosp_prod.R `nhsn_prod_archive`),
# so this is "what would our seasonal forecaster have said" for 2010-2024, on the
# ILI+ percent scale rather than NHSN counts. See notes/CALIBRATION.md, "ILI+
# burn-in", for how the scale mismatch is handled downstream.
#
# ILI+ is faux-versioned (version = time_value), so every snapshot is finalized
# data cut at the forecast date; there is no revision realism here. To mimic
# the hub's reporting lag (at a Saturday reference date NHSN is complete only
# through the previous Saturday), each snapshot is truncated one week before the
# forecast date, so hub horizon 0 is one week past the last observation.
#
# Usage, from the repo root (~30 min cold, cached in cache/calibration/):
#   distrobox enter rocker -- Rscript scripts/calibration_ili_backfill.R
# Then in R: ili <- ili_read_pseudo_hub()  # list(forecasts, truth)

suppressPackageStartupMessages(source(here::here("R/load_all.R")))

ILI_CACHE_DIR <- here::here("cache/calibration")
ILI_FORECASTS_FILE <- file.path(ILI_CACHE_DIR, "ili_pseudo_hub_forecasts.parquet")
ILI_TRUTH_FILE <- file.path(ILI_CACHE_DIR, "ili_pseudo_hub_truth.parquet")
ILI_HORIZONS <- -1:3
# Pipeline convention: MMWR-week Sunday label + 3 = Wednesday, like NHSN's
# Saturday - 3 in nhsn_prod_archive.
ILI_TIME_VALUE_ADJUST <- 3L
# The windowed_seasonal row of scripts/flu_hosp_prod.R's grid, minus the prod
# spec columns run_forecaster() owns (ahead_multiplier 7, target_date_shift 3,
# sort_quantiles). primary_source tells the forecaster which source is the
# outcome's; here the archive has only ILI+.
ILI_PARAMS <- list(
  outcome = "value",
  trainer = epipredict::quantile_reg(),
  seasonal_method = "window",
  pop_scaling = FALSE,
  lags = list(c(0, 7)),
  primary_source = "ILI+"
)

#' State-level ILI+ as a Wednesday-labelled, faux-versioned epi_archive.
ili_archive <- function() {
  suppressWarnings(gen_ili_data())$DT %>%
    filter(agg_level == "state", !is.na(hhs), hhs > 1e-4) %>%
    mutate(
      time_value = time_value + ILI_TIME_VALUE_ADJUST,
      version = time_value,
      source = "ILI+"
    ) %>%
    select(geo_value, time_value, version, value = hhs, source, agg_level, season, season_week) %>%
    add_pop_and_density() %>%
    as_epi_archive(other_keys = "source", compactify = TRUE)
}

#' Wednesdays from October through May of every season the archive covers.
ili_schedule <- function(archive) {
  seasons <- archive$DT %>%
    distinct(season) %>%
    mutate(start_year = as.integer(substr(season, 1, 4))) %>%
    arrange(start_year)
  purrr::map(seasons$start_year, function(y) {
    from <- as.Date(sprintf("%d-10-01", y))
    from <- from + ((3 - as.integer(format(from, "%u"))) %% 7)
    seq.Date(from, as.Date(sprintf("%d-05-31", y + 1L)), by = 7L)
  }) %>%
    purrr::reduce(c) %>%
    keep(~ .x + ILI_TIME_VALUE_ADJUST <= archive$versions_end)
}

#' One forecast date, all horizons, in hub long format.
ili_forecast_one <- function(archive, fd, geo_map) {
  snapshot <- make_forecast_snapshot(archive, forecast_date = fd, generation_date = fd, as_of_policy = "asof") %>%
    filter(time_value <= fd - 7L)
  purrr::map(ILI_HORIZONS, function(h) {
    run_forecaster(
      snapshot = snapshot, forecaster = scaled_pop_seasonal, aheads = h * 7L,
      params = ILI_PARAMS, id = "windowed_seasonal_ili",
      target_date_shift = 3L, sort_quantiles = TRUE
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
      value
    )
}

ili_backfill <- function(refresh = FALSE) {
  if (file.exists(ILI_FORECASTS_FILE) && !refresh) {
    cli::cli_alert_success("Pseudo-hub cache present at {.path {ILI_FORECASTS_FILE}}; pass refresh = TRUE to rebuild.")
    return(invisible(ili_read_pseudo_hub()))
  }
  archive <- ili_archive()
  geo_map <- get_population_data() %>%
    distinct(state_id, .keep_all = TRUE) %>%
    transmute(geo_value = state_id, location = state_code)
  schedule <- ili_schedule(archive)
  cli::cli_alert_info("Backfilling {length(schedule)} ILI+ forecast dates x {length(ILI_HORIZONS)} horizons")

  failures <- list()
  forecasts <- purrr::map(schedule, .progress = "ILI+ backfill", function(fd) {
    tryCatch(ili_forecast_one(archive, fd, geo_map), error = function(e) {
      failures[[as.character(fd)]] <<- conditionMessage(e)
      NULL
    })
  }) %>%
    bind_rows()
  if (length(failures) > 0) {
    cli::cli_warn("{length(failures)} date{?s} failed: {.val {names(failures)}}")
  }
  # Hub-schema truth: finalized ILI+ on Saturday labels.
  truth <- archive$DT %>%
    inner_join(geo_map, by = "geo_value") %>%
    transmute(location, target_end_date = time_value + 3L, truth = value) %>%
    arrange(location, target_end_date)

  dir.create(ILI_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  nanoparquet::write_parquet(forecasts, ILI_FORECASTS_FILE)
  nanoparquet::write_parquet(truth, ILI_TRUTH_FILE)
  cli::cli_alert_success(
    "{nrow(forecasts)} forecast rows over {n_distinct(forecasts$reference_date)} rounds -> {.path {ILI_FORECASTS_FILE}}"
  )
  invisible(list(forecasts = forecasts, truth = truth, failures = failures))
}

#' Read the cached pseudo-hub tables in hub_read_forecasts() shape (forecasts) and NHSN truth shape.
ili_read_pseudo_hub <- function() {
  list(
    forecasts = nanoparquet::read_parquet(ILI_FORECASTS_FILE) %>%
      mutate(across(c(reference_date, target_end_date), as.Date), horizon = as.integer(horizon)),
    truth = nanoparquet::read_parquet(ILI_TRUTH_FILE) %>% mutate(target_end_date = as.Date(target_end_date))
  )
}

if (sys.nframe() == 0L) {
  ili_backfill(refresh = "--refresh" %in% commandArgs(trailingOnly = TRUE))
}
