# Shared helpers for the production forecasting pipelines (covid_hosp_prod, flu_hosp_prod).
# Functions prefixed build_* return target lists and depend on g_* globals defined in
# the calling script.

g_baseline_forecaster <- function(epi_data, ahead, extra_data, ...) {
  if (ahead < 3) {
    return(tibble(
      geo_value = character(), forecast_date = Date(),
      target_end_date = Date(), quantile_value = numeric(), value = numeric()
    ))
  }
  real_forecast_date <- attributes(epi_data)$metadata$as_of
  last_data <- epi_data$time_value %>% max()
  latency_weeks <- as.integer(real_forecast_date - last_data) / 7
  fcst <- epi_data %>%
    cdc_baseline_forecaster(
      "value",
      args_list = cdc_baseline_args_list(aheads = 1:(3 + latency_weeks))
    ) %>%
    `$`(predictions) %>%
    pivot_quantiles_longer(.pred_distn) %>%
    select(
      geo_value, forecast_date,
      target_end_date = target_date,
      value = .pred_distn_value,
      quantile = .pred_distn_quantile_level
    ) %>%
    mutate(
      forecast_date = floor_date(forecast_date, "weeks", week_start = 7) + 3,
      target_end_date = floor_date(target_end_date, "weeks", week_start = 7) + 3
    ) %>%
    mutate(
      ahead = as.integer(target_end_date - forecast_date),
      forecast_date = real_forecast_date
    )
  fcst
}

# Returns a tar_map covering all forecast dates, fetching and scoring external
# forecasts from S3. Depends on g_s3_prefix, g_disease, nhsn_latest_data,
# nssp_latest_data targets.
build_external_forecast_targets <- function() {
  tar_map(
    values = tibble(
      forecast_date_int = seq(as.Date("2024-11-23"), round_date(Sys.Date() - 3, "week", 6), by = "week")
    ) %>%
      mutate(
        forecast_date_chr = as.character(as.Date(forecast_date_int)),
        filename = paste0(g_s3_prefix, "/", forecast_date_chr, "/", g_disease, "_forecasts.parquet"),
      ),
    names = "forecast_date_chr",
    tar_change(
      name = external_forecasts,
      change = get_s3_object_last_modified(filename, "forecasting-team-data"),
      command = get_external_forecasts(filename)
    ),
    tar_target(
      name = score_external_nhsn_forecasts,
      command = score_forecasts(
        nhsn_latest_data, external_forecasts,
        paste0("wk inc ", g_disease, " hosp")
      )
    ),
    tar_target(
      name = score_external_nssp_forecasts,
      command = score_forecasts(
        nssp_latest_data %>% mutate(value = nssp),
        external_forecasts,
        paste0("wk inc ", g_disease, " prop ed visits")
      )
    )
  )
}

# Combines per-forecaster forecast_nhsn and forecast_nssp tar_map outputs into
# single forecast_nhsn_full / forecast_nssp_full targets.
build_combined_forecast_targets <- function(forecast_targets) {
  list(
    tar_combine(
      name = forecast_nhsn_full,
      forecast_targets[["forecast_nhsn"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_combine(
      name = forecast_nssp_full,
      forecast_targets[["forecast_nssp"]],
      command = dplyr::bind_rows(!!!.x)
    )
  )
}

# Combines per-date external forecast / score tar_map outputs and assembles
# final scores_nhsn / scores_nssp by joining with local scores.
build_combined_targets <- function(external_forecast_targets) {
  list2(
    tar_combine(
      name = external_forecasts_full,
      external_forecast_targets[["external_forecasts"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_combine(
      name = external_scores_nhsn_full,
      external_forecast_targets[["score_external_nhsn_forecasts"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_combine(
      name = external_scores_nssp_full,
      external_forecast_targets[["score_external_nssp_forecasts"]],
      command = dplyr::bind_rows(!!!.x)
    ),
    tar_target(
      name = scores_nhsn,
      command = bind_rows(external_scores_nhsn_full, local_scores_nhsn)
    ),
    tar_target(
      name = scores_nssp,
      command = bind_rows(external_scores_nssp_full, local_scores_nssp)
    )
  )
}

# Returns score plot targets for backtest mode. Depends on g_disease, g_forecast_dates,
# scores_nhsn, scores_nssp, and score_report_rmd targets.
build_backtest_score_targets <- function() {
  list2(
    tar_target(
      name = score_nhsn_plot,
      command = render_score_plot(score_report_rmd, scores_nhsn, g_forecast_dates, g_disease, "nhsn"),
      cue = tar_cue("always")
    ),
    tar_target(
      name = score_nssp_plot,
      command = render_score_plot(score_report_rmd, scores_nssp, g_forecast_dates, g_disease, "nssp"),
      cue = tar_cue("always")
    )
  )
}
