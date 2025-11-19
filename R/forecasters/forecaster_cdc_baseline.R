#' most of these args don't matter for this case
forecaster_cdc_baseline <- function(
    epi_data,
    outcome,
    extra_sources = character(),
    ahead = 1,
    trainer = epipredict::quantile_reg(),
    all_aheads = 1:7,
    data_frequency = "1 week",
    quantile_levels = covidhub_probs(),
    filter_source = "",
    filter_agg_level = "",
    ...) {
  # it's an iterative method, only calculate if we're at the final ahead
  all_aheads <- all_aheads[[1]]
  max_ahead <- max(all_aheads)
  if (ahead < max_ahead) {
    return(tibble(geo_value = character(), forecast_date = Date(), target_end_date = Date(), quantile_value = numeric(), value = numeric()))
  }
  epi_data <- validate_epi_data(epi_data) %>%
    filter(!is.na(!!as.symbol(outcome)))
  extra_sources <- unlist(extra_sources)

  real_forecast_date <- attributes(epi_data)$metadata$as_of
  last_data <- epi_data$time_value %>% max()
  latency_days <- as.integer(real_forecast_date - last_data)
  all_nonzero_aheads <- all_aheads + latency_days
  all_nonzero_aheads_weeks <- all_nonzero_aheads[all_nonzero_aheads != 0] / 7
  ## fcst %>%
  ##   group_by(geo_value, forecast_date, target_date, .pred_distn_quantile_level) %>%
  ##   summarize(max_diff = max(.pred_distn_value) - min(.pred_distn_value)) %>%
  ##   filter(max_diff > 0) %>%
  ##   arrange(desc(max_diff))
  ## basic$predictions %>% arrange(geo_value, ahead)
  fcst <- epi_data %>%
    filter(!is.na(value)) %>%
    cdc_baseline_forecaster(
      outcome,
      args_list = cdc_baseline_args_list(
        aheads = all_nonzero_aheads_weeks,
        data_frequency = "7 days",
        quantile_levels = quantile_levels,
      )
    ) %>%
    `$`(predictions) %>%
    group_by(geo_value, ahead, forecast_date, target_date) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    pivot_quantiles_longer(.pred_distn) %>%
    select(
      geo_value,
      forecast_date,
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
