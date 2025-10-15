get_external_forecasts <- function(external_object_name) {
  # try-catch in case that particular date is a 404
  tryCatch(
    {
      external_values <- s3read_using(
        nanoparquet::read_parquet,
        object = external_object_name,
        bucket = "forecasting-team-data"
      )
      locations_crosswalk <- get_population_data() %>%
        select(state_id, state_code) %>%
        filter(state_id != "usa")
      external_values <- external_values %>%
        filter(output_type == "quantile") %>%
        select(target, forecaster, geo_value = location, forecast_date, target_end_date, quantile = output_type_id, value) %>%
        inner_join(locations_crosswalk, by = c("geo_value" = "state_code")) %>%
        mutate(geo_value = state_id) %>%
        select(target, forecaster, geo_value, forecast_date, target_end_date, quantile, value)
      return(external_values)
    },
    error = function(e) {
      return(tibble())
    }
  )
}

score_forecasts <- function(latest_data, forecasts, target) {
  if (length(forecasts) == 0) {
    return(tibble())
  }
  truth_data <-
    latest_data %>%
    select(geo_value, target_end_date = time_value, oracle_value = value) %>%
    left_join(
      get_population_data() %>%
        select(state_id, state_code),
      by = c("geo_value" = "state_id")
    ) %>%
    drop_na() %>%
    rename(location = state_code) %>%
    select(-geo_value) %>%
    mutate(
      target_end_date = round_date(target_end_date, unit = "week", week_start = 6)
    )
  # limit the forecasts to the same set of forecasting times
  max_forecast_date <-
    forecasts %>%
    group_by(forecaster) %>%
    summarize(max_forecast = max(forecast_date)) %>%
    pull(max_forecast) %>%
    min()
  forecasts_formatted <-
    forecasts[forecasts$forecast_date <= max_forecast_date, ]
  if ("target" %in% names(forecasts_formatted)) {
    forecasts_formatted <-
      forecasts_formatted[forecasts_formatted$target == target, ]
  }
  # no forecasts for that target for these forecast dates
  if (nrow(forecasts_formatted) == 0) {
    return(tibble())
  }
  forecasts_formatted %<>%
    format_scoring_utils(target)
  if (target == "wk inc covid prop ed visits") {
    forecasts_formatted %<>% mutate(value = value * 100)
  }
  tryCatch(
    {
      scores <-
        forecasts_formatted %>%
        filter(output_type == "quantile") %>%
        filter(location %nin% c("US", "60", "66", "78")) %>%
        hubEvals::score_model_out(
          truth_data,
          metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_90"),
          summarize = TRUE,
          by = c("model_id", "target", "reference_date", "location", "horizon")
        )
      scores %>%
        left_join(
          get_population_data() %>%
            select(state_id, state_code),
          by = c("location" = "state_code")
        ) %>%
        rename(
          forecaster = model_id,
          forecast_date = reference_date,
          ahead = horizon,
          geo_value = state_id
        ) %>%
        select(-location)
    },
    error = function(e) {
      if (rlang::cnd_message(e) == "\033[1m\033[22m\033[33m!\033[39m After removing rows with NA values in the data, no forecasts are left.") {
        return(tibble())
      } else {
        e
      }
    }
  )
}

render_score_plot <- function(score_report_rmd, scores, forecast_dates, disease, target) {
  rmarkdown::render(
    score_report_rmd,
    params = list(
      scores = scores,
      forecast_dates = forecast_dates,
      disease = disease,
      target = target
    ),
    output_file = here::here(
      "reports",
      glue::glue("{disease}_{target}_backtesting_2024_2025_on_{as.Date(Sys.Date())}")
    )
  )
}
