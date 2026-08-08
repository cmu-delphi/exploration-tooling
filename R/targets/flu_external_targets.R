#' Create external forecast and score targets for flu forecasting
#'
#' @return A list of targets for external forecasts and scores
#' @export
create_flu_external_targets <- function() {
  list2(
    tar_target(
      outside_forecaster_subset,
      command = c("FluSight-baseline", "FluSight-ensemble", "UMass-flusion")
    ),
    tar_target(
      external_forecasts,
      command = {
        # Reuse the prod hub-download path: get_external_forecasts() reads the
        # per-date hub submissions the get_forecast_data.R cron uploads to S3, so
        # explore compares against the same season it is sweeping rather than the
        # frozen 2023 snapshot. A missing date (404) resolves to empty rows.
        purrr::map(
          as.character(forecast_dates + g_time_value_adjust),
          \(d) get_external_forecasts(glue::glue("exploration/{d}/flu_forecasts.parquet"))
        ) %>%
          bind_rows() %>%
          filter(target == "wk inc flu hosp", forecaster %in% outside_forecaster_subset) %>%
          select(geo_value, forecaster, forecast_date, target_end_date, quantile, prediction = value)
      }
    ),
    tar_target(
      external_scores,
      command = {
        # No matching external forecasts (e.g. the hub has not posted this season
        # yet) -> empty, so downstream bind_rows just yields the Delphi scores.
        if (nrow(external_forecasts) == 0) {
          tibble::tibble()
        } else {
          evaluate_predictions(
            forecasts = external_forecasts %>% rename(model = forecaster),
            truth_data = hhs_evaluation_data %>% select(-population)
          ) %>%
            rename(forecaster = model)
        }
      }
    )
  )
}
