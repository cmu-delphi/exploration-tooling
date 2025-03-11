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
      external_forecasts_file,
      command = {
        s3load("flusight_forecasts_2023.rds", bucket = "forecasting-team-data")
        flusight_forecasts_2023
      }
    ),
    tar_target(
      external_forecasts,
      command = {
        external_forecasts_file %>%
          filter(forecaster %in% outside_forecaster_subset)
      }
    ),
    tar_target(
      external_scores,
      command = {
        evaluate_predictions(
          forecasts = external_forecasts %>%
            filter(forecast_date %in% (forecast_dates + g_time_value_adjust)) %>%
            rename(model = forecaster),
          truth_data = hhs_evaluation_data %>% select(-population)
        ) %>%
          rename(forecaster = model)
      }
    )
  )
}
