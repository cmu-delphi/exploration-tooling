#' COVID forecast targets
#'
#' This file contains functions to create targets for COVID forecasts and scores.

#' Create external forecast and score targets for COVID forecasting
#'
#' @return A list of targets for external forecasts and scores
#' @export
create_covid_external_targets <- function() {
  rlang::list2(
    tar_target(
      outside_forecaster_subset,
      command = c("COVIDhub-baseline", "COVIDhub-trained_ensemble", "COVIDhub_CDC-ensemble")
    ),
    tar_target(
      external_forecasts_file,
      command = s3read_using(
        nanoparquet::read_parquet,
        # TODO: How was this generated? Was there any date shifting there?
        object = "covid19_forecast_hub_2023_full_summed.parquet",
        bucket = "forecasting-team-data"
      )
    ),
    tar_target(
      external_forecasts,
      command = {
        external_forecasts_file %>%
          filter(geo_value %in% state_geo_values, forecaster %in% outside_forecaster_subset) %>%
          rename(ahead = week_ahead, prediction = value) %>%
          # Push the label to Saturday from Monday.
          mutate(forecast_date = forecast_date + 5) %>%
          # Filter to only forecasts we care about.
          filter(forecast_date %in% (forecast_dates + g_time_value_adjust)) %>%
          mutate(target_end_date = as.Date(forecast_date) + 7 * as.numeric(ahead)) %>%
          # TODO: A very rough adjustment to get daily counts on the same scale
          # as weekly counts.
          mutate(prediction = prediction * 7)
      }
    ),
    tar_target(
      external_scores,
      command = {
        evaluate_predictions(
          forecasts = external_forecasts %>% rename(model = forecaster),
          truth_data = hhs_evaluation_data
        ) %>%
          rename(forecaster = model)
      }
    )
  )
}
