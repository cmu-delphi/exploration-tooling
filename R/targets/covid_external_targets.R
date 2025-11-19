#' COVID forecast targets
#'
#' This file contains functions to create targets for COVID forecasts and scores.

#' Create external forecast and score targets for COVID forecasting
#'
#' @return A list of targets for external forecasts and scores
#' @export
create_covid_external_targets <- function(start_date, end_date) {
  external_forecast_targets <- tar_map(
    values = tibble(
      forecast_date_int = seq(round_date(g_start_date - 3, "week", 6), round_date(g_end_date - 3, "week", 6), by = "week")
    ) %>%
      mutate(
        forecast_date_chr = as.character(as.Date(forecast_date_int)),
        filename = paste0(g_s3_prefix, "/", forecast_date_chr, "/", g_disease, "_forecasts.parquet"),
      ),
    names = "forecast_date_chr",
    tar_change(
      name = external_forecasts,
      change = get_s3_object_last_modified(filename, "forecasting-team-data"),
      command = {
        df <- get_external_forecasts(filename)
        if (g_percent_to_fraction & (nrow(df) > 0)) {
          df <-
            df %>%
            mutate(value = value / 100)
        }
        return(df)
      }
    ),
    tar_target(
      name = score_external_nhsn_forecasts,
      command = {
        score_forecasts(
          evaluation_data %>% mutate(
            time_value = target_end_date, value = true_value
          ),
          external_forecasts, "wk inc covid hosp"
        )
      }
    )
  )
  rlang::list2(
    external_forecast_targets,
    tar_combine(
      name = external_forecasts_full,
      external_forecast_targets[["external_forecasts"]],
      command = {
        dplyr::bind_rows(!!!.x)
      }
    ),
    tar_target(
      name = external_forecasts_nhsn,
      command = {
        external_forecasts_full %>%
          filter(target == "wk inc covid hosp")
      }
    ),
    tar_combine(
      name = external_scores_nhsn,
      external_forecast_targets[["score_external_nhsn_forecasts"]],
      command = {
        dplyr::bind_rows(!!!.x)
      }
    ),
  )
}
create_covid_external_targets_old <- function() {
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
          truth_data = evaluation_data
        ) %>%
          rename(forecaster = model)
      }
    )
  )
}
