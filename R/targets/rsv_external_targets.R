#' RSV external forecast targets
#'
#' This file contains functions to create targets for RSV hub forecasts and
#' scores.

#' Create external forecast and score targets for RSV forecasting
#'
#' @return A list of targets for external forecasts and scores
#' @export
create_rsv_external_targets <- function() {
  rlang::list2(
    tar_target(
      outside_forecaster_subset,
      command = c("RSVHub-baseline", "RSVHub-ensemble")
    ),
    tar_target(
      external_forecasts,
      command = {
        purrr::map(
          as.character(forecast_dates + g_time_value_adjust),
          \(d) get_external_forecasts(glue::glue("exploration/{d}/rsv_forecasts.parquet"))
        ) %>%
          bind_rows() %>%
          filter(target == "wk inc rsv hosp", forecaster %in% outside_forecaster_subset) %>%
          select(geo_value, forecaster, forecast_date, target_end_date, quantile, prediction = value)
      }
    ),
    tar_target(
      external_scores,
      command = {
        if (nrow(external_forecasts) == 0) {
          tibble::tibble()
        } else {
          evaluate_predictions(
            forecasts = external_forecasts %>% rename(model = forecaster),
            truth_data = hhs_evaluation_data
          ) %>%
            rename(forecaster = model)
        }
      }
    )
  )
}
