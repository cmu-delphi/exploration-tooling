# Scoring and Evaluation Functions

evaluate_predictions <- function(forecasts, truth_data) {
  # make sure the quantiles are in ascending order
  forecasts <- forecasts %>%
    arrange(model, geo_value, target_end_date, forecast_date, quantile) %>%
    group_by(model, geo_value, target_end_date, forecast_date) %>%
    mutate(prediction = sort(prediction)) %>%
    ungroup()

  checkmate::assert_data_frame(forecasts)
  checkmate::assert_data_frame(truth_data)
  checkmate::assert_names(
    names(forecasts),
    must.include = c("model", "geo_value", "forecast_date", "target_end_date", "quantile", "prediction")
  )
  checkmate::assert_names(
    names(truth_data),
    must.include = c("geo_value", "target_end_date", "true_value")
  )

  forecast_obj <- left_join(forecasts, truth_data, by = c("geo_value", "target_end_date")) %>%
    scoringutils::as_forecast_quantile(
      quantile_level = "quantile",
      observed = "true_value",
      predicted = "prediction",
      forecast_unit = c("model", "geo_value", "forecast_date", "target_end_date")
    )

  scores <- forecast_obj %>%
    scoringutils::score(metrics = get_metrics(.)) %>%
    as_tibble()
  missing_metrics <- setdiff(
    c(
      "model",
      "geo_value",
      "forecast_date",
      "target_end_date",
      "wis",
      "ae_median",
      "interval_coverage_50",
      "interval_coverage_90"
    ),
    names(scores)
  )
  if (length(missing_metrics) > 0) {
    cli::cli_abort(c(
      "scoring error",
      "i" = "missing metrics: {missing_metrics}",
      "i" = "if wis is missing, then likely quantile monotonicity was violated"
    ))
  }
  scores %>%
    select(
      model,
      geo_value,
      forecast_date,
      target_end_date,
      wis,
      ae = ae_median,
      coverage_50 = interval_coverage_50,
      coverage_90 = interval_coverage_90
    ) %>%
    mutate(ahead = as.numeric(target_end_date - forecast_date))
}
