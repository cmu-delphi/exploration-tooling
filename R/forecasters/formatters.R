#' Storage format
#'
#' The columns for any stored prediction are `(geo_value, forecast_date,
#' target_end_date, quantile, value)` in that particular order. It does not
#' include the point estimate as a separate row.
#'
#' @param pred the output as produced by epipredict
#' @param true_forecast_date the actual date from which the model is
#'   making the forecast, rather than the last day of available data
#' @param target_end_date the date of the prediction
#'
#' @importFrom epipredict nested_quantiles
#' @importFrom tidyr unnest
#' @export
format_storage <- function(pred, true_forecast_date, target_end_date) {
  pred %>%
    epipredict::pivot_quantiles_longer(.pred_distn) %>%
    mutate(value = .pred_distn_value, quantile = .pred_distn_quantile_level) %>%
    select(-starts_with(".pred"), -any_of(c("time_value"))) %>%
    relocate(where(is.character), where(is.factor), forecast_date, target_end_date = target_date, quantile, value)
}

#' Format for the COVID-19 Forecast Hub
#'
#' Expects the pred to have the columns geo_value, .pred, .pred_distn,
#' forecast_date, and target_date. It does not assume that the forecast_date is
#' accurate.
#'
#' @param pred the output as produced by epipredict
#' @param true_forecast_date the actual date from which the model is
#'   making the forecast, rather than the last day of available data
#' @param target_end_date the date of the prediction
#' @param quantile_levels the quantile levels
format_covidhub <- function(pred, true_forecast_date, target_end_date, quantile_levels) {
  pred %<>%
    group_by(forecast_date, geo_value, target_date) %>%
    rename(target_end_date = target_date) %>%
    reframe(quantile = quantile_levels, value = quantile(.pred_distn, quantile_levels)[[1]])
  forecasts$ahead <- ahead
  forecasts %<>%
    group_by(forecast_date, geo_value, target_date) %>%
    mutate(forecast_date = target_date - ahead) %>%
    rename(target_end_date = target_date) %>%
    reframe(
      quantile = quantiles,
      value = quantile(.pred_distn, quantiles)[[1]],
      ahead = ahead
    )
  forecasts$forecaster <- "epipredict_forecast"
  forecasts$incidence_period <- "day"
  forecasts$data_source <- source
  forecasts$signal <- signal
  epipredict_forecast <- rbind(epipredict_forecast, forecasts)
}

#' Expects columns: geo_value, forecast_date, target_end_date, quantile, value
#' Returns columns: reference_date, target, horizon, target_end_date, location, output_type, output_type_id, value
format_flusight <- function(pred, disease = c("flu", "covid")) {
  disease <- arg_match(disease)
  pred %>%
    mutate(
      reference_date = get_forecast_reference_date(forecast_date),
      target = glue::glue("wk inc {disease} hosp"),
      horizon = as.integer(floor((target_end_date - reference_date) / 7)),
      output_type = "quantile",
      output_type_id = quantile,
      value = value
    ) %>%
    left_join(get_population_data() %>% select(state_id, state_code), by = c("geo_value" = "state_id")) %>%
    mutate(location = state_code) %>%
    select(reference_date, target, horizon, target_end_date, location, output_type, output_type_id, value)
}

format_scoring_utils <- function(forecasts_and_ensembles, disease = c("flu", "covid")) {
  forecasts_and_ensembles %>%
    filter(!grepl("region.*", geo_value)) %>%
    mutate(
      reference_date = get_forecast_reference_date(forecast_date),
      target = glue::glue("wk inc {disease} hosp"),
      horizon = as.integer(floor((target_end_date - reference_date) / 7)),
      output_type = "quantile",
      output_type_id = quantile,
      value = value
    ) %>%
    left_join(
      get_population_data() %>%
        select(state_id, state_code),
      by = c("geo_value" = "state_id")
    ) %>%
    rename(location = state_code, model_id = forecaster) %>%
    select(reference_date, target, horizon, target_end_date, location, output_type, output_type_id, value, model_id) %>%
    drop_na()
}

#' The quantile levels used by the covidhub repository
#'
#' @param type either standard or inc_case, with inc_case being a small subset of the standard
#'
#' @export
covidhub_probs <- function(type = c("standard", "inc_case")) {
  type <- match.arg(type)
  switch(type,
    standard = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
    inc_case = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  ) |> round(digits = 3)
}
