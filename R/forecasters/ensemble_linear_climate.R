#' An ensemble model that averages each quantile separately
#'
#' The simplest class of ensembing models, it takes in a list of quantile
#' forecasts and averages them on a per-quantile basis. By default the average
#' used is the median, but it can accept any vectorized function.
#'
#' @param epi_data The data for fitting. Currently unused, but matches interface
#' of other forecasters.
#' @param forecasts A tibble of quantile forecasts to aggregate. They should
#'   be tibbles with columns `(geo_value, forecast_date, target_end_date,
#'   quantile, value)`, preferably in that order.
#' @param outcome The name of the target variable. Currently unused, but matches
#' interface of other forecasters.
#' @param other_weights if non null, it should be a tibble giving a list of weights by forecaster and geo_value
#' @importFrom rlang %||%
#' @export
ensemble_linear_climate <- function(forecasts,
                                    aheads,
                                    other_weights = NULL,
                                    probs = covidhub_probs(),
                                    min_climate_ahead_weight = 0.05,
                                    max_climate_ahead_weight = 0.90,
                                    min_climate_quantile_weight = 0.1,
                                    max_climate_quantile_weight = 1) {
  weights <-
    make_ahead_weights(aheads, min_climate_ahead_weight, max_climate_ahead_weight) %>%
    left_join(
      make_quantile_weights(probs, min_climate_quantile_weight, max_climate_quantile_weight),
      by = c("forecast_family"),
      relationship = "many-to-many"
    ) %>%
    mutate(weight = weight.x * weight.y) %>%
    select(forecast_family, quantile, ahead, weight)

  forecasters <- unique(forecasts$forecaster)
  other_weights <-
    other_weights %>%
    filter(
      geo_value %in% unique(forecasts$geo_value),
      forecaster %in% forecasters
    )
  full_weights <- weights %>%
    left_join(
      tibble(
        forecast_family = str_match(forecasters, "climate|linear"),
        forecaster = forecasters
      ),
      by = join_by(forecast_family),
      relationship = "many-to-many"
    ) %>%
    select(-forecast_family) %>%
    left_join(
      other_weights,
      by = join_by(forecaster),
      relationship = "many-to-many"
    ) %>%
    mutate(weight = weight.x * weight.y) %>%
    select(geo_value, ahead, forecaster, quantile, weight) # renormalize so the weights sum to 1
  renorm <-
    full_weights %>%
    group_by(geo_value, quantile, ahead) %>%
    summarize(mass = sum(weight), .groups = "drop")
  full_weights <- full_weights %>%
    left_join(renorm, by = c("geo_value", "ahead", "quantile")) %>%
    mutate(weight = weight / mass) %>%
    select(-mass)
  weighted_forecasts <-
    forecasts %>%
    mutate(quantile = round(quantile, digits = 3)) %>%
    mutate(ahead = as.integer(floor((target_end_date - forecast_date) / 7))) %>%
    left_join(
      full_weights,
      by = join_by(geo_value, forecaster, quantile, ahead)
    ) %>%
    mutate(value = weight * value) %>%
    group_by(geo_value, forecast_date, target_end_date, quantile) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
  # sort the quantiles
  weighted_forecasts <- weighted_forecasts %>%
    group_by(geo_value, target_end_date, forecast_date) %>%
    arrange(geo_value, target_end_date, forecast_date, quantile) %>%
    mutate(value = sort(value))
  return(weighted_forecasts)
}

#' Generate linear vs climate weights based on quantile
#'
#' The weights are chosen to go from heavy on the linear models near the median,
#' to heavy on the climatic on the last forecasted day.
#' @param min_climate_ahead_weight the minimal weight the climate model is able
#'   to take on, which occurs on the first day
#' @param max_climate_ahead_weight the maximal weight the climate model is able
#'   to take on, which occurs on the last day
make_quantile_weights <- function(quantiles,
                                  min_climate_weight = 0.1,
                                  max_climate_weight = 1.0) {
  # x ranges from 0-1, so abs(x-0.5)*1 is 0 near 0.5, and 1 near either 0 or 1
  quantile_weight_values <-
    quantiles %>% map_dbl(\(x) min_climate_weight + (max_climate_weight - min_climate_weight) * abs(x - 0.5) * 2)
  bind_rows(
    tibble(forecast_family = "climate", quantile = quantiles, weight = quantile_weight_values),
    tibble(forecast_family = "linear", quantile = quantiles, weight = 1 - quantile_weight_values)
  )
}

#' Generate linear vs climate weights based on ahead
#'
#' The weights are chosen to go from heavy on the linear models on the first
#' forecasted day to heavy on the climatic on the last forecasted day
#' @param min_climate_ahead_weight the minimal weight the climate model is able
#'   to take on, which occurs on the first day
#' @param max_climate_ahead_weight the maximal weight the climate model is able
#'   to take on, which occurs on the last day
make_ahead_weights <- function(aheads,
                               min_climate_weight = 0.05,
                               max_climate_weight = 0.9) {
  ahead_weight_values <- seq(
    from = min_climate_weight,
    to = max_climate_weight,
    length.out = max(aheads) - min(aheads) + 1
  )
  ahead_weights <- bind_rows(
    tibble(forecast_family = "climate", ahead = sort(aheads), weight = ahead_weight_values),
    tibble(forecast_family = "linear", ahead = sort(aheads), weight = 1 - ahead_weight_values)
  )
}
