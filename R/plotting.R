# Version: 2024-04-23

library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)


plot_forecasts <- function(predictions_cards, forecast_date, exclude_geos, geo_type) {
  assertthat::assert_that(nrow(predictions_cards) > 0)
  assertthat::assert_that(geo_type %in% c("state", "nation"))

  # Get truth data
  truth_data <- epidatr::pub_covidcast(
    source = "hhs",
    signals = "confirmed_admissions_covid_1d",
    geo_type = geo_type,
    time_type = "day",
    geo_values = "*",
    time_values = epidatr::epirange("2023-12-01", forecast_date)
  ) %>%
    filter(!.data$geo_value %in% exclude_geos) %>%
    select(.data$geo_value, .data$time_value, .data$value) %>%
    rename(target_end_date = .data$time_value) %>%
    mutate(data_source = "hhs", forecaster = "hhs hosp truth")

  # Setup plot
  g <- ggplot(truth_data, mapping = aes(
    x = .data$target_end_date,
    color = .data$forecaster,
    fill = .data$forecaster
  )) +
    geom_line(mapping = aes(y = .data$value))

  # Plot (symmetric) quantiles
  quantiles <- c(0.75, 0.95)
  alphas <- c(0.4, 0.2)
  for (i in seq_along(quantiles)) {
    q <- quantiles[i]
    a <- alphas[i]
    quantile_data <- predictions_cards %>%
      filter(near(.data$quantile, q) | near(.data$quantile, 1 - q)) %>%
      mutate(
        quantile = ifelse(near(.data$quantile, q), "upper", "lower") %>%
          as.factor()
      ) %>%
      pivot_wider(names_from = "quantile", values_from = "value")
    g <- g +
      geom_ribbon(
        data = quantile_data,
        mapping = aes(
          ymin = .data$lower,
          ymax = .data$upper,
          group = interaction(.data$forecast_date, .data$forecaster),
          color = NULL
        ),
        alpha = a
      )
  }

  # Plot median points
  g <- g +
    geom_point(
      data = predictions_cards %>%
        filter(near(.data$quantile, 0.5)),
      mapping = aes(
        y = .data$value,
        group = interaction(.data$forecast_date, .data$forecaster)
      ),
      size = 0.125
    )

  # Add lines, facet, and theme
  if (geo_type == "state") {
    g <- g +
      facet_wrap(~ .data$geo_value, scales = "free_y", ncol = 2, drop = TRUE)
  }
  g <- g + theme(legend.position = "top", legend.text = element_text(size = 7))

  return(g)
}
