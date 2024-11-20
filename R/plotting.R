# Version: 2024-04-23

library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)

get_default_truth_data <- function(exclued_geos, geo_type) {
  # Get truth data
  epidatr::pub_covidcast(
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
}

plot_forecasts <- function(predictions_cards, forecast_date, truth_data, exclude_geos = c(), geo_type = c("state", "nation"), quantiles = c(0.8, 0.95), alphas = c(0.4, 0.2), relevant_period = NULL) {
  if (is.null(truth_data)) {
    truth_data <- get_default_truth_data(exclude_geos, geo_type)
  }
  assertthat::assert_that(nrow(predictions_cards) > 0)
  geo_type <- rlang::arg_match(geo_type)
  # Setup plot
  g <- ggplot(truth_data, mapping = aes(
    x = .data$target_end_date
  )) +
    geom_line(mapping = aes(y = .data$value), color = "red")

  # Plot (symmetric) quantiles
  for (i in seq_along(quantiles)) {
    q <- quantiles[i]
    a <- alphas[i]
    quantile_data <- predictions_cards %>%
      filter(near(.data$quantile, q) | near(.data$quantile, 1 - q)) %>%
      mutate(quantile = ifelse(near(.data$quantile, q), "upper", "lower") %>% as.factor()) %>%
      pivot_wider(names_from = "quantile", values_from = "value")
    g <- g +
      geom_ribbon(
        data = quantile_data,
        mapping = aes(
          ymin = .data$lower,
          ymax = .data$upper,
          group = .data$forecast_date,
          color = NULL,
        ),
        fill = "#22bd22",
        alpha = a
      )
  }

  # Plot median points
  g <- g +
    geom_point(
      data = predictions_cards %>% filter(near(.data$quantile, 0.5)),
      mapping = aes(
        y = .data$value,
        group = .data$forecast_date
      ),
      size = 0.25,
      color = "black"
    )
  # Add lines, facet, and theme
  if ((geo_type == "state") & (length(unique(predictions_cards$forecaster)) > 1)) {
    g <- g +
      facet_grid(.data$geo_value ~ .data$forecaster, scales = "free_y", drop = TRUE)
  } else if (geo_type == "state") {
    g <- g +
      facet_wrap(~ .data$geo_value, scales = "free_y", ncol = 2, drop = TRUE)
  } else if (geo_type == "nation") {
    g <- g +
      facet_grid(~ .data$forecaster, scales = "free_y", drop = TRUE)
  }
  g <- g + theme(legend.position = "top", legend.text = element_text(size = 7))

  # add highlights for the training regions
  if (!is.null(relevant_period)) {
    g <- g + geom_rect(data = relevant_period, inherit.aes = FALSE, aes(xmin = start, xmax = stop, ymin = -Inf, ymax = Inf), color = "transparent", fill = "orange", alpha = 0.3)
  }
  return(g)
}
