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

plot_forecasts <- function(
  predictions_cards,
  forecast_date,
  truth_data,
  exclude_geos = c(),
  geo_type = c("state", "nation"),
  quantiles = c(0.8, 0.95),
  alphas = c(0.4, 0.2),
  relevant_period = NULL
) {
  if (is.null(truth_data)) {
    truth_data <- get_default_truth_data(exclude_geos, geo_type)
  }
  assertthat::assert_that(nrow(predictions_cards) > 0)
  geo_type <- rlang::arg_match(geo_type)
  # Setup plot
  if ("source" %in% names(truth_data)) {
    g <- ggplot(
      truth_data,
      mapping = aes(
        x = .data$target_end_date,
        color = .data$source
      )
    ) +
      geom_line(mapping = aes(y = .data$value))
  } else {
    g <- ggplot(
      truth_data,
      mapping = aes(
        x = .data$target_end_date
      )
    ) +
      geom_line(mapping = aes(y = .data$value), color = "red")
  }

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
  # Add lines, facet, and theme, and grid lines
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
  g <- g +
    theme(legend.position = "top", legend.text = element_text(size = 7)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    scale_x_date(breaks = "3 months", minor_breaks = "1 month", labels = scales::label_date_short())

  # add highlights for the training regions
  if (!is.null(relevant_period)) {
    g <- g +
      geom_rect(
        data = relevant_period,
        inherit.aes = FALSE,
        aes(xmin = start, xmax = stop, ymin = -Inf, ymax = Inf),
        color = "transparent",
        fill = "orange",
        alpha = 0.3
      )
  }
  return(g)
}

# Sets the initial plotly view to the past year on x and per-geo ensemble_mix
# max on y. All data is preserved so the user can zoom/pan out to full history.
apply_view_defaults <- function(p, forecast_data, truth_data, forecast_date, quantiles = c(0.8)) {
  one_year_ago <- forecast_date - 365
  forecast_end <- max(forecast_data$target_end_date, na.rm = TRUE)
  max_q <- max(quantiles)

  ens_max <- forecast_data %>%
    filter(forecaster == "ensemble_mix", near(.data$quantile, max_q)) %>%
    group_by(.data$geo_value) %>%
    summarise(max_y = max(.data$value, na.rm = TRUE), .groups = "drop")

  truth_max <- truth_data %>%
    filter(.data$target_end_date >= one_year_ago) %>%
    group_by(.data$geo_value) %>%
    summarise(max_y = max(.data$value, na.rm = TRUE), .groups = "drop")

  geo_max <- bind_rows(ens_max, truth_max) %>%
    group_by(.data$geo_value) %>%
    summarise(max_y = max(.data$max_y, na.rm = TRUE) * 1.1, .groups = "drop")

  pb <- plotly_build(p)
  layout_items <- pb$x$layout

  x_range <- list(format(one_year_ago, "%Y-%m-%d"), format(forecast_end + 7, "%Y-%m-%d"))
  # Set every x-axis to the past-year range. facet_grid(geo ~ forecaster) creates
  # one xaxis entry per forecaster column, so we must update them all.
  xaxis_keys <- names(layout_items)[grepl("^xaxis", names(layout_items))]
  xaxis_updates <- setNames(
    lapply(xaxis_keys, function(k) list(range = x_range, autorange = FALSE)),
    xaxis_keys
  )

  # Primary y-axes are those with their own domain (one per row = one per geo).
  # Sort by domain[1] descending: top of screen = highest domain value = first geo alphabetically.
  yaxis_keys <- names(layout_items)[grepl("^yaxis", names(layout_items))]
  primary_yaxis <- Filter(function(k) !is.null(layout_items[[k]]$domain), yaxis_keys)
  domain_bottoms <- sapply(primary_yaxis, function(k) layout_items[[k]]$domain[[1]])
  yaxis_by_position <- names(sort(domain_bottoms, decreasing = TRUE))

  geos <- sort(unique(as.character(forecast_data$geo_value)))

  yaxis_updates <- list()
  for (ii in seq_along(geos)) {
    if (ii > length(yaxis_by_position)) break
    max_y <- geo_max %>% filter(.data$geo_value == geos[[ii]]) %>% pull(.data$max_y)
    if (length(max_y) == 0 || !is.finite(max_y)) next
    yaxis_updates[[yaxis_by_position[[ii]]]] <- list(range = list(0, max_y), autorange = FALSE)
  }

  do.call(layout, c(list(p), xaxis_updates, yaxis_updates))
}
