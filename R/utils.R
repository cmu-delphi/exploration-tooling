`%nin%` <- function(x, y) !(x %in% y)

#' Ensure that forecast values are monotically increasing
#' in quantile order.
sort_by_quantile <- function(forecasts) {
  forecasts %>%
    arrange(geo_value, target_end_date, forecast_date, quantile) %>%
    group_by(geo_value, forecast_date, target_end_date) %>%
    filter(!anyNA(value)) %>%
    mutate(value = sort(value)) %>%
    ungroup()
}

#' Print recent targets errors.
get_targets_errors <- function(project = tar_path_store(), top_n = 10) {
  meta_df <- targets::tar_meta(store = project)
  forecast_errors <- meta_df %>%
    filter(!is.na(parent), !is.na(error)) %>%
    distinct(parent, error, .keep_all = TRUE) %>%
    mutate(parent = gsub("forecast_", "", parent)) %>%
    slice_max(time, n = top_n)

  # Print each error message, along with the parent target.
  if (nrow(forecast_errors) > 0) {
    cat("Forecast errors:\n")
    for (i in 1:nrow(forecast_errors)) {
      cli::cli_inform(c(
        "Parent target: {forecast_errors$parent[i]}",
        "Time: {forecast_errors$time[i]}",
        "Error: {forecast_errors$error[i]}"
      ))
    }
  }

  other_errors <- meta_df %>%
    filter(!is.na(error)) %>%
    distinct(error, .keep_all = TRUE) %>%
    slice_max(time, n = top_n)

  # Print each error message, along with the parent target.
  if (nrow(other_errors) > 0) {
    cat("Other errors:\n")
    for (i in 1:nrow(other_errors)) {
      cli::cli_inform(c(
        "Target: {other_errors$name[i]}",
        "Time: {other_errors$time[i]}",
        "Error: {other_errors$error[i]}"
      ))
    }
  }

  return(invisible(meta_df %>% slice_max(time, n = top_n)))
}

#' Retry a function.
#'
#' @param max_attempts The maximum number of attempts.
#' @param wait_seconds The number of seconds to wait between attempts.
#' @param fn The function to retry.
#' @param ... Additional arguments to pass to the function.
retry_fn <- function(max_attempts = 10, wait_seconds = 1, fn, ...) {
  for (attempt in 1:max_attempts) {
    tryCatch(
      {
        result <- fn(...)
        return(result) # Return successful result
      },
      error = function(e) {
        if (attempt == max_attempts) {
          stop("Maximum retry attempts reached. Last error: ", e$message)
        }
        message(sprintf("Attempt %d failed. Retrying in %d second(s)...", attempt, wait_seconds))
        Sys.sleep(wait_seconds)
      }
    )
  }
}

validate_epi_data <- function(epi_data) {
  if (!inherits(epi_data, "epi_df")) {
    epi_data <- epi_data %>% as_epi_df(as_of = max(epi_data$time_value))
  }
  if (is.null(attributes(epi_data)$metadata$as_of)) {
    attributes(epi_data)$metadata$as_of <- max(epi_data$time_value)
  }
  return(epi_data)
}

#' create a list of valid locations x forecast_dates shared among forecasters
#' which have at least `min_locations` and `min_dates`, and create a list of
#' these for each forecaster
get_unique <- function(forecasts, min_locations = 50, min_dates = 40) {
  forecasters <- forecasts %>%
    pull(forecaster) %>%
    unique()
  distinct <- map(
    forecasters,
    \(x) {
      forecasts %>%
        filter(forecaster == x) %>%
        distinct(geo_value, forecast_date, target_end_date)
    }
  )
  # decide which of the forecasters has enough locations
  to_keep <- distinct %>%
    map_lgl(\(x) {
      (nrow(distinct(x, geo_value)) >= min_locations) &
        (nrow(distinct(x, forecast_date)) >= min_dates)
    })
  if (all(!to_keep)) {
    max_geos <- distinct %>%
      map_int(\(x) {
        nrow(distinct(x, geo_value))
      }) %>%
      max()
    max_dates <- distinct %>%
      map_int(\(x) {
        nrow(distinct(x, forecast_date))
      }) %>%
      max()
    cli::cli_abort(
      "there are at most {max_geos} locations and {max_dates} dates. Adjust `min_locations` and/or `min_dates`."
    )
  }
  forecasters <- forecasters[to_keep]
  distinct <- distinct[to_keep]
  distinct_dates <- reduce(
    distinct,
    \(x, y) x %>% inner_join(y, by = c("geo_value", "forecast_date", "target_end_date"))
  )
  distinct_dates %>%
    mutate(
      forecast_date = round_date(forecast_date, unit = "week", week_start = 6)
    ) %>%
    cross_join(
      tibble(forecaster = forecasters),
      .
    )
}

#' filter the external and local forecasts to just the shared dates/geos
#' some forecasters have a limited set of geos; we want to include those
#' anyways, they are `tructated_forecasters`, while the external_forecasts may
#' have previous years forecasts that we definitely want to exclude via
#' `season_start`.
filter_shared_geo_dates <- function(
  local_forecasts,
  external_forecasts,
  season_start = "2024-11-01",
  trucated_forecasters = "windowed_seasonal_extra_sources",
  min_locations = 52,
  min_dates = 12
) {
  # the length is one if we're forecasting this week, in which case we only want the last 12 weeks of forecasts
  if (local_forecasts %>% distinct(forecast_date) %>% length() == 1) {
    viable_dates <-
      external_forecasts %>%
      get_unique(min_locations = min_locations, min_dates = min_dates)
  } else {
    viable_dates <- inner_join(
      local_forecasts %>%
        filter(forecaster %nin% trucated_forecasters) %>%
        get_unique(),
      external_forecasts %>%
        filter(forecast_date > season_start) %>%
        get_unique(),
      by = c("geo_value", "forecast_date", "target_end_date")
    )
  }
  dplyr::bind_rows(
    local_forecasts %>%
      mutate(
        forecast_date = round_date(forecast_date, unit = "week", week_start = 6)
      ) %>%
      inner_join(viable_dates, by = c("forecaster", "geo_value", "forecast_date", "target_end_date")),
    external_forecasts %>%
      inner_join(viable_dates, by = c("forecaster", "geo_value", "forecast_date", "target_end_date"))
  )
}

#' Calculate MD5 hash of a file
get_file_hash <- function(file, algorithm = "md5") {
  readBin(file, what = "raw", n = file.size(file)) %>%
    digest::digest(algo = algorithm, serialize = FALSE)
}
