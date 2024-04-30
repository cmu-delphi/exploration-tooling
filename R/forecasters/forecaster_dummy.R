#' Dummy forecaster.
#'
#' A simple faux-forecaster handy for debugging pipelines. Returns a fake
#' forecast, with the its arguments stored in the `params` column.
#' Implementation assumes that the `epi_df` argument is the first argument.
#'
#' @param ... Arguments to be stored in the `params` column.
#' @param .keep_epi_data Whether to keep the `epi_df` argument in the `params`
#'  column. Defaults to `FALSE` because it's expensive to store and read.
dummy_forecaster <- function(..., .keep_epi_data = FALSE) {
  dots <- list(...)
  if ("epi_df" %in% names(dots)) {
    forecast_date <- max(pluck(dots, "epi_df")$time_value)
  } else {
    forecast_date <- as.Date("2024-01-01")
  }
  if (!.keep_epi_data) {
    dots <- dots[-1]
  }
  tibble(
    forecaster = pluck(dots, "forecaster", .default = "dummy_forecaster"),
    geo_value = "ak",
    forecast_date = forecast_date,
    target_end_date = forecast_date + pluck(dots, "ahead", .default = 1),
    quantile = covidhub_probs(),
    value = seq_along(quantile),
    params = list(dots),
  )
}
