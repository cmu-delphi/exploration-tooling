#' an ensemble model that averages each quantile separately
#' @inheritParams scaled_pop
#' @param other_forecasts a list of other
#' @importFrom rlang %||%
#' @export
average_ensemble <- function(epi_data,
                             other_forecasts,
                             outcome,
                             extra_sources = "",
                             ensemble_args = list(),
                             ensemble_args_names = NULL) {
  # unique parameters must be buried in ensemble_args so that the generic function signature is stable
  # their names are separated for obscure target related reasons
  if (!is.null(ensemble_args_names)) {
    names(ensemble_args) <- ensemble_args_names
  }
  average_type <- ensemble_args$average_type %||% median
  join_columns <- ensemble_args$join_columns %||% c("geo_value", "forecast_date", "target_end_date", "quantile")
  # begin actual analysis
  bind_rows(!!!other_forecasts, .id = "forecaster") %>%
    group_by(across(all_of(join_columns))) %>%
    summarize(value = average_type(value)) %>%
    ungroup()
}
## ii <- 1
## browser()
## # group_by followed
## for (ii in seq_along(other_forecasts)) {
##   other_forecasts[[ii]] %<>% rename_with(~ paste(.x, ii, sep = "."), value)
## }
## other_forecasts
## full_forecasts <- reduce(other_forecasts, function(x, y) full_join(x, y, by = join_columns))
## full_forecasts %>% mutate(
##   value = rowMeans(across(starts_with("value")))
## ) %>% glimpse
## rowMeans
