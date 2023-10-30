# an ensemble model that averages each quantile separately
# @inheritParams scaled_pop
# @param other_forecasts a list of other
# @export
average_ensemble <- function(epi_data,
                             other_forecasts,
                             outcome,
                             extra_sources = "",
                             ahead = 1,
                             average_type = median,
                             join_columns = c("geo_value", "forecast_date", "target_end_date", "quantile"),
                             quantile_levels = covidhub_probs(),
                             ...) {
  # need to figure out if forecaster's predictions go in here, or the finished product comes as input
  bind_rows(!!!other_forecasts, .id = "forecaster") %>%
    group_by(across(join_columns)) %>%
    summarize(value = average_type(value)) %>%
    ungroup
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
