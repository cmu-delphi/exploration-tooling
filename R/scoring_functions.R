#' tables don't play nicely with functions directly
scoring_lookup <- function(string_name) {

}
weighted_interval_score <- function(predictions) {
  predictions %>%
    group_by(across(-c(
      quantile,
      value,
      actual_value
    ))) %>%
    summarise(weighted_interval_score = 2 * mean(pmax(quantile * (actual_value - value), (1 - quantile) * (value - actual_value), na.rm = TRUE)))
}
