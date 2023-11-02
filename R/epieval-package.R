#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr select rename inner_join join_by mutate relocate any_of
#'    group_by reframe summarize left_join across filter rowwise everything ungroup
#' @importFrom purrr transpose map map2_vec
#' @keywords internal
"_PACKAGE"
globalVariables(c("ahead", "id", "parent_id", "all_of", "last_col", "time_value", "geo_value", "target_end_date", "forecast_date", "quantile", ".pred_distn", "quantiles", "quantile_levels", "signal", ".dstn", "values", ".", "forecasters", "forecaster", "trainer", "forecast_date", ".pred", "n_distinct", "target_date", "value"))
