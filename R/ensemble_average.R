#' An ensemble model that averages each quantile separately
#'
#' The simplest class of ensembing models, it takes in a list of quantile
#' forecasts and averages them on a per-quantile basis. By default the average
#' used is the median, but it can accept any vectorized function.
#'
#' @param epi_data unused for this forecaster, but potentially an ensemble may
#'   want the underlying data.
#' @param outcome The name of the target variable.
#' @param extra_sources The name of any extra columns to use. This list could be
#'   empty
#' @param forecasts a list of quantile forecasts to aggregate. They should
#'   be tibbles with columns `(geo_value, forecast_date, target_end_date,
#'   quantile, value)`, preferably in that order.
#' @param ensemble_args any arguments unique to this particular ensembler should
#'   be included in a list like this (unfortunate targets issues). The arguments
#'   for `ensemble_average` in particular are `average_type` and `join_columns`
#' @param ensemble_args_names an argument purely for use in targets. You
#'   probably shouldn't worry about it. In a target, it should probably be
#'   `ensemble_args_names = names(ensemble_args)`
#'
#' @importFrom rlang %||%
#' @export
ensemble_average <- function(epi_data,
                             forecasts,
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
  bind_rows(!!!forecasts, .id = "forecaster") %>%
    group_by(across(all_of(join_columns))) %>%
    summarize(value = average_type(value)) %>%
    ungroup()
}
