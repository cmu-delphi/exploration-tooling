#' only extend the ahead
#' @description
#' instead of filling in new values, this just extends how far into the future
#'   the model is predicting. For example, if the last data is on the 3rd, the
#'   `as_of` is the 5th, and we want an ahead of 4, then this actually sets the
#'   ahead to be 6, since the 9th (the target date) is 6 days after the last day
#'   of data.
extend_ahead <- function(epi_data, ahead) {
  effective_ahead <- as.numeric(
    attributes(epi_data)$metadata$as_of - max(epi_data$time_value) + ahead
  )
  return(list(epi_data, effective_ahead))
}

#' last observation carried forward
#' @description
locf_latency <- function(epi_data, ahead) {
  # TODO
}
