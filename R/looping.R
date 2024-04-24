#' Generate forecaster predictions on a particular dataset
#'
#' A wrapper that turns a forecaster, parameters, data combination into an
#' actual experiment that outputs a prediction. as far as batchtools is
#' concerned, the scoring function is a particular parameter of the forecaster
#' (or Algorithm, as they call it).
#'
#' @param data the epi_df object
#' @param outcome the name of the target column
#' @param extra_sources any extra columns used for prediction that aren't
#'   the target
#' @param forecaster a function that does the actual forecasting for a given
#'   day. See `exampleSpec.R` for an example function and its documentation
#'   for the general parameter requirements.
#' @param slide_training a required parameter that governs how much data to
#'   exclude before starting the evaluation.
#' @param n_training_pad a required parameter that determines how many extra
#'   samples for epix_slide to hand to the forecaster to guarantee that at
#'   least `ntraining` examples are available to the forecaster.
#' @param forecaster_args the list of arguments to the forecaster; it must
#'   contain `ahead`
#' @param forecaster_args_names a bit of a hack around targets, it contains
#'   the names of the `forecaster_args`.
#' @param date_range_step_size the step size (in days) to use when generating
#'   the forecast dates.
#'
#' @importFrom epiprocess epix_slide
#' @importFrom cli cli_abort
#' @importFrom rlang !!
#' @export
slide_forecaster <- function(data,
                             outcome,
                             extra_sources = "",
                             forecaster = scaled_pop,
                             slide_training = 0,
                             n_training_pad = 5,
                             forecaster_args = list(),
                             forecaster_args_names = list(),
                             date_range_step_size = 1L) {
  archive <- data
  if (length(forecaster_args) > 0) {
    names(forecaster_args) <- forecaster_args_names
  }
  if (is.null(forecaster_args$ahead)) {
    cli_abort(
      c(
        "exploration-tooling error: slide_forecaster needs some value for ahead."
      ),
      class = "explorationToolingError"
    )
  }
  if (!is.numeric(forecaster_args$n_training) && !is.null(forecaster_args$n_training)) {
    n_training <- as.numeric(forecaster_args$n_training)
    net_slide_training <- max(slide_training, n_training) + n_training_pad
  } else {
    n_training <- Inf
    net_slide_training <- slide_training + n_training_pad
  }
  # restrict the dataset to areas where training is possible
  start_date <- min(archive$DT$time_value) + net_slide_training
  end_date <- max(archive$DT$time_value) - forecaster_args$ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = date_range_step_size)

  # first generate the forecasts
  before <- n_training + n_training_pad - 1
  ## TODO: epix_slide doesn't support infinite `before`
  ## https://github.com/cmu-delphi/epiprocess/issues/219
  if (before == Inf) before <- 365L * 10000
  res <- epix_slide(archive,
    function(data, gk, rtv, ...) {
      # TODO: Can we get rid of this tryCatch and instead hook it up to targets
      #       error handling or something else?
      #       https://github.com/cmu-delphi/exploration-tooling/issues/41
      tryCatch(
        {
          do.call(
            forecaster,
            append(
              list(
                epi_data = data,
                outcome = outcome,
                extra_sources = extra_sources
              ),
              forecaster_args
            )
          )
        },
        error = function(e) {
          if (interactive()) {
            browser()
          } else {
            dump_vars <- list(
              data = data,
              rtv = rtv,
              forecaster = forecaster,
              forecaster_args = forecaster_args,
              e = e
            )
            saveRDS(dump_vars, "slide_forecaster_error.rds")
            e
          }
        }
      )
    },
    before = before,
    ref_time_values = valid_predict_dates,
  )
  res %<>% select(-time_value)
  names(res) <- sub("^slide_value_", "", names(res))

  # append the truth data
  true_value <- archive %>%
    epiprocess::epix_as_of(archive$versions_end) %>%
    select(geo_value, time_value, !!outcome) %>%
    rename(true_value = !!outcome)
  res %<>%
    inner_join(true_value,
      by = join_by(geo_value, target_end_date == time_value)
    )
  return(res)
}
