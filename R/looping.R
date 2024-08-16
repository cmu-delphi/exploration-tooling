#' Generate forecaster predictions on a particular dataset
#'
#' A wrapper that turns a forecaster, parameters, data combination into an
#' actual experiment that outputs a prediction for each day.
#'
#' @param archive the epi_df object
#' @param outcome the name of the target column
#' @param ahead the number of days ahead to forecast
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
#' @param cache_key a unique identifier for the cache file
#'
#' @importFrom epiprocess epix_slide
#' @importFrom cli cli_abort
#' @importFrom rlang !!
#' @export
slide_forecaster <- function(epi_archive,
                             outcome,
                             ahead,
                             extra_sources = "",
                             forecaster = scaled_pop,
                             slide_training = 0,
                             n_training_pad = 5,
                             forecaster_args = list(),
                             forecaster_args_names = list(),
                             date_range_step_size = 1L,
                             cache_key = NULL) {
  if (length(forecaster_args) > 0) {
    names(forecaster_args) <- forecaster_args_names
  }
  forecaster_args$ahead <- ahead
  if (!is.numeric(forecaster_args$n_training) && !is.null(forecaster_args$n_training)) {
    n_training <- as.numeric(forecaster_args$n_training)
    net_slide_training <- max(slide_training, n_training) + n_training_pad
  } else {
    n_training <- Inf
    net_slide_training <- slide_training + n_training_pad
  }
  # restrict the dataset to areas where training is possible
  start_date <- min(epi_archive$DT$time_value) + net_slide_training
  end_date <- max(epi_archive$DT$time_value) - forecaster_args$ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = date_range_step_size)

  # first generate the forecasts
  before <- n_training + n_training_pad - 1
  forecaster_args <- rlang::dots_list(
    !!!list(
      outcome = outcome,
      extra_sources = extra_sources
    ),
    !!!forecaster_args,
    .homonyms = "last"
  )
  forecaster_wrapper <- function(x) {
    inject(forecaster(epi_data = x, !!!forecaster_args))
  }
  res <- epix_slide_simple(
    epi_archive,
    forecaster_wrapper,
    valid_predict_dates,
    before,
    cache_key = cache_key
  )

  true_value <- epi_archive %>%
    epiprocess::epix_as_of(epi_archive$versions_end) %>%
    select(geo_value, time_value, !!outcome) %>%
    rename(true_value = !!outcome)
  res %>%
    inner_join(true_value,
      by = join_by(geo_value, target_end_date == time_value)
    )
}

epix_slide_simple <- function(epi_archive, forecaster, ref_time_values, before, cache_key = NULL) {
  purrr::map(ref_time_values, function(tv) {
    if (is.null(cache_key)) {
      epi_df <- epi_archive %>%
        epix_as_of(tv, min_time_value = tv - before) %>%
        forecaster()
    } else {
      dir.create(".exploration_cache/slide_cache", showWarnings = FALSE, recursive = TRUE)
      file_path <- glue::glue(".exploration_cache/slide_cache/{cache_key}_{before}_{tv}.parquet")
      if (file.exists(file_path)) {
        epi_df <- qs::qread(file_path)
      } else {
        epi_df <- epi_archive %>%
          epix_as_of(tv, min_time_value = tv - before)
         qs::qsave(epi_df, file_path)
      }
    }
    epi_df %>% forecaster()
  }) %>% bind_rows()
}
