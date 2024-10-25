#' this is a hack that is a copy of arx_args_list. The main difference is not
#' checking the sign for ahead, and some new parameters
#' @param keys_to_ignore instead of being a named list of vectors, the first
#'   list is the columns used, and then it moves on to normal vectors for the
#'   rest
default_args_list <- function(
    lags = c(0L, 7L, 14L),
    ahead = 7L,
    n_training = Inf,
    forecast_date = NULL,
    target_date = NULL,
    adjust_latency = c("extend_lags", "locf", "none", "extend_ahead"),
    warn_latency = TRUE,
    quantile_levels = c(0.05, 0.95),
    symmetrize = TRUE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    check_enough_data_n = NULL,
    check_enough_data_epi_keys = NULL,
    keys_to_ignore = list(),
    ...) {
  # error checking if lags is a list
  rlang::check_dots_empty()
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  adjust_latency <- rlang::arg_match(adjust_latency)
  epipredict:::arg_is_scalar(ahead, n_training, symmetrize, nonneg, adjust_latency, warn_latency)
  epipredict:::arg_is_chr(quantile_by_key, allow_empty = TRUE)
  epipredict:::arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  epipredict:::arg_is_date(forecast_date, target_date, allow_null = TRUE)
  epipredict:::arg_is_nonneg_int(lags)
  epipredict:::arg_is_lgl(symmetrize, nonneg)
  epipredict:::arg_is_probabilities(quantile_levels, allow_null = TRUE)
  epipredict:::arg_is_pos(n_training)
  if (is.finite(n_training)) epipredict:::arg_is_pos_int(n_training)
  epipredict:::arg_is_pos(check_enough_data_n, allow_null = TRUE)
  epipredict:::arg_is_chr(check_enough_data_epi_keys, allow_null = TRUE)

  if (!is.null(forecast_date) && !is.null(target_date)) {
    if (forecast_date + ahead != target_date) {
      cli_abort("`forecast_date` {.val {forecast_date}} + `ahead` {.val {ahead}} must equal `target_date` {.val {target_date}}.",
        class = "epipredict__arx_args__inconsistent_target_ahead_forecaste_date"
      )
    }
  }
  # putting the first vector as the names of the list of the rest
  key_colnames <- keys_to_ignore[[1]][[1]]
  keys_to_ignore <- keys_to_ignore[[1]][-1]
  names(keys_to_ignore) <- key_colnames

  max_lags <- max(lags)
  structure(
    epipredict:::enlist(
      lags = .lags,
      ahead,
      n_training,
      quantile_levels,
      forecast_date,
      target_date,
      adjust_latency,
      warn_latency,
      symmetrize,
      nonneg,
      max_lags,
      quantile_by_key,
      check_enough_data_n,
      check_enough_data_epi_keys,
      keys_to_ignore
    ),
    class = c("arx_fcast", "alist")
  )
}

# Generated from function body. Editing this file has no effect.
default_flatline_args <- function(
    ahead = 7L, n_training = Inf, forecast_date = NULL,
    target_date = NULL, quantile_levels = c(0.05, 0.95), symmetrize = TRUE,
    nonneg = TRUE, quantile_by_key = character(0L), ...) {
  rlang::check_dots_empty()
  epipredict:::arg_is_scalar(ahead, n_training)
  epipredict:::arg_is_chr(quantile_by_key, allow_empty = TRUE)
  epipredict:::arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  epipredict:::arg_is_date(forecast_date, target_date, allow_null = TRUE)
  epipredict:::arg_is_lgl(symmetrize, nonneg)
  epipredict:::arg_is_probabilities(quantile_levels, allow_null = TRUE)
  epipredict:::arg_is_pos(n_training)
  if (is.finite(n_training)) {
    epipredict:::arg_is_pos_int(n_training)
  }
  if (!is.null(forecast_date) && !is.null(target_date)) {
    if (forecast_date + ahead != target_date) {
      cli_warn(c("`forecast_date` + `ahead` must equal `target_date`.",
        i = "{.val {forecast_date}} + {.val {ahead}} != {.val {target_date}}."
      ))
    }
  }
  structure(
    epipredict:::enlist(
      ahead, n_training, forecast_date, target_date,
      quantile_levels, symmetrize, nonneg, quantile_by_key
    ),
    class = c("flat_fcast", "alist")
  )
}
