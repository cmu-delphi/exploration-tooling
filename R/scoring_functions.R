# Scoring and Evaluation Functions
#
# The functions in this file are used to evaluate forecasts. They were ported
# from the evalcast package, see
#
#   https://cmu-delphi.github.io/covidcast/evalcastR/reference/evaluate_predictions.html
#

#' Create a score card data frame
#'
#' Evaluates the performance of a forecaster, through the following steps:
#' \enumerate{
#'   \item Takes a prediction card (as created by
#'   `get_predictions`).
#'   \item Computes various user-specified error measures.
#' }
#' The result is a "score card" data frame, where each row corresponds to
#' a prediction-result pair, where the columns define the prediction
#' task, give the observed value, and give the calculated values of the provided
#' error measures.
#'
#' @param predictions_cards tibble of quantile forecasts, which contains at
#'   least `quantile` and `value` columns, as well as any other prediction task
#'   identifiers. For covid data, a predictions card may be created by the
#'   function `get_predictions`, downloaded with `get_covidhub_predictions`
#'   or created manually.
#' @param err_measures Named list of one or more functions, where each function
#'   takes a data frame with three columns `quantile`, `value` and `actual`
#'   (i.e., observed) and returns a scalar measure of error. Null or an empty
#'   list may be provided if scoring is not desired.
#' @param truth_data truth data (observed). This should be a data frame that
#'   will be joined to `predictions_cards` by all available columns. The
#'   observed data column should be named `actual`.
#' @param grp_vars character vector of named columns in `predictions_cards`
#'  such that the combination gives a unique (quantile) prediction.
#'
#' @return tibble of "score cards". Contains the same information as the
#'   `predictions_cards()` with additional columns for each `err_measure` and
#'   for the truth (named `actual`).
#'
#' @importFrom rlang quo .data
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by summarize inner_join left_join mutate relocate across
#' @export
evaluate_predictions <- function(
    predictions_cards,
    truth_data,
    err_measures = list(
      wis = weighted_interval_score,
      ae = absolute_error,
      coverage_80 = interval_coverage(coverage = 0.8)
    ),
    grp_vars = c(
      "forecaster",
      intersect(colnames(predictions_cards), colnames(truth_data))
    )) {
  if ("actual" %in% names(predictions_cards)) {
    cli::cli_warn(
      c(
        "`predictions_cards` already has an `actual` column; this ",
        "`actual` column will be used and `truth_data` will be ignored."
      )
    )
  }
  assert_that(is.data.frame(truth_data),
    msg = paste("In evaluate_predictions: `truth_data` must be a
                          data frame")
  )
  assert_that("actual" %in% names(truth_data),
    msg = paste("`truth_data` must contain a column named `actual`")
  )
  predictions_cards <- left_join(predictions_cards, truth_data,
    by = intersect(colnames(predictions_cards), colnames(truth_data))
  )
  if (is.null(err_measures) || length(err_measures) == 0) {
    score_card <- predictions_cards
  } else {
    err_calls <- lapply(err_measures, function(err_measure) {
      quo(err_measure(.data$quantile, .data$value, .data$actual))
    })
    score_card <- predictions_cards %>%
      group_by(across(all_of(grp_vars))) %>%
      summarize(!!!err_calls, .groups = "drop") %>%
      inner_join(predictions_cards, by = grp_vars, multiple = "all")
  }
  class(score_card) <- c("score_cards", class(score_card))
  attributes(score_card) <- c(attributes(score_card),
    as_of = Sys.Date()
  )
  score_card <- collapse_cards(score_card)
  score_card %<>%
    select(-.data$quantile, -.data$value)

  score_card %<>%
    relocate(attr(err_measures, "names"), .after = last_col())
  return(score_card)
}


#' Remove all quantile forecasts
#'
#' @param cards either predictions_cards or scorecards
#'
#' @return cards of the same class but with only one row for each
#'   geo_value/forecast_date/ahead/forecaster (the point estimate)
#'
#' @importFrom assertthat assert_that
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter mutate relocate select
#'
#' @export
collapse_cards <- function(cards) {
  cls <- class(cards)[1]
  assert_that(cls %in% c("predictions_cards", "score_cards"),
    msg = paste(
      "This function is only appropriate for",
      "predictions_cards or score_cards classes."
    )
  )
  cards %<>%
    filter(abs(.data$quantile - 0.5) < 1e-8 | is.na(.data$quantile)) %>%
    mutate(quantile = ifelse(is.na(.data$quantile), "p", "m"))
  if (n_distinct(cards$quantile) == 1) {
    cards %<>%
      mutate(quantile = ifelse(.data$quantile == "p", NA, 0.5))
  } else {
    cards %<>%
      pivot_wider(names_from = .data$quantile, values_from = .data$value) %>%
      mutate(
        quantile = ifelse(is.na(.data$p), 0.5, NA),
        value = ifelse(is.na(.data$p), .data$m, .data$p)
      ) %>%
      select(-.data$p, -.data$m)
  }
  if ("geo_value" %in% colnames(cards)) {
    cards %<>%
      relocate(.data$quantile:.data$value, .after = .data$geo_value)
  }
  class(cards) <- c(cls, class(cards))
  cards
}



#' Compute weighted interval score
#'
#' Computes weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#'
#' @export
weighted_interval_score <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "weighted_interval_score")
  if (all(is.na(actual_value))) {
    return(NA)
  }
  # `score_func_param_checker` above has already checked for uniqueness, so we
  # can save a bit of effort and just take the first actual.
  actual_value <- actual_value[[1L]]

  value <- value[!is.na(quantile)]
  quantile <- quantile[!is.na(quantile)]

  # per Ryan: WIS is equivalent to quantile loss modulo an extra 0.5 AE term
  # for the median forecast (counted twice).
  #
  # update: WIS is now being redefined to match exactly, still some question
  # about the correct denominator but the formula seems to be  1 / (K + 0.5)
  #
  # Finally, the multiplication by 2 is because alpha_k = 2*quantile_k
  #
  med <- value[find_quantile_match(quantile, 0.5)]

  if (length(med) > 1L) {
    return(NA)
  }

  wis <- 2 * mean(pmax(
    quantile * (actual_value - value),
    (1 - quantile) * (value - actual_value),
    na.rm = TRUE
  ))

  return(wis)
}

#' Compute absolute error
#'
#' Absolute error of a forecaster
#'
#'
#' Intended to be used with `evaluate_predictions()`, it expects three arguments
#' of the same length, finds the location of the point forecast, and returns
#' the absolute error.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value vector of actual values of the same length as
#'   `quantile`/`value` or a scalar
#'
#' @export
absolute_error <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "absolute_error")
  point_fcast <- which(is.na(quantile))
  ae <- abs(actual_value - value)
  if (length(point_fcast) == 1L) {
    return(ae[point_fcast])
  }
  point_fcast <- which(find_quantile_match(quantile, 0.5))
  if (length(point_fcast) == 1L) {
    return(ae[point_fcast])
  }
  warning(paste(
    "Absolute error: Forecaster must return either a point forecast",
    "with quantile == NA or a median with quantile == 0.5",
    "Returning NA."
  ))
  return(NA)
}

#' Generate interval coverage error measure function
#'
#' Returns an error measure function indicating whether a central interval
#' covers the actual value. The interval is defined as the (alpha/2)-quantile
#' to the (1 - alpha/2)-quantile, where alpha = 1 - coverage.
#'
#' @param coverage Nominal interval coverage (from 0 to 1).
#'
#' @export
interval_coverage <- function(coverage) {
  function(quantiles, value, actual_value) {
    score_func_param_checker(quantiles, value, actual_value, "interval_coverage")
    value <- value[!is.na(quantiles)]
    quantiles <- quantiles[!is.na(quantiles)]
    alpha <- 1 - coverage
    lower_interval <- alpha / 2
    upper_interval <- 1 - (alpha / 2)
    if (!any(find_quantile_match(quantiles, lower_interval)) |
      !any(find_quantile_match(quantiles, upper_interval))) {
      warning(paste(
        "Interval Coverage:",
        "Quantiles must cover an interval of specified width",
        "centered at 0.5. Returning NA."
      ))
      return(NA)
    }

    lower <- value[which(find_quantile_match(quantiles, lower_interval))]
    upper <- value[which(find_quantile_match(quantiles, upper_interval))]
    return(actual_value[[1L]] >= lower & actual_value[[1L]] <= upper)
  }
}

#' Overprediction component of the weighted interval score
#'
#' Requires symmetric quantile forecasts. Roughly, a penalty for predicted
#' quantiles smaller than .5 falling above the observed value.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#'
#' @export
overprediction <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "overprediction")
  if (!is_symmetric(quantile)) {
    warning(paste0(
      "overprediction/underprediction/sharpness require",
      "symmetric quantile forecasts. Using NA."
    ))
    return(NA)
  }
  if (all(is.na(actual_value))) {
    return(NA)
  }
  # `score_func_param_checker` above has already checked for uniqueness, so we
  # can save a bit of effort and just take the first actual.
  actual_value <- actual_value[[1L]]

  lower <- value[!is.na(quantile) & quantile < .5]
  med <- value[find_quantile_match(quantile, 0.5)]

  if (length(med) > 1L) {
    return(NA)
  }
  m <- ifelse(length(med == 1L),
    (med - actual_value) * (med > actual_value),
    NULL
  )

  ans <- mean(c(
    rep((lower - actual_value) * (lower > actual_value), 2), m
  ))

  return(ans)
}


#' Underprediction component of the weighted interval score
#'
#' Requires symmetric quantile forecasts. Roughly, a penalty for predicted
#' quantiles larger than .5 falling under the observed value.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#'
#' @export
underprediction <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "underprediction")
  if (!is_symmetric(quantile)) {
    warning(paste0(
      "overprediction/underprediction/sharpness require",
      "symmetric quantile forecasts. Using NA."
    ))
    return(NA)
  }
  if (all(is.na(actual_value))) {
    return(NA)
  }
  # `score_func_param_checker` above has already checked for uniqueness, so we
  # can save a bit of effort and just take the first actual.
  actual_value <- actual_value[[1L]]

  upper <- value[!is.na(quantile) & quantile > .5]
  med <- value[find_quantile_match(quantile, 0.5)]

  if (length(med) > 1L) {
    return(NA)
  }
  m <- ifelse(length(med == 1L),
    (actual_value - med) * (med < actual_value),
    NULL
  )
  ans <- mean(c(
    rep((actual_value - upper) * (upper < actual_value), 2), m
  ))

  return(ans)
}
#' Sharpness component of the weighted interval score
#'
#' Requires symmetric quantile forecasts. Roughly, a penalty for the
#' width of predicted quantiles.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#'
#' @export

sharpness <- function(quantile, value, actual_value) {
  weighted_interval_score(quantile, value, actual_value) -
    overprediction(quantile, value, actual_value) -
    underprediction(quantile, value, actual_value)
}


#' Common parameter checks for score functions
#'
#' A set of common checks for score functions, meant to identify common causes
#' of issues. Avoids `assert_that` for speed.
#'
#' @param quantiles vector of forecasted quantiles
#' @param values vector of forecasted values
#' @param actual_value actual_value, either as a scalar or a vector of the same
#'   length as `quantiles` and `values`
#' @param id string to identify the caller of the function and displayed in
#'   error messages (recommended to be the parent function's name)
score_func_param_checker <- function(quantiles, values, actual_value, id = "") {
  id_str <- paste0(id, ": ")
  if (length(actual_value) > 1) {
    if (length(actual_value) != length(values)) {
      stop(paste0(
        id_str,
        "actual_value must be a scalar or the same length",
        " as values"
      ))
    }
    actual_value <- unique(actual_value)
  }

  if (length(actual_value) != 1) {
    stop(paste0(
      id_str,
      "actual_value must have exactly 1 unique value"
    ))
  }
  if (length(quantiles) != length(values)) {
    stop(paste0(
      id_str,
      "quantiles and values must be of the same length"
    ))
  }

  if (anyDuplicated(quantiles)) {
    stop(paste0(
      id_str,
      "quantiles must be unique."
    ))
  }
}

is_symmetric <- function(x, tol = 1e-8) {
  # Checking if `x` is sorted is much faster than trying to sort it again
  if (is.unsorted(x, na.rm = TRUE)) {
    # Implicitly drops NA values
    x <- sort(x)
  } else {
    # Match `sort` behavior
    x <- x[!is.na(x)]
  }
  all(abs(x + rev(x) - 1) < tol)
}


find_quantile_match <- function(quantiles, val_to_match, tol = 1e-8) {
  return(abs(quantiles - val_to_match) < tol & !is.na(quantiles))
}

#' import prediction cards generated elsewhere
#' @description
#' load an externally generated RDS to be evaluated
#' @param predictions_filename the filename to be read as an RDS
#' @param forecaster_name the name to assign the forecaster
#' @export
read_external_predictions_data <- function(predictions_filename, forecaster_name = NULL) {
  prediction_cards <- readRDS(predictions_filename)
  if (is.null(forecaster_name)) {
    return(prediction_cards)
  }
  prediction_cards %>%
    filter(forecast_date >= "2023-06-01") %>%
    filter(forecaster == forecaster_name)
}

#' evaluate_predictions wrapper
#' @description
#' run the measures on `data`, with truth data `evaluation_data`
#' @param data a prediction card to be scored
#' @param evaluation_data the true values
#' @param measures a set of scores to be used
#' @export
run_evaluation_measure <- function(data, evaluation_data, measures) {
  data %>%
    evaluate_predictions(
      evaluation_data,
      err_measures = measures,
      grp_vars = c(
        "signal",
        "geo_value",
        "forecast_date",
        "target_end_date"
      )
    )
}
