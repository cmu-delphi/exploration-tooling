# Reusable transforms to apply before handing to epipredict

#' Extract the non-key, non-smoothed columns from epi_data
#'
#' @param epi_data the `epi_df`
#' @param cols vector of column names to use. If `NULL`, fill with all non-key columns
get_trainable_names <- function(epi_data, cols) {
  if (is.null(cols)) {
    cols <- get_nonkey_names(epi_data)
    # exclude anything with the same naming schema as the rolling average/sd created below
    cols <- cols[!grepl("_\\w{1,2}\\d+", cols)]
  }
  return(cols)
}

#' Just the names which aren't keys for an epi_df
#'
#' Names, but it excludes keys
#'
#' @param epi_data the epi_df
get_nonkey_names <- function(epi_data) {
  cols <- names(epi_data)
  cols <- cols[!(cols %in% c("geo_value", "time_value", attr(epi_data, "metadata")$other_keys))]
  return(cols)
}

#' Get a rolling average for the named columns
#'
#' Add column(s) that are the rolling means of the specified columns, as
#' implemented by slider. Defaults to the previous 7 days. Currently only
#' group_by's on the geo_value. Should probably extend to more keys if you have
#' them.
#'
#' @param epi_data the dataset
#' @param width the number of days (or examples, the sliding isn't time-aware) to use
#' @param cols_to_mean the non-key columns to take the mean over. `NULL` means all
#'
#' @importFrom slider slide_dbl
#' @importFrom epiprocess epi_slide
#' @export
rolling_mean <- function(epi_data, width = 7L, cols_to_mean = NULL) {
  cols_to_mean <- get_trainable_names(epi_data, cols_to_mean)
  epi_data %<>% group_by(geo_value)
  for (col in cols_to_mean) {
    mean_name <- paste0(col, "_m", width)
    epi_data %<>% epi_slide(~ mean(.x[[col]], rm.na = TRUE), before = width - 1L, new_col_name = mean_name)
  }
  epi_data %<>% ungroup()
  return(epi_data)
}

#' Get a rolling standard deviation for the named columns
#'
#' A rolling standard deviation, based off of a rolling mean. First it
#' calculates a rolling mean with width `mean_width`, and then squares the
#' difference between that and the actual value, averaged over `sd_width`.
#'
#' @param epi_data the dataset
#' @param sd_width the number of days (or examples, the sliding isn't
#'   time-aware) to use for the standard deviation calculation
#' @param mean_width like `sd_width`, but it governs the mean. Should be less
#'   than the `sd_width`, and if `NULL` (the default) it is half of `sd_width`
#'   (so 14 in the complete default case)
#' @param cols_to_sd the non-key columns to take the sd over. `NULL` means all
#' @param keep_mean bool, if `TRUE`, it retains keeps the mean column
#'
#' @importFrom epiprocess epi_slide
#' @export
rolling_sd <- function(epi_data, sd_width = 28L, mean_width = NULL, cols_to_sd = NULL, keep_mean = FALSE) {
  if (is.null(mean_width)) {
    mean_width <- as.integer(ceiling(sd_width / 2))
  }
  cols_to_sd <- get_trainable_names(epi_data, cols_to_sd)
  result <- epi_data
  for (col in cols_to_sd) {
    result %<>% group_by(geo_value)
    mean_name <- paste0(col, "_m", mean_width)
    sd_name <- paste0(col, "_sd", sd_width)
    result %<>% epi_slide(~ mean(.x[[col]], na.rm = TRUE), before = mean_width - 1L, new_col_name = mean_name)
    result %<>% epi_slide(~ sqrt(mean((.x[[mean_name]] - .x[[col]])^2, na.rm = TRUE)), before = sd_width - 1, new_col_name = sd_name)
    if (!keep_mean) {
      # select currently turns result into a tibble
      result %<>% select(-{{ mean_name }})
    }
    # convert back to an epi_df from a tibble
    result %<>% dplyr_reconstruct(epi_data)
  }
  result %<>% ungroup()
}

#' Temporary patch that pulls `NA`'s out of an epi_df
#'
#' Just delete rows that have NA's in them. eventually epipredict should
#' directly handle this so we don't have to
#'
#' @param epi_data the epi_df to be fixed
#' @param outcome the column name containing the target variable
#' @param extra_sources any other columns used as predictors
#'
#' @importFrom tidyr drop_na
#' @importFrom epiprocess as_epi_df
#' @export
clear_lastminute_nas <- function(epi_data, outcome, extra_sources) {
  meta_data <- attr(epi_data, "metadata")
  if (extra_sources == c("")) {
    extra_sources <- character(0L)
  }
  epi_data %<>%
    drop_na(c(!!outcome, !!!extra_sources)) %>%
    as_epi_df()
  attr(epi_data, "metadata") <- meta_data
  return(epi_data)
}

#' Only extend the ahead
#'
#' Instead of filling in new values, this just extends how far into the future
#' the model is predicting. For example, if the last data is on the 3rd, the
#' `as_of` is the 5th, and we want an ahead of 4, then this actually sets the
#' ahead to be 6, since the 9th (the target date) is 6 days after the last day
#' of data.
#'
#' @param epi_data the dataset
#' @param ahead how many units (depending on the dataset, normally days or
#' weeks) to predict ahead of the `forecast_date`
#'
#' @export
extend_ahead <- function(epi_data, ahead) {
  time_values <- epi_data$time_value
  if (length(time_values) > 0) {
    as_of <- attributes(epi_data)$metadata$as_of
    max_time <- max(time_values)
    if (is.null(as_of)) {
      as_of <- max_time
    }
    effective_ahead <- as.integer(
      as.Date(as_of) - max_time + ahead
    )
  } else {
    effective_ahead <- Inf
  }
  return(list(epi_data, effective_ahead))
}
