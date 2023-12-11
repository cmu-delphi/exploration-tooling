# various reusable transforms to apply before handing to epipredict

#' extract the non-key, non-smoothed columns from epi_data
#' @keywords internal
#' @param epi_data the epi_data tibble
#' @param cols vector of column names to use. If `NULL`, fill with all non-key columns
get_trainable_names <- function(epi_data, cols) {
  if (is.null(cols)) {
    cols <- names(epi_data)
    cols <- cols[!(cols %in% c("geo_value", "time_value", attr(epi_data, "metadata")$other_keys))]
    # exclude anything with the same naming schema as the rolling average/sd created below
    cols <- cols[!grepl("_\\w{1,2}\\d+", cols)]
  }
  return(cols)
}

#' get a rolling average for the named columns
#' @description
#' add column(s) that are the rolling means of the specified columns, as
#'   implemented by slider. Defaults to the previous 7 days.
#' Currently only group_by's on the geo_value. Should probably extend to more
#'   keys if you have them
#' @param epi_data the dataset
#' @param width the number of days (or examples, the sliding isn't time-aware) to use
#' @param cols_to_mean the non-key columns to take the mean over. `NULL` means all
#' @importFrom slider slide_dbl
#' @export
rolling_mean <- function(epi_data, width = 7L, cols_to_mean = NULL) {
  cols_to_mean <- get_trainable_names(epi_data, cols_to_mean)
  epi_data %<>% group_by(geo_value)
  for (col in cols_to_mean) {
    mean_name <- paste0(col, "_m", width)
    epi_data %<>% mutate({{ mean_name }} := slider::slide_dbl(.data[[col]], mean, .before = width))
  }
  epi_data %<>% ungroup()
  return(epi_data)
}

#' get a rolling standard deviation for the named columns
#' @description
#' A rolling standard deviation, based off of a rolling mean. First it
#'   calculates a rolling mean with width `mean_width`, and then squares the
#'   difference between that and the actual value, averaged over `sd_width`.
#' @param epi_data the dataset
#' @param sd_width the number of days (or examples, the sliding isn't
#'   time-aware) to use for the standard deviation calculation
#' @param mean_width like `sd_width`, but it governs the mean. Should be less
#'   than the `sd_width`, and if `NULL` (the default) it is half of `sd_width`
#'   (so 14 in the complete default case)
#' @param cols_to_sd the non-key columns to take the sd over. `NULL` means all
#' @param keep_mean bool, if `TRUE`, it retains keeps the mean column
#' @importFrom slider slide_dbl slide2_dbl
#' @export
rolling_sd <- function(epi_data, sd_width = 28L, mean_width = NULL, cols_to_sd = NULL, keep_mean = FALSE) {
  if (is.null(mean_width)) {
    mean_width <- as.integer(ceiling(sd_width / 2))
  }
  cols_to_sd <- get_trainable_names(epi_data, cols_to_sd)
  epi_data %<>% group_by(geo_value)
  for (col in cols_to_sd) {
    mean_name <- paste0(col, "_m", mean_width)
    sd_name <- paste0(col, "_SD", sd_width)
    epi_data %<>% mutate({{ mean_name }} := slider::slide_dbl(.data[[col]], mean, .before = mean_width))
    epi_data %<>% mutate({{ sd_name }} := slider::slide2_dbl(.data[[col]], .data[[mean_name]], ~ sqrt(mean((.x - .y)^2)), .before = sd_width))
    if (!keep_mean) {
      epi_data %<>% select(-{{ mean_name }})
    }
  }
  epi_data %<>% ungroup()
  return(epi_data)
}
