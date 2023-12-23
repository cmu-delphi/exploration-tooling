# various reusable transforms to apply before handing to epipredict

#' extract the non-key, non-smoothed columns from epi_data
#' @keywords internal
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

#' just the names which aren't keys for an epi_df
#' @description
#' names, but it excludes keys
#' @param epi_data the epi_df
get_nonkey_names <- function(epi_data) {
  cols <- names(epi_data)
  cols <- cols[!(cols %in% c("geo_value", "time_value", attr(epi_data, "metadata")$other_keys))]
  return(cols)
}


#' update the predictors to only contain the smoothed/sd versions of cols
#' @description
#' modifies the list of preditors so that any which have been modified have the
#'   modified versions included, and not the original. Should only be applied
#'   after both rolling_mean and rolling_sd.
#' @param epi_data the epi_df, only included to get the non-key column names
#' @param cols_modified the list of columns which have been modified. If this is `NULL`, that means we were modifying every column.
#' @param predictors the initial set of predictors; any unmodified are kept, any modified are replaced with the modified versions (e.g. "a" becoming "a_m17").
#' @importFrom purrr map map_chr reduce
#' @return returns an updated list of predictors, with modified columns replaced and non-modified columns left intact.
#' @export
update_predictors <- function(epi_data, cols_modified, predictors) {
  if (!is.null(cols_modified)) {
    # if cols_modified isn't null, make sure we include predictors that weren't modified
    unchanged_predictors <- map(cols_modified, ~ !grepl(.x, predictors, fixed = TRUE)) %>% reduce(`&`)
    unchanged_predictors <- predictors[unchanged_predictors]
  } else {
    # if it's null, we've modified every predictor
    unchanged_predictors <- character(0L)
  }
  # all the non-key names
  col_names <- get_nonkey_names(epi_data)
  is_present <- function(original_predictor) {
    grepl(original_predictor, col_names) & !(col_names %in% predictors)
  }
  is_modified <- map(predictors, is_present) %>% reduce(`|`)
  new_predictors <- col_names[is_modified]
  return(c(unchanged_predictors, new_predictors))
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
#' @importFrom epiprocess epi_slide
#' @export
rolling_mean <- function(epi_data, width = 7L, cols_to_mean = NULL) {
  cols_to_mean <- get_trainable_names(epi_data, cols_to_mean)
  epi_data %<>% group_by(geo_value)
  for (col in cols_to_mean) {
    mean_name <- paste0(col, "_m", width)
    epi_data %<>% epi_slide(~ mean(.x[[col]], rm.na = TRUE), before = width-1L, new_col_name = mean_name)
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
    result %<>% epi_slide(~ mean(.x[[col]], na.rm = TRUE), before = mean_width-1L, new_col_name = mean_name)
    result %<>% epi_slide(~ sqrt(mean((.x[[mean_name]] - .x[[col]])^2, na.rm = TRUE)), before = sd_width-1, new_col_name = sd_name)
    if (!keep_mean) {
      # TODO make sure the extra info sticks around
      result %<>% select(-{{ mean_name }})
    }
    result %<>% dplyr_reconstruct(epi_data)
  }
  result %<>% ungroup()
}
