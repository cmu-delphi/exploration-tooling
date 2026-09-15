#' Add rolling mean and standard deviation columns
#'
#' A recipe step that adds rolling mean and/or rolling SD columns for named
#' predictor columns. Output column names follow the convention from
#' [rolling_mean] and [rolling_sd]: `slide_{col}_m{width}` for means and
#' `slide_{col}_sd{width}` for SDs. At least one of `mean_width` or `sd_width`
#' must be non-`NULL`.
#'
#' Because [rolling_mean] and [rolling_sd] operate on the incoming `epi_df`
#' using the full window of available history, the test data passed to
#' [predict] must contain enough prior rows for the window. The
#' [run_workflow_and_format] helper's `test_data_interval` (default 52 weeks)
#' covers all practical window sizes.
#'
#' @param recipe an [epipredict::epi_recipe].
#' @param colname character vector of columns to compute rolling stats for.
#' @param mean_width integer or difftime; window size for rolling mean. `NULL`
#'   skips the mean.
#' @param sd_width integer or difftime; window size for rolling SD. `NULL`
#'   skips the SD.
#' @param sd_mean_width integer or difftime; window size for the internal mean
#'   used in SD computation. Defaults to `ceiling(sd_width / 2)` when `NULL`.
#' @param keep_mean logical; if `TRUE`, retain the intermediate mean column when
#'   computing the SD.
#' @inheritParams step_epi_lag
#' @template step-return
#' @seealso [rolling_mean], [rolling_sd]
#' @export
step_epi_rolling_stats <- function(
  recipe,
  colname,
  mean_width = NULL,
  sd_width = NULL,
  sd_mean_width = NULL,
  keep_mean = FALSE,
  id = rand_id("epi_rolling_stats")
) {
  if (is.null(mean_width) && is.null(sd_width)) {
    cli::cli_abort("At least one of `mean_width` or `sd_width` must be non-NULL.")
  }
  add_step(
    recipe,
    step_epi_rolling_stats_new(
      colname = colname,
      mean_width = mean_width,
      sd_width = sd_width,
      sd_mean_width = sd_mean_width,
      keep_mean = keep_mean,
      metadata = NULL,
      trained = FALSE,
      role = NA,
      skip = FALSE,
      id = id
    )
  )
}

step_epi_rolling_stats_new <- function(
  colname, mean_width, sd_width, sd_mean_width, keep_mean,
  metadata, trained, role, skip, id
) {
  step(
    subclass = "epi_rolling_stats",
    colname = colname,
    mean_width = mean_width,
    sd_width = sd_width,
    sd_mean_width = sd_mean_width,
    keep_mean = keep_mean,
    metadata = metadata,
    trained = trained,
    role = role,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_epi_rolling_stats <- function(x, training, info = NULL, ...) {
  hardhat::validate_column_names(training, x$colname)
  # epi_slide_mean requires difftime; resolve integer widths using the data's time_type.
  meta <- attr(training, "metadata")
  time_type <- meta$time_type %||% "week"
  units <- if (time_type == "day") "days" else "weeks"
  resolve_width <- function(w) {
    if (is.null(w) || inherits(w, "difftime")) return(w)
    as.difftime(w, units = units)
  }
  step_epi_rolling_stats_new(
    colname = x$colname,
    mean_width = resolve_width(x$mean_width),
    sd_width = resolve_width(x$sd_width),
    sd_mean_width = resolve_width(x$sd_mean_width),
    keep_mean = x$keep_mean,
    metadata = meta,
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_rolling_stats <- function(object, new_data, ...) {
  # hardhat::forge strips epi_df class before bake; restore it so rolling_mean
  # and rolling_sd can call key_colnames on the data.
  if (!is.null(object$metadata) && !inherits(new_data, "epi_df")) {
    new_data <- epiprocess::as_epi_df(
      new_data,
      other_keys = object$metadata$other_keys,
      as_of = object$metadata$as_of
    )
  }
  if (!is.null(object$mean_width)) {
    new_data <- rolling_mean(new_data, width = object$mean_width, cols_to_mean = object$colname)
  }
  if (!is.null(object$sd_width)) {
    new_data <- rolling_sd(
      new_data,
      sd_width = object$sd_width,
      mean_width = object$sd_mean_width,
      cols_to_sd = object$colname,
      keep_mean = object$keep_mean
    )
  }
  new_data
}

#' @export
print.step_epi_rolling_stats <- function(x, width = max(20, options()$width - 30), ...) {
  parts <- c(
    if (!is.null(x$mean_width)) glue::glue("mean(w={x$mean_width})"),
    if (!is.null(x$sd_width)) glue::glue("sd(w={x$sd_width})")
  )
  title <- glue::glue("Rolling {paste(parts, collapse=', ')} for:")
  recipes::print_step(x$colname, rlang::enquos(x$colname), x$trained, title, width)
  invisible(x)
}
