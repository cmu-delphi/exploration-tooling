#' Add polynomial coefficient features via sliding windows
#'
#' A recipe step that fits a degree-`degree` polynomial to multiple trailing
#' windows of a single column and adds the coefficients as new predictor
#' columns. Each named entry in `windows` produces `degree + 1` columns named
#' `{window_name}_c1`, `{window_name}_c2`, ..., `{window_name}_c{degree+1}`.
#'
#' All windows are computed in a single [epiprocess::epi_slide] call over
#' `slide_window`. Rows with fewer than `n_points` non-NA values in the window
#' return all-NA coefficients for that window (see [get_poly_coefs]).
#'
#' @param recipe an [epipredict::epi_recipe].
#' @param colname character scalar; the column to fit polynomials to.
#' @param windows named integer vector of trailing window sizes, e.g.
#'   `c(quad4 = 4, quad6 = 6, lin3 = 3, lin5 = 5)`. Names become the prefix in
#'   output column names.
#' @param degree integer; polynomial degree. Default 2.
#' @param slide_window difftime or integer; the [epiprocess::epi_slide] window
#'   (how far back the slice reaches). Must be >= the largest value in
#'   `windows`.
#' @inheritParams step_epi_lag
#' @template step-return
#' @seealso [get_poly_coefs]
#' @export
step_epi_poly_coefs <- function(
  recipe,
  colname,
  windows,
  degree = 2L,
  slide_window = as.difftime(6, units = "weeks"),
  id = rand_id("epi_poly_coefs")
) {
  if (is.null(names(windows)) || any(names(windows) == "")) {
    cli::cli_abort("`windows` must be a fully named integer vector.")
  }
  add_step(
    recipe,
    step_epi_poly_coefs_new(
      colname = colname,
      windows = windows,
      degree = as.integer(degree),
      slide_window = slide_window,
      metadata = NULL,
      trained = FALSE,
      role = NA,
      skip = FALSE,
      id = id
    )
  )
}

step_epi_poly_coefs_new <- function(
  colname, windows, degree, slide_window,
  metadata, trained, role, skip, id
) {
  step(
    subclass = "epi_poly_coefs",
    colname = colname,
    windows = windows,
    degree = degree,
    slide_window = slide_window,
    metadata = metadata,
    trained = trained,
    role = role,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_epi_poly_coefs <- function(x, training, info = NULL, ...) {
  hardhat::validate_column_names(training, x$colname)
  step_epi_poly_coefs_new(
    colname = x$colname,
    windows = x$windows,
    degree = x$degree,
    slide_window = x$slide_window,
    metadata = attr(training, "metadata"),
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_poly_coefs <- function(object, new_data, ...) {
  # hardhat::forge strips epi_df class before bake; restore it so epi_slide
  # can call key_colnames on the data.
  if (!is.null(object$metadata) && !inherits(new_data, "epi_df")) {
    new_data <- epiprocess::as_epi_df(
      new_data,
      other_keys = object$metadata$other_keys,
      as_of = object$metadata$as_of
    )
  }
  col <- object$colname
  windows <- object$windows
  degree <- object$degree

  new_data %>%
    group_by(across(key_colnames(new_data, exclude = "time_value"))) %>%
    epi_slide(
      .f = function(x, gk, rtv) {
        rel_col <- x[[col]]
        results <- purrr::imap(windows, function(n_pts, win_name) {
          get_poly_coefs(tail(rel_col, n = n_pts), degree, n_pts) %>%
            setNames(paste0(win_name, "_c", seq_len(degree + 1L)))
        })
        bind_cols(results)
      },
      .window_size = object$slide_window
    ) %>%
    ungroup()
}

#' @export
print.step_epi_poly_coefs <- function(x, width = max(20, options()$width - 30), ...) {
  title <- glue::glue("Degree-{x$degree} polynomial coefficients for:")
  recipes::print_step(x$colname, rlang::enquos(x$colname), x$trained, title, width)
  invisible(x)
}
