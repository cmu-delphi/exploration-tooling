#' Whiten predictor columns per (source, geo_value) group
#'
#' A recipe step that applies per-(source, geo_value) whitening — centering,
#' scaling, and optional nonlinear transform — to named columns. Parameters are
#' learned from the training data during [prep] and applied identically to bake-
#' time data, so the train/test split is respected automatically.
#'
#' Pair with [layer_epi_coloring] in a [epipredict::frosting] to reverse the
#' transform on predictions.
#'
#' @param recipe an [epipredict::epi_recipe].
#' @param colname character vector of column names to whiten.
#' @param learn_from character scalar; if set, learns whitening params from this
#'   column but applies them to all columns in `colname`. Useful when multiple
#'   columns represent the same underlying variable at different lags (e.g.
#'   revision design matrices). `NULL` learns from each column independently.
#' @param scale_method,center_method,nonlin_method whitening parameters, passed
#'   to [calculate_whitening_params]. `scale_method = "none"` is a no-op.
#' @inheritParams step_epi_lag
#' @template step-return
#' @seealso [layer_epi_coloring], [calculate_whitening_params]
#' @export
step_epi_whitening <- function(
  recipe,
  colname,
  learn_from = NULL,
  scale_method = c("quantile", "quantile_upper", "std", "none"),
  center_method = c("median", "mean", "none"),
  nonlin_method = c("quart_root", "none"),
  id = rand_id("epi_whitening")
) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  add_step(
    recipe,
    step_epi_whitening_new(
      colname = colname,
      learn_from = learn_from,
      scale_method = scale_method,
      center_method = center_method,
      nonlin_method = nonlin_method,
      learned_params = NULL,
      join_cols = NULL,
      trained = FALSE,
      role = NA,
      skip = FALSE,
      id = id
    )
  )
}

step_epi_whitening_new <- function(
  colname, learn_from, scale_method, center_method, nonlin_method,
  learned_params, join_cols, trained, role, skip, id
) {
  step(
    subclass = "epi_whitening",
    colname = colname,
    learn_from = learn_from,
    scale_method = scale_method,
    center_method = center_method,
    nonlin_method = nonlin_method,
    learned_params = learned_params,
    join_cols = join_cols,
    trained = trained,
    role = role,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_epi_whitening <- function(x, training, info = NULL, ...) {
  hardhat::validate_column_names(training, x$colname)
  if (!is.null(x$learn_from)) {
    hardhat::validate_column_names(training, x$learn_from)
  }
  # calculate_whitening_params groups by source + geo_value; both must be present.
  if (!("source" %in% names(training))) {
    cli::cli_abort(
      "step_epi_whitening requires a `source` column; add one before this step."
    )
  }
  join_cols <- c("source", "geo_value")

  learn_cols <- x$learn_from %||% x$colname
  raw_params <- calculate_whitening_params(
    training, learn_cols, x$scale_method, x$center_method, x$nonlin_method
  )
  learned_params <- if (!is.null(x$learn_from) && !is.null(raw_params)) {
    replicate_whitening_params(raw_params, x$learn_from, x$colname)
  } else {
    raw_params
  }

  step_epi_whitening_new(
    colname = x$colname,
    learn_from = x$learn_from,
    scale_method = x$scale_method,
    center_method = x$center_method,
    nonlin_method = x$nonlin_method,
    learned_params = learned_params,
    join_cols = join_cols,
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_whitening <- function(object, new_data, ...) {
  meta <- attributes(new_data)$metadata
  result <- data_whitening(
    new_data, object$colname, object$learned_params,
    object$nonlin_method, object$join_cols
  )
  # left_join inside data_whitening drops the epi_df subclass; restore it.
  if (!is.null(meta) && !inherits(result, "epi_df")) {
    result <- epiprocess::as_epi_df(
      result,
      other_keys = meta$other_keys,
      as_of = meta$as_of
    )
  }
  result
}

#' @export
print.step_epi_whitening <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Whitening columns:"
  recipes::print_step(x$colname, rlang::enquos(x$colname), x$trained, title, width)
  invisible(x)
}

# --- Paired frosting layer ---------------------------------------------------

#' Reverse whitening on prediction output
#'
#' A frosting layer that reverses [step_epi_whitening] on `.pred` and
#' `.pred_distn` by retrieving the learned whitening params from the trained
#' recipe step via [workflows::extract_recipe]. Must appear in the frosting
#' after [layer_predict] (and after [layer_quantile_distn] /
#' [layer_point_from_distn] for quantile trainers).
#'
#' @param frosting a [epipredict::frosting].
#' @param colname character scalar; the column name whose whitening params are
#'   used for the reversal. Must match an entry in `colname` of a
#'   [step_epi_whitening] earlier in the recipe.
#' @param nonlin_method the nonlinear transform that was applied during
#'   whitening. Must match the value used in [step_epi_whitening].
#' @param step_id optional character scalar; id of the specific
#'   [step_epi_whitening] step to use, if the recipe has more than one.
#' @param id a character string used to identify the layer.
#' @seealso [step_epi_whitening]
#' @export
layer_epi_coloring <- function(
  frosting,
  colname,
  nonlin_method = c("quart_root", "none"),
  step_id = NULL,
  id = rand_id("epi_coloring")
) {
  nonlin_method <- arg_match(nonlin_method)
  add_layer(
    frosting,
    layer_epi_coloring_new(
      colname = colname,
      nonlin_method = nonlin_method,
      step_id = step_id,
      trained = FALSE,
      id = id
    )
  )
}

layer_epi_coloring_new <- function(colname, nonlin_method, step_id, trained, id) {
  layer(
    subclass = "epi_coloring",
    colname = colname,
    nonlin_method = nonlin_method,
    step_id = step_id,
    trained = trained,
    id = id
  )
}

#' @export
slather.layer_epi_coloring <- function(object, components, workflow, new_data, ...) {
  trained_recipe <- workflows::extract_recipe(workflow)
  whitening_step <- purrr::detect(
    trained_recipe$steps,
    function(s) {
      inherits(s, "step_epi_whitening") &&
        (is.null(object$step_id) || s$id == object$step_id) &&
        object$colname %in% s$colname
    }
  )
  if (is.null(whitening_step) || is.null(whitening_step$learned_params)) {
    return(components)
  }

  params <- whitening_step$learned_params
  join_cols <- intersect(whitening_step$join_cols, names(components$predictions))
  center_col <- paste0(object$colname, "_center")
  scale_col <- paste0(object$colname, "_scale")

  components$predictions <- components$predictions %>%
    left_join(
      select(params, all_of(c(join_cols, center_col, scale_col))),
      by = join_cols
    ) %>%
    mutate(.pred = .pred * .data[[scale_col]] + .data[[center_col]])

  if (".pred_distn" %in% names(components$predictions)) {
    components$predictions <- components$predictions %>%
      mutate(.pred_distn = .pred_distn * .data[[scale_col]] + .data[[center_col]])
  }

  if (object$nonlin_method == "quart_root") {
    components$predictions <- components$predictions %>%
      mutate(.pred = .pred^4 - 0.01)
    if (".pred_distn" %in% names(components$predictions)) {
      components$predictions <- components$predictions %>%
        mutate(.pred_distn = .pred_distn^4 - 0.01)
    }
  }

  components$predictions <- components$predictions %>%
    select(-all_of(c(center_col, scale_col)))

  components
}

#' @export
print.layer_epi_coloring <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Coloring (reverse whitening) column:"
  recipes::print_step(x$colname, rlang::enquos(x$colname), x$trained, title, width)
  invisible(x)
}
