#' Unormalizing transformation
#'
#' Will undo a step_YeoJohnson2 transformation.
#'
#' @param frosting a `frosting` postprocessor. The layer will be added to the
#'   sequence of operations for this frosting.
#' @param ... One or more selector functions to scale variables
#'   for this step. See [recipes::selections()] for more details.
#' @param df a data frame that contains the population data to be used for
#'   inverting the existing scaling.
#' @param by A (possibly named) character vector of variables to join by.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' library(dplyr)
#' jhu <- epidatasets::cases_deaths_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
#'   select(geo_value, time_value, cases)
#'
#' pop_data <- data.frame(states = c("ca", "ny"), value = c(20000, 30000))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_YeoJohnson2(
#'     df = pop_data,
#'     df_pop_col = "value",
#'     by = c("geo_value" = "states"),
#'     cases, suffix = "_scaled"
#'   ) %>%
#'   step_epi_lag(cases_scaled, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(cases_scaled, ahead = 7, role = "outcome") %>%
#'   step_epi_naomit()
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(.pred) %>%
#'   layer_naomit(.pred) %>%
#'   layer_YeoJohnson2(.pred,
#'     df = pop_data,
#'     by = c("geo_value" = "states"),
#'     df_pop_col = "value"
#'   )
#'
#' wf <- epi_workflow(r, linear_reg()) %>%
#'   fit(jhu) %>%
#'   add_frosting(f)
#'
#' forecast(wf)
layer_YeoJohnson2 <- function(frosting, ..., lambdas = NULL, by = NULL, id = rand_id("YeoJohnson2")) {
  checkmate::assert_tibble(lambdas, min.rows = 1, null.ok = TRUE)

  add_layer(
    frosting,
    layer_YeoJohnson2_new(
      lambdas = lambdas,
      by = by,
      terms = dplyr::enquos(...),
      id = id
    )
  )
}

layer_YeoJohnson2_new <- function(lambdas, by, terms, id) {
  layer("YeoJohnson2", lambdas = lambdas, by = by, terms = terms, id = id)
}

#' @export
#' @importFrom workflows extract_preprocessor
slather.layer_YeoJohnson2 <- function(object, components, workflow, new_data, ...) {
  rlang::check_dots_empty()

  # Get the lambdas from the layer or from the workflow.
  lambdas <- object$lambdas %||% get_lambdas_in_layer(workflow)

  # If the by is not specified, try to infer it from the lambdas.
  if (is.null(object$by)) {
    # Assume `layer_predict` has calculated the prediction keys and other
    # layers don't change the prediction key colnames:
    prediction_key_colnames <- names(components$keys)
    lhs_potential_keys <- prediction_key_colnames
    rhs_potential_keys <- colnames(select(lambdas, -starts_with("lambda_")))
    object$by <- intersect(lhs_potential_keys, rhs_potential_keys)
    suggested_min_keys <- setdiff(lhs_potential_keys, "time_value")
    if (!all(suggested_min_keys %in% object$by)) {
      cli_warn(c(
        "{setdiff(suggested_min_keys, object$by)} {?was an/were} epikey column{?s} in the predictions,
          but {?wasn't/weren't} found in the population `df`.",
        "i" = "Defaulting to join by {object$by}",
        ">" = "Double-check whether column names on the population `df` match those expected in your predictions",
        ">" = "Consider using population data with breakdowns by {suggested_min_keys}",
        ">" = "Manually specify `by =` to silence"
      ), class = "epipredict__layer_population_scaling__default_by_missing_suggested_keys")
    }
  }

  # Establish the join columns.
  object$by <- object$by %||%
    intersect(
      epipredict:::epi_keys_only(components$predictions),
      colnames(select(lambdas, -starts_with("lambda_")))
    )
  joinby <- list(x = names(object$by) %||% object$by, y = object$by)
  hardhat::validate_column_names(components$predictions, joinby$x)
  hardhat::validate_column_names(lambdas, joinby$y)

  # Get the columns to transform. In components$predictions, the output is .pred, but then
  # this corresponds to the lambda_<outcome> column in lambdas. So we have to:
  # - identify the outcome column from the recipe
  # - select it from lambdas and make the transformation
  # TODO: We don't do multiple outcomes, do we? Assume not for now.
  exprs <- rlang::expr(c(!!!object$terms))
  pos <- tidyselect::eval_select(exprs, components$predictions)
  col_names <- names(pos)

  # Assuming that the above is basically just .pred.

  # Get the outcome from the outcomes. Assuming this is a vector of objects like
  # ahead_1_cases, ahead_7_cases, etc. We want to extract the cases part.
  outcome_col <- names(components$mold$outcomes) %>%
    stringr::str_extract("(?<=_)[^_]+$") %>%
    unique() %>%
    extract(1)

  # Join the lambdas.
  components$predictions <- inner_join(
    components$predictions,
    lambdas,
    by = object$by,
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  )
  # For every column, we need to use the appropriate lambda column, which differs per row.
  # Note that yj_inverse() is vectorized.
  for (col in col_names) {
    components$predictions <- components$predictions %>%
      mutate(!!col := yj_inverse(!!sym(col), !!sym(paste0("lambda_", outcome_col))))
  }
  # Remove the lambda columns.
  components$predictions <- components$predictions %>%
    select(-any_of(starts_with("lambda_")))
  components
}

# TODO: Print the layer info.
#' @export
print.layer_YeoJohnson2 <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Yeo-Johnson transformation (see `lambdas` object for values) on "
  epipredict:::print_layer(x$terms, title = title, width = width)
}

#' Inverse Yeo-Johnson transformation
#'
#' @keywords internal
yj_inverse <- function(x, lambda) {
  # TODO: Fix this.
  return((x^lambda - 1) / lambda)
}

# TODO: Error checks?
get_lambdas_in_layer <- function(workflow) {
  this_recipe <- hardhat::extract_recipe(workflow)
  if (!(this_recipe %>% recipes::detect_step("YeoJohnson2"))) {
    cli_abort("`layer_YeoJohnson2` requires `step_YeoJohnson2` in the recipe.", call = rlang::caller_env())
  }
  for (step in this_recipe$steps) {
    if (inherits(step, "step_YeoJohnson2")) {
      lambdas <- step$lambdas
      break
    }
  }
  lambdas
}
