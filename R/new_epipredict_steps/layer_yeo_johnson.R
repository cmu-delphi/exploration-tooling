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
layer_YeoJohnson2 <- function(frosting,
                              ...,
                              df,
                              by = NULL,
                              id = rand_id("YeoJohnson2")) {
  arg_is_scalar(df_pop_col, rate_rescaling, create_new, suffix, id)
  arg_is_lgl(create_new)
  arg_is_chr(df_pop_col, suffix, id)
  arg_is_chr(by, allow_null = TRUE)
  if (rate_rescaling <= 0) {
    cli_abort("`rate_rescaling` must be a positive number.")
  }

  add_layer(
    frosting,
    layer_YeoJohnson2_new(
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      rate_rescaling = rate_rescaling,
      terms = dplyr::enquos(...),
      create_new = create_new,
      suffix = suffix,
      id = id
    )
  )
}

layer_YeoJohnson2_new <-
  function(df, by, df_pop_col, rate_rescaling, terms, create_new, suffix, id) {
    layer("YeoJohnson2",
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      rate_rescaling = rate_rescaling,
      terms = terms,
      create_new = create_new,
      suffix = suffix,
      id = id
    )
  }

#' @export
slather.layer_YeoJohnson2 <-
  function(object, components, workflow, new_data, ...) {
    rlang::check_dots_empty()

    browser()
    if (is.null(object$by)) {
      # Assume `layer_predict` has calculated the prediction keys and other
      # layers don't change the prediction key colnames:
      prediction_key_colnames <- names(components$keys)
      lhs_potential_keys <- prediction_key_colnames
      rhs_potential_keys <- colnames(select(object$df, !object$df_pop_col))
      object$by <- intersect(lhs_potential_keys, rhs_potential_keys)
      suggested_min_keys <- kill_time_value(lhs_potential_keys)
      if (!all(suggested_min_keys %in% object$by)) {
        cli_warn(c(
          "{setdiff(suggested_min_keys, object$by)} {?was an/were} epikey column{?s} in the predictions,
           but {?wasn't/weren't} found in the population `df`.",
          "i" = "Defaulting to join by {object$by}",
          ">" = "Double-check whether column names on the population `df` match those expected in your predictions",
          ">" = "Consider using population data with breakdowns by {suggested_min_keys}",
          ">" = "Manually specify `by =` to silence"
        ), class = "epipredict__layer_YeoJohnson2__default_by_missing_suggested_keys")
      }
    }

    object$by <- object$by %||% intersect(
      epi_keys_only(components$predictions),
      colnames(select(object$df, !object$df_pop_col))
    )
    joinby <- list(x = names(object$by) %||% object$by, y = object$by)
    hardhat::validate_column_names(components$predictions, joinby$x)
    hardhat::validate_column_names(object$df, joinby$y)

    # object$df <- object$df %>%
    #  dplyr::mutate(dplyr::across(tidyselect::where(is.character), tolower))
    pop_col <- rlang::sym(object$df_pop_col)
    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)
    suffix <- ifelse(object$create_new, object$suffix, "")
    col_to_remove <- setdiff(colnames(object$df), colnames(components$predictions))

    components$predictions <- inner_join(
      components$predictions,
      object$df,
      by = object$by,
      relationship = "many-to-one",
      unmatched = c("error", "drop"),
      suffix = c("", ".df")
    ) %>%
      mutate(across(
        all_of(col_names),
        ~ .x * !!pop_col / object$rate_rescaling,
        .names = "{.col}{suffix}"
      )) %>%
      select(-any_of(col_to_remove))
    components
  }

#' @export
print.layer_YeoJohnson2 <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Scaling predictions by population"
  print_layer(x$terms, title = title, width = width)
}
