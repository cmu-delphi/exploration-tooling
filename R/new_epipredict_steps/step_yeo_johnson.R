#' Yeo-Johnson transformation
#'
#' `step_epi_YeoJohnson()` creates a *specification* of a recipe step that will
#' transform data using a Yeo-Johnson transformation. This fork works with panel
#' data and is meant for epidata.
#' TODO: Do an edit pass on this docstring.
#'
#' @inheritParams step_center
#' @param lambdas A numeric vector of transformation values. This
#'  is `NULL` until computed by [prep()].
#' @param na_lambda_fill A numeric value to fill in for any
#'  geos where the lambda cannot be estimated.
#' @param limits A length 2 numeric vector defining the range to
#'  compute the transformation parameter lambda.
#' @param num_unique An integer where data that have less possible
#'  values will not be evaluated for a transformation.
#' @param na_rm A logical indicating whether missing values should be
#'  removed.
#' @param skip A logical. Should the step be skipped when the recipe is
#'  baked by [bake()]. On the `training` data, the step will always be
#'  conducted (even if `skip = TRUE`).
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details The Yeo-Johnson transformation is very similar to the
#'  Box-Cox but does not require the input variables to be strictly
#'  positive. In the package, the partial log-likelihood function is
#'  directly optimized within a reasonable set of transformation
#'  values (which can be changed by the user).
#'
#' This transformation is typically done on the outcome variable
#'  using the residuals for a statistical model (such as ordinary
#'  least squares). Here, a simple null model (intercept only) is
#'  used to apply the transformation to the *predictor*
#'  variables individually. This can have the effect of making the
#'  variable distributions more symmetric.
#'
#' If the transformation parameters are estimated to be very
#'  closed to the bounds, or if the optimization fails, a value of
#'  `NA` is used and no transformation is applied.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the lambda estimate}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @references Yeo, I. K., and Johnson, R. A. (2000). A new family of power
#'   transformations to improve normality or symmetry. *Biometrika*.
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'   data = biomass_tr
#' )
#'
#' yj_transform <- step_epi_YeoJohnson(rec, all_numeric())
#'
#' yj_estimates <- prep(yj_transform, training = biomass_tr)
#'
#' yj_te <- bake(yj_estimates, biomass_te)
#'
#' plot(density(biomass_te$sulfur), main = "before")
#' plot(density(yj_te$sulfur), main = "after")
#'
#' tidy(yj_transform, number = 1)
#' tidy(yj_estimates, number = 1)
step_epi_YeoJohnson <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  lambdas = NULL,
  na_lambda_fill = 1 / 4,
  limits = c(-5, 5),
  num_unique = 5,
  na_rm = TRUE,
  skip = FALSE,
  id = rand_id("epi_YeoJohnson")
) {
  checkmate::assert_numeric(limits, len = 2)
  checkmate::assert_numeric(na_lambda_fill, lower = min(limits), upper = max(limits), len = 1)
  checkmate::assert_numeric(num_unique, lower = 2, upper = Inf, len = 1)
  checkmate::assert_logical(na_rm, len = 1)
  checkmate::assert_logical(skip, len = 1)
  add_step(
    recipe,
    step_epi_YeoJohnson_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      lambdas = lambdas,
      na_lambda_fill = na_lambda_fill,
      limits = sort(limits)[1:2],
      num_unique = num_unique,
      na_rm = na_rm,
      forecast_date = NULL,
      metadata = NULL,
      columns = NULL,
      skip = skip,
      id = id
    )
  )
}

step_epi_YeoJohnson_new <- function(
  terms,
  role,
  trained,
  lambdas,
  na_lambda_fill,
  limits,
  num_unique,
  na_rm,
  forecast_date,
  metadata,
  columns,
  skip,
  id
) {
  step(
    subclass = "epi_YeoJohnson",
    terms = terms,
    role = role,
    trained = trained,
    lambdas = lambdas,
    na_lambda_fill = na_lambda_fill,
    limits = limits,
    num_unique = num_unique,
    na_rm = na_rm,
    forecast_date = forecast_date,
    metadata = metadata,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_epi_YeoJohnson <- function(x, training, info = NULL, ...) {
  # Check that the columns selected for transformation are numeric.
  col_names <- recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  lambdas <- get_lambdas_yj_table(
    training,
    col_names,
    x$limits,
    x$num_unique,
    x$na_lambda_fill,
    x$na_rm,
    key_colnames(training, exclude = "time_value")
  )

  step_epi_YeoJohnson_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lambdas = lambdas,
    na_lambda_fill = x$na_lambda_fill,
    limits = x$limits,
    num_unique = x$num_unique,
    na_rm = x$na_rm,
    forecast_date = attributes(training)$metadata$as_of,
    metadata = attributes(training)$metadata,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_YeoJohnson <- function(object, new_data, ...) {
  # If not an epi_df, make it one assuming the template of training data.
  # If it is an epi_df, check that the keys match.
  # Imitating the pattern in step_adjust_latency().
  if (!inherits(new_data, "epi_df") || is.null(attributes(new_data)$metadata$as_of)) {
    new_data <- as_epi_df(
      new_data,
      as_of = object$forecast_date,
      other_keys = object$metadata$other_keys %||% character()
    )
    new_data %@% metadata <- object$metadata
  }
  # Check that the keys match.
  keys <- key_colnames(new_data, exclude = "time_value")
  old_keys <- object$lambdas %>% select(-starts_with(".lambda_")) %>% colnames()
  if (!all(keys %in% old_keys)) {
    cli::cli_abort(
      "The keys of the new data do not match the keys of the training data.",
      call = rlang::caller_fn()
    )
  }
  # Check that the columns for transformation are present in new_data.
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  # Transform each column, using the appropriate lambda column per row.
  # Note that yj_transform() is vectorized in x, but not in lambda.
  new_data <- left_join(new_data, object$lambdas, by = keys)
  for (col in col_names) {
    new_data <- new_data %>%
      rowwise() %>%
      mutate(!!col := yj_transform(!!sym(col), !!sym(paste0(".lambda_", col))))
  }
  # Remove the lambda columns.
  new_data %>%
    select(-starts_with(".lambda_")) %>%
    ungroup()
}

#' @export
print.step_epi_YeoJohnson <- function(x, width = max(20, options()$width - 39), ...) {
  title <- "Yeo-Johnson transformation (see `lambdas` object for values) on "
  epipredict:::print_epi_step(x$terms, x$terms, title = title, width = width)
  invisible(x)
}

#' Compute the lambda values per group for each column.
#'
#' @keywords internal
#' @rdname recipes-internal
get_lambdas_yj_table <- function(training, col_names, limits, num_unique, na_lambda_fill, na_rm, epi_keys_checked) {
  # Estimate the lambda for each column, creating a lambda_ column for each.
  # Note that estimate_yj() operates on a vector.
  lambdas <- training %>%
    summarise(
      across(all_of(col_names), ~ estimate_yj(.x, limits, num_unique, na_rm)),
      .by = all_of(epi_keys_checked)
    ) %>%
    rename_with(~ paste0(".lambda_", .x), -all_of(epi_keys_checked))

  # Check for NAs in any of the lambda_ columns.
  # EDIT: This warning was too noisy. Keeping code around, in case we want it.
  # for (col in col_names) {
  #   if (any(is.na(values[[paste0("lambda_", col)]]))) {
  #     cli::cli_warn(
  #       c(
  #         x = "Yeo-Johnson lambda could not be estimated for some geos for {col}.",
  #         i = "Using lambda={x$na_lambda_fill} in these cases."
  #       ),
  #       call = rlang::caller_fn()
  #     )
  #   }
  # }

  # Fill in NAs with the default lambda.
  lambdas %>%
    mutate(across(starts_with(".lambda_"), \(col) ifelse(is.na(col), na_lambda_fill, col)))
}


### Code below taken from recipes::step_YeoJohnson.
### https://github.com/tidymodels/recipes/blob/v1.1.1/R/YeoJohnson.R#L172

#' Internal Functions
#'
#' @keywords internal
#' @rdname recipes-internal
#' @export
yj_transform <- function(x, lambda, ind_neg = NULL, eps = 0.001) {
  if (is.na(lambda)) {
    return(x)
  }
  if (!inherits(x, "tbl_df") || is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  } else {
    if (!is.vector(x)) {
      x <- as.vector(x)
    }
  }
  # TODO case weights: can we use weights here?
  if (is.null(ind_neg)) {
    dat_neg <- x < 0
    ind_neg <- list(is = which(dat_neg), not = which(!dat_neg))
  }
  not_neg <- ind_neg[["not"]]
  is_neg <- ind_neg[["is"]]

  nn_trans <- function(x, lambda) {
    if (abs(lambda) < eps) {
      log(x + 1)
    } else {
      ((x + 1)^lambda - 1) / lambda
    }
  }

  ng_trans <- function(x, lambda) {
    if (abs(lambda - 2) < eps) {
      -log(-x + 1)
    } else {
      -((-x + 1)^(2 - lambda) - 1) / (2 - lambda)
    }
  }

  if (length(not_neg) > 0) {
    x[not_neg] <- nn_trans(x[not_neg], lambda)
  }

  if (length(is_neg) > 0) {
    x[is_neg] <- ng_trans(x[is_neg], lambda)
  }
  x
}


## Helper for the log-likelihood calc for eq 3.1 of Yeo, I. K.,
## & Johnson, R. A. (2000). A new family of power transformations
## to improve normality or symmetry. Biometrika. page 957
ll_yj <- function(lambda, y, ind_neg, const, eps = 0.001) {
  n <- length(y)
  y_t <- yj_transform(y, lambda, ind_neg)
  mu_t <- mean(y_t)
  var_t <- var(y_t) * (n - 1) / n
  res <- -.5 * n * log(var_t) + (lambda - 1) * const
  res
}

## eliminates missing data and returns -llh
yj_obj <- function(lam, dat, ind_neg, const) {
  ll_yj(lambda = lam, y = dat, ind_neg = ind_neg, const = const)
}

## estimates the values
#' @keywords internal
#' @rdname recipes-internal
#' @export
estimate_yj <- function(dat, limits = c(-5, 5), num_unique = 5, na_rm = TRUE, call = caller_env(2)) {
  na_rows <- which(is.na(dat))
  if (length(na_rows) > 0) {
    if (na_rm) {
      dat <- dat[-na_rows]
    } else {
      cli::cli_abort(
        c(
          x = "Missing values are not allowed for the YJ transformation.",
          i = "See {.arg na_rm} option."
        ),
        call = call
      )
    }
  }

  eps <- .001
  if (length(unique(dat)) < num_unique) {
    return(NA)
  }
  dat_neg <- dat < 0
  ind_neg <- list(is = which(dat_neg), not = which(!dat_neg))

  const <- sum(sign(dat) * log(abs(dat) + 1))

  res <- optimize(
    yj_obj,
    interval = limits,
    maximum = TRUE,
    dat = dat,
    ind_neg = ind_neg,
    const = const,
    tol = .0001
  )
  lam <- res$maximum
  if (abs(limits[1] - lam) <= eps | abs(limits[2] - lam) <= eps) {
    lam <- NA
  }
  lam
}

# Copied from recipes:::tidy.step_BoxCox
#
#' @rdname tidy.recipe
#' @export
tidy.step_epi_YeoJohnson <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$lambdas),
      value = unname(x$lambdas)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}
