#' helper function for those writing forecasters
#' @description
#' a smorgasbord of checks that any epipredict-based forecaster should do:
#' 1. check that the args list is created correctly,
#' 2. rewrite an empty extra sources list from an empty string
#' 3. validate the outcome and predictors as present,
#' 4. make sure the trainer is a `regression` model from `parsnip`
#' 5. adjust the trainer's quantiles based on those in args_list if it's a
#'    quantile trainer
#' 6. remake the lags to match the numebr of predictors
#' @inheritParams scaled_pop
#' @param predictors the full list of predictors including the outcome. can
#'   include empty strings
#' @param args_list the args list created by [`epipredict::arx_args_list`]
#' @export
sanitize_args_predictors_trainer <- function(epi_data,
                                             outcome,
                                             predictors,
                                             trainer,
                                             args_list) {
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `arx_args_list().")
  }

  predictors <- predictors[predictors != ""]
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)

  if (!is.null(trainer) && !epipredict:::is_regression(trainer)) {
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  } else if (inherits(trainer, "quantile_reg")) {
    # add all quantile_levels to the trainer and update args list
    quantile_levels <- sort(epipredict:::compare_quantile_args(
      args_list$quantile_levels,
      rlang::eval_tidy(trainer$args$quantile_levels)
    ))
    args_list$quantile_levels <- quantile_levels
    trainer$args$quantile_levels <- rlang::enquo(quantile_levels)
  }
  args_list$lags <- epipredict:::arx_lags_validator(predictors, args_list$lags)
  return(list(args_list, predictors, trainer))
}

#' confirm that there's enough data to run this model
#' @description
#' epipredict is a little bit fragile about having enough data to train; we want
#'   to be able to return a null result rather than error out.
#' @param epi_data the input data
#' @param ahead the effective ahead; may be infinite if there isn't enough data.
#' @param args_input the input as supplied to `slide_forecaster`; lags is the
#'   important argument, which may or may not be defined, with the default
#'   coming from `arx_args_list`
#' @param outcome the outcome column
#' @param extra_sources any non-outcome predictor columns
#' @param buffer how many training data to insist on having (e.g. if `buffer=1`,
#'   this trains on one sample; the default is set so that `linear_reg` isn't
#'   rank deficient)
#' @importFrom tidyr drop_na
#' @export
confirm_sufficient_data <- function(epi_data, ahead, args_input, outcome, extra_sources, buffer = 9) {
  if (!is.null(args_input$lags)) {
    lag_max <- max(unlist(args_input$lags))
  } else {
    lag_max <- 14 # default value of 2 weeks
  }

  # TODO: Buffer should probably be 2 * n(lags) * n(predictors). But honestly,
  # this needs to be fixed in epipredict itself, see
  # https://github.com/cmu-delphi/epipredict/issues/106.
  if (extra_sources == c("")) {
    extra_sources <- character(0L)
  }
  has_no_last_nas <- epi_data %>%
    drop_na(c(!!outcome, !!!extra_sources)) %>%
    group_by(geo_value) %>%
    summarise(has_enough_data = n_distinct(time_value) >= lag_max + ahead + buffer) %>%
    pull(has_enough_data) %>%
    any()
  return(
    !is.infinite(ahead) && has_no_last_nas
  )
}
