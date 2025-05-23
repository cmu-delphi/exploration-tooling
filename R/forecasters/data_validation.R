#' Helper function for those writing forecasters
#'
#' A smorgasbord of checks that any epipredict-based forecaster should do:
#' 1. check that the args list is created correctly,
#' 2. rewrite an empty extra sources list from an empty string
#' 3. validate the outcome and predictors as present,
#' 4. make sure the trainer is a `regression` model from `parsnip`
#' 5. adjust the trainer's quantiles based on those in args_list if it's a
#'    quantile trainer
#' 6. remake the lags to match the numebr of predictors
#'
#' @inheritParams scaled_pop
#' @param predictors the full list of predictors including the outcome. can
#'   include empty strings
#' @param args_list the args list created by [`epipredict::default_args_list`]
#'
#' @export
sanitize_args_predictors_trainer <- function(epi_data, outcome, predictors, trainer, args_list) {
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli::cli_abort("args_list was not created using `default_args_list().")
  }

  predictors <- predictors[predictors != ""]
  epipredict:::validate_forecaster_inputs(epi_data, outcome, predictors)

  if (!is.null(trainer) && !epipredict:::is_regression(trainer)) {
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  } else if (inherits(trainer, "rand_forest") && trainer$engine == "grf_quantiles") {
    trainer %<>% set_engine("grf_quantiles", quantiles = args_list$quantile_levels)
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

#' Confirm that there's enough data to run this model
#'
#' Epipredict is a little bit fragile about having enough data to train; we want
#' to be able to return a null result rather than error out.
#'
#' @param epi_data the input data
#' @param ahead the effective ahead; may be infinite if there isn't enough data.
#' @param args_input the input as supplied to `slide_forecaster`; lags is the
#'   important argument, which may or may not be defined, with the default
#'   coming from `default_args_list`
#' @param outcome the outcome column
#' @param extra_sources any non-outcome predictor columns
#' @param buffer how many training data to insist on having (e.g. if `buffer=1`,
#'   this trains on one sample; the default is set so that `linear_reg` isn't
#'   rank deficient)
#'
#' @importFrom tidyr drop_na
#' @export
confirm_sufficient_data <- function(epi_data, ahead, args_input, outcome, extra_sources, buffer = 9) {
  lag_max <- max(unlist(pluck(args_input, "lags", .default = list(14))))

  # TODO: Buffer should probably be 2 * n(lags) * n(predictors). But honestly,
  # this needs to be fixed in epipredict itself, see
  # https://github.com/cmu-delphi/epipredict/issues/106.
  has_no_last_nas <- epi_data %>%
    drop_na(c(!!outcome, !!!extra_sources)) %>%
    group_by(geo_value) %>%
    summarise(has_enough_data = n_distinct(time_value) >= lag_max + ahead + buffer, .groups = "drop") %>%
    pull(has_enough_data) %>%
    any()
  return(
    all(!is.infinite(ahead)) && has_no_last_nas
  )
}

#' if we want to filter the main data column in some way, this is a simple way to share that across forecasters
filter_extraneous <- function(epi_data, filter_source, filter_agg_level) {
  if (filter_source != "") {
    if ("source" %in% colnames(epi_data)) {
      epi_data %<>% filter(source == filter_source)
    }
  }
  if (filter_agg_level != "") {
    if ("agg_level" %in% colnames(epi_data)) {
      epi_data %<>% filter(agg_level == filter_agg_level)
    }
  }
  return(epi_data)
}

#' the minus one ahead causes problems for `quantile_regression` if that data is
#' actually present, so we should filter it out
filter_minus_one_ahead <- function(epi_data, ahead) {
  if (ahead < 0) {
    dont_include <- attr(epi_data, "metadata")$as_of + ahead
    epi_data %<>% filter(time_value < dont_include)
  }
  epi_data
}
