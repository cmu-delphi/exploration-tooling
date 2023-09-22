library(epipredict)
library(recipes)
library(tidyverse)
library(magrittr)

#' to define a forecaster:
#' first define any pre-epipredict steps (they should be operating on a epi_df)
#' then define any post-epipredict steps (same)
#' then create the forecaster as a function, which must have the following
#' arguments:
#' @param epi_df the actual data used
#' @param outcome the name of the target variable
#' @param extra_sources the name of any extra columns to use. This list could be
#'   empty
#' @param ahead (this is relative to the `as_of` field of the `epi_df`, which is
#'   likely *not* the same as the `ahead` used by epipredict, which is relative
#'   to the max time value of the `epi_df`. how to handle this is a modelling
#'   question left up to each forecaster)
#' @param pop_scaling an example extra parameter unique to this forecaster
#' @param trainer an example extra parameter that is fairly common
#' @param ... it can also have any number of other parameters. In this case, the
#'   `...` args are all inputs to [`epipredict::arx_args_list`].  Consult the
#'   repository for existing parameter names so that your function will follow a
#'   similar schema (e.g. `trainer`, while not strictly required, is a common
#'   parameter, as are any of the `arx_args_list()` parameters) these parameters
#'   should be ones that will store well in a data.table; if you need more
#'   complicated parameters, it is better to store them in separate files, and
#'   use the filename as the parameter.
scaled_pop <- function(epi_data,
                       outcome,
                       extra_sources,
                       ahead,
                       pop_scaling = TRUE,
                       trainer = parsnip::linear_reg(),
                       levels = covidhub_probs(),
                       ...) {
  # perform any preprocessing not supported by epipredict
  # one that every forecaster will need to handle: how to manage max(time_value)
  # that's older than the `as_of` date
  epidataAhead <- extend_ahead(epi_data, ahead)
  # see latency_adjusting for other examples
  # this next part is basically unavoidable boilerplate you'll want to copy
  epi_data <- epidataAhead[[1]]
  effective_ahead <- epidataAhead[[2]]
  args_input <- list(...)
  args_input[["ahead"]] <- effective_ahead
  args_list <- do.call(arx_args_list, args_input)
  predictors <- c(outcome, extra_sources)
  perform_sanity_checks(epi_data, outcome, predictors, trainer, args_list)
  # edge case where there is no data; eventually epipredict will handle this
  if (is.infinite(args_list$ahead)) {
    effective_ahead <- 0
    null_result <- tibble(geo_value = character(), .pred = numeric(), .pred_distn = numeric(), forecast_date = numeric(), target_date = numeric())
    return(null_result)
  }
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict

  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  preproc %<>% step_population_scaling(
    all_numeric(),
    df = state_census,
    df_pop_col = "pop",
    create_new = FALSE,
    rate_rescaling = 1e5,
    by = c("geo_value" = "abbr")
  )
  preproc %<>% arx_preprocess(outcome, predictors, args_list)

  # postprocessing supported by epipredict
  postproc <- frosting()
  postproc %<>% layer_population_scaling(
    ".pred",
    df = state_census,
    df_pop_col = "pop",
    create_new = FALSE,
    rate_rescaling = 1e5,
    by = c("geo_value" = "abbr")
  )
  postproc %<>% arx_postprocess(trainer, outcome)
  # with all the setup done, we execute and format
  pred <- run_workflow_and_format(preproc, postproc, trainer, epi_data)
  # now pred has the columns
  #' `(geo_value, forecast_date, target_end_date, quantile, value)`
  # finally, any postprocessing not supported by epipredict e.g. calibration
  return(pred)
}
