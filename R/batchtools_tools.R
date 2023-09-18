library(epiprocess)
library(batchtools)
library(epidatr)
explore_env <- new.env(parent = emptyenv())
explore_env$registry <- NULL
#' setup the batchtools registry
#' @description
#' settings come from. Can either load an existing registry, or make a new one in the case it doesn't exist
#' @params registry_folder where the registry is stored
#' @return [`NULL`]
#' @export
set_registry <- function(registry_folder = here::here("registry")) {
  tryCatch(
    {
      explore_env$registry <- makeExperimentRegistry(seed = 1)
    },
    error = function(cond) {
      explore_env$registry <- loadRegistry(registry_folder, writeable = TRUE)
    }
  )
}

#' add a new dataset to the registry
#' @description
#' in the context of forecaster exploration, we can be a bit more specific about
#'   the problem than a generic batchtools Problem. In addition to the data, the
#'   problem also specifies the outcome and available extra sources.
#' @param archive the actual data, its expected to be in an Epiprocess archive
#' @param problem_name what you want to name the problem
#' @param outcome the target variable
#' @param extra_sources any potential side information the model has available
#'   in the archive
#' @inheritDotParams addProblem
add_data_problem <- function(problem_name,
                             archive,
                             outcome,
                             extra_sources,
                             ...) {
  addProblem(
    name = problem_name,
    data = list(
      archive = archive,
      outcome = outcome,
      extra_sources = extra_sources
    ), ...
  )
}

#' score a forecaster on a particular dataset
#' @description
#' a wrapper that turns a forecaster, scoring function, parameter, data
#' combination into an actual experiment that outputs a
#' score for each ahead.
#' as far as batchtools is concerned, the scoring function is a particular
#'   parameter of the forecaster (or Algorithm, as they call it).
#' @param data as per batchtools
#' @param job as per batchtools
#' @param instance as per batchtools
#' @param scoring_function TODO detailed spec
#' @param forecaster a function that does the actual forecasting for a given
#'   day. See `exampleSpec.R` for an example function and its documentation for
#'   the general parameter requirements.
#' @param slide_training a required parameter that governs the window size that
#'   epix_slide hands off to epipredict.
#' @param slide_training_pad a required parameter that determines padding
#' @param ... any extra parameters the user has defined for forecaster.
forecaster_eval <- function(data,
                            job,
                            instance,
                            scoring_function,
                            forecaster,
                            slide_training = Inf,
                            slide_training_pad = 20L,
                            trainer = parsnip::linear_reg(),
                            ahead = 1,
                            ...) {
  archive <- instance$archive
  outcome <- instance$outcome
  extra_sources <- instance$extra_sources
  # restrict the dataset to areas where training is possible
  start_date <- min(archive$DT$time_value) + slide_training + slide_training_pad
  end_date <- max(archive$DT$time_value) - ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = 1)
  # first generate the forecasts
  res <- epix_slide(
    archive,
    ~ forecaster(
      ahead,
      .x,
      trainer,
      ...
    ),
    before = n_training + n_training_pad - 1,
    ref_time_values = valid_predict_dates,
    new_col_name = ".pred_distn",
  )
  # reformat them to have hub format
  # score them
  # return the scores
  return(res)
}

reformat_slide_result <- function() {
}

#' adding an algorithm to the batchtool collection
add_forecaster_eval <- function(data, job, instance, scoring_function, forecaster, n_training, ...) {
}













#' probably not useful, and best left up to the user along with some utils for
#' formatting epidatr output
fetch_particular_data <- function(problemName, outcome_variable, archive, current_version, registry = batchtools::getDefaultRegistry(), source_signal_pairs, backend, ..., time_values = epirange(from = "1980-05-08", to = "2023-06-08"), issue_range = time_values, issue_batch_by = "month") {
  # first check whether this data already exists
  if (problemName %in% explore_env$registry$problems) {
    print("problem already in registry")
    return()
  }
  monthly <- seq.Date(from = earliest_issue_date, to = end_date, by = issue_batch_by)
  for (source_signal in source_signal_pairs) {
    hhs_init <- get_data(end_date, monthly[[1]], monthly[[5]], "hhs")
    chng_init <- get_data(end_date, monthly[[1]], monthly[[5]], "chng", timeout_seconds = 5 * 60)
  }

  archive <- epix_merge(hhs_init, chng_init, sync = "locf")

  for (ii in seq(from = 4, to = length(monthly))) {
    print(monthly[[ii]])
    hhs <- get_data(end_date, monthly[[ii - 1]], monthly[[ii]], "hhs")
    chng <- get_data(end_date, monthly[[ii - 1]], monthly[[ii]], "chng")
    merged <- epix_merge(hhs, chng, sync = "locf")
    archive <- epix_rbind(archive, merged, sync = "locf", force_distinct = TRUE)
    saveRDS(archive, archive_filename)
  }
}

#' add a forecaster to the registry as an algorithm
#' @description
#' because there is quite a bit of shared code around evaluating a given
#'   forecaster, we don't expect to define the slide and other evaluation
add_forecaster <- function(forecaster_name, forecaster, reg = getDefaultRegistry()) {
  addAlgorithm(forecaster_name, fun = )
}
