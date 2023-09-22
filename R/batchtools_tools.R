library(epiprocess)
library(batchtools)
library(epidatr)

explore_env <- new.env(parent = emptyenv())
explore_env$registry <- NULL

#' setup the batchtools registry
#' @description
#' settings come from. Can either load an existing registry, or make a new one
#'   in the case it doesn't exist
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
#' @param output_formatter a function with signature `output_formatter(pred,
#'   ahead, levels)` which converts the output of epipredict into the format
#'   expected by the scoring utils. See [formatters.R] for some examples]
#' @inheritDotParams addProblem -name -data
add_forecast_problem <- function(problem_name,
                             archive,
                             outcome,
                             extra_sources,
                             ...) {
  addProblem(
    name = problem_name,
    data = list(
      archive = archive,
      outcome = outcome,
      extra_sources = extra_sources,
    ), ...
  )
}

#' add a forecaster to the registry as an algorithm
#' @description
#' because there is quite a bit of shared code around evaluating a given
#'   forecaster, we don't expect the user to define the slide and other
#'   evaluation details.
add_forecaster <- function(forecaster_name,
                           forecaster,
                           reg = getDefaultRegistry()) {
  wrapper <- function(..., forecaster = forecaster) {
    forecaster_pred(..., forecaster = forecaster)
  }
  addAlgorithm(forecaster_name, fun = wrapper, reg = reg)
}
