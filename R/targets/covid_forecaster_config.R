#' Configuration parameters for COVID hospitalization forecasting
#'
#' This file contains configuration parameters used by the COVID hospitalization
#' forecasting pipeline.

#' Get forecaster parameter combinations for COVID forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' @param dummy_mode Boolean indicating whether to use dummy forecasters
#' @return A list of forecaster parameter combinations
#' @export
get_covid_forecaster_params <- function() {
  out <- rlang::list2(
    scaled_pop_main = tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = list("linreg", "quantreg"),
      lags = list(
        c(0, 7),
        c(0, 7, 14),
        c(0, 7, 14, 21),
        c(0, 7, 14, 21, 28)
      ),
      pop_scaling = FALSE,
      n_training = Inf
    ),
    flatline_forecaster = tidyr::expand_grid(
      forecaster = "flatline_fc",
    ),
    # using exogenous variables
    scaled_pop_exogenous = bind_rows(
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        # since it's a list, this gets expanded out to a single one in each row
        extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7) # exogenous feature
          ),
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7, 14) # exogenous feature
          )
        ),
        pop_scaling = FALSE,
        scale_method = "quantile",
        n_training = Inf
      ),
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        extra_sources = list2(
          c("nssp", "google_symptoms"),
          c("nssp", "nwss"),
          c("nssp", "nwss_region"),
          c("google_symptoms", "nwss"),
          c("google_symptoms", "nwss_region"),
          c("nwss", "nwss_region")
        ),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # first feature
            c(0, 7) # second feature
          )
        ),
        pop_scaling = FALSE,
        scale_method = "quantile",
        n_training = Inf
      ),
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        extra_sources = list2(
          c("nssp", "google_symptoms", "nwss", "nwss_region"),
        ),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # nssp
            c(0, 7), # google symptoms
            c(0, 7), # nwss
            c(0, 7), # nwss_region
          ),
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # nssp
            c(0, 7, 14), # google symptoms
            c(0, 7, 14), # nwss
            c(0, 7, 14), # nwss_region
          )
        ),
        pop_scaling = FALSE,
        scale_method = "quantile",
        n_training = Inf
      )
    ),
    scaled_pop_season = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7, 14, 21),
        c(0, 7)
      ),
      pop_scaling = FALSE,
      n_training = Inf,
      seasonal_method = list(
        c("covid"),
        c("window"),
        c("covid", "window"),
        c("climatological"),
        c("climatological", "window")
      )
    )
  ) %>%
    map(function(x) {
      if (g_dummy_mode) {
        x$forecaster <- "dummy_forecaster"
      }
      x <- add_id(x)
      if ("trainer" %in% names(x) && is.list(x$trainer)) {
        x$trainer <- x$trainer[[1]]
      }
      if ("seasonal_method" %in% names(x) && is.list(x$seasonal_method)) {
        x$seasonal_method <- x$seasonal_method[[1]]
      }
      # Add the outcome to each forecaster.
      x$outcome <- "hhs"
      x
    })

  # Make sure all ids are unique.
  stopifnot(
    length(out$id %>% unique()) == length(out$id)
  )
  out
}
