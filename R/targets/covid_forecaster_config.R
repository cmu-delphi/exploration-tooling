#' Configuration parameters for COVID hospitalization forecasting
#'
#' This file contains configuration parameters used by the COVID hospitalization
#' forecasting pipeline.

#' Get forecaster parameter combinations for COVID forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' Note that expand_grid has some quirks:
#' - if an entry is a vector c() or a list(), each top-level element is expanded out to a row.
#' - this means that list(list()) reuses the same inner list for each row.
#'
#' @param dummy_mode Boolean indicating whether to use dummy forecasters
#' @return A list of forecaster parameter combinations
#' @export
get_covid_forecaster_params <- function() {
  out <- rlang::list2(
    cdc_baseline = tidyr::expand_grid(
      forecaster = "forecaster_cdc_baseline",
      all_aheads = list(g_aheads),
    ),
    scaled_pop_main = tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
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
        extra_sources = list2("nssp", "google_symptoms", "va_covid_per_100k", "nhsn_hhs_region"),
        # "nwss", "nwss_region",  # removed b/c missing
        lags = list2(
          list2(
            c(0, 7, 14, 21), # nhsn
            c(0, 7) # exogenous feature
          ),
          list2(
            c(0, 7, 14, 21), # nhsn
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
          ## c("nssp", "nwss"), # removed b/c missing
          ## c("nssp", "nwss_region"),
          c("nssp", "va_covid_per_100k"),
          ## c("google_symptoms", "nwss"),
          ## c("google_symptoms", "nwss_region"),
          c("google_symptoms", "va_covid_per_100k"),
          ## c("nwss", "nwss_region"),
          ## c("nwss", "va_covid_per_100k"),
        ),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # nhsn
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
          c("nssp", "google_symptoms", "va_covid_per_100k"),
          # "nwss", "nwss_region",  # missing
        ),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # nhsn
            c(0, 7), # nssp
            c(0, 7), # google symptoms
            ## c(0, 7), # nwss
            ## c(0, 7), # nwss_region
            c(0, 7), # va_covid_per_100k
          ),
          list2(
            c(0, 7, 14, 21), # nhsn
            c(0, 7), # nssp
            c(0, 7, 14), # google symptoms
            ## c(0, 7, 14), # nwss
            ## c(0, 7, 14), # nwss_region
            c(0, 7, 14), # va_covid_per_100k
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
      lags = list2(
        c(0, 7, 14, 21),
        c(0, 7)
      ),
      pop_scaling = FALSE,
      n_training = Inf,
      seasonal_method = list2(
        list2("covid"),
        list2("window"),
        list2("covid", "window"),
        list2("climatological"),
        list2("climatological", "window")
      )
    ),
    climate_linear = bind_rows(
      expand_grid(
        forecaster = "climate_linear_ensembled",
        scale_method = "quantile",
        center_method = "median",
        nonlin_method = c("quart_root", "none"),
        model_used = c("climate_linear", "climate", "climatological_forecaster"),
        filter_agg_level = "state",
        drop_non_seasons = c(TRUE, FALSE),
        quantiles_by_geo = c(TRUE, FALSE),
        aheads = list(g_aheads),
        residual_tail = 0.70,
        residual_center = 0.127
      ),
      expand_grid(
        forecaster = "climate_linear_ensembled",
        scale_method = "none",
        center_method = "none",
        nonlin_method = c("quart_root", "none"),
        model_used = c("climate_linear", "climate", "climatological_forecaster"),
        filter_agg_level = "state",
        drop_non_seasons = c(TRUE, FALSE),
        quantiles_by_geo = c(TRUE, FALSE),
        aheads = list(g_aheads),
        residual_tail = 0.97,
        residual_center = 0.097
      ),
      # only linear, a bunch of the parameters don't matter for it
      expand_grid(
        forecaster = "climate_linear_ensembled",
        scale_method = "none",
        center_method = "none",
        nonlin_method = "none",
        model_used = "linear",
        filter_agg_level = "state",
        aheads = list(g_aheads),
        residual_tail = 0.97,
        residual_center = 0.097
      ),
    )
  ) %>%
    map(function(x) {
      if (g_dummy_mode) {
        x$forecaster <- "dummy_forecaster"
      }
      x <- add_id(x)
      # Add the outcome to each forecaster.
      x$outcome <- "value"
      x
    })

  # Make sure all ids are unique.
  stopifnot(
    length(out$id %>% unique()) == length(out$id)
  )
  out
}
