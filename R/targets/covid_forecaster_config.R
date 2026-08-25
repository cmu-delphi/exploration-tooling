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
    # NOTE: nwss / nwss_region / va_covid_per_100k are temporarily removed as
    # exogenous sources -- their upstream feeds are retired and will be
    # reintroduced through new endpoints that aren't wired up yet. Only the live
    # sources (nssp, google_symptoms) remain; restore the combinations below when
    # the new endpoints land.
    scaled_pop_exogenous = bind_rows(
      # a single exogenous source
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        extra_sources = list2("nssp", "google_symptoms"),
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
      # both live exogenous sources together
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        extra_sources = list2(c("nssp", "google_symptoms")),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # nssp
            c(0, 7) # google_symptoms
          ),
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # nssp
            c(0, 7, 14) # google_symptoms
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
    # Revision-aware analog of the `window` seasonal method: trains on past
    # vintages (needs_archive hands it the truncated archive). The covid archive
    # has a single (unversioned-key) source, so there is no include/exclude-faux
    # `train_sources` knob as there is for flu -- just the whitening contrast
    # (`quart_root` vs `none`, whose ^4 coloring can blow up the upper tail).
    # sort_quantiles is set here since the covid config map (unlike flu's) does
    # not add it, and quantile_reg output can cross.
    revision_aware = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal_revision",
      trainer = "quantreg_fn",
      lags = list2(c(0, 7)),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = c("quart_root", "none"),
      seasonal_backward_window = 5 * 7,
      seasonal_forward_window = 3 * 7,
      needs_archive = TRUE,
      sort_quantiles = TRUE
    ),
    # Revision-aware with nssp as an exogenous predictor. No train_sources knob
    # since the covid explore archive has no source key (single-source nhsn).
    revision_aware_nssp = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal_revision",
      trainer = "quantreg_fn",
      lags = list2(c(0, 7)),
      extra_sources = list("nssp"),
      pop_scaling = FALSE,
      scale_method = "none",
      center_method = "none",
      nonlin_method = "none",
      seasonal_backward_window = 5 * 7,
      seasonal_forward_window = 3 * 7,
      needs_archive = TRUE,
      sort_quantiles = TRUE,
      outlier_n_weeks = c(NA_integer_, 4L)
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
      x$outcome <- "hhs"
      # Whitening in the scaled_pop family (scale_method != "none") produces
      # occasional tiny quantile crossings; opt those forecasters into
      # monotonicity enforcement (flu does this for every family via its own
      # config map). Monotone-by-construction families (climate, flatline) stay
      # strict so a real crossing still surfaces as an error. Set after add_id so
      # the spec column stays out of the id hash and existing ids/caches hold.
      x$sort_quantiles <- x$forecaster %in% c("scaled_pop", "scaled_pop_seasonal", "scaled_pop_seasonal_revision")
      x
    })

  # Make sure all ids are unique.
  stopifnot(
    length(out$id %>% unique()) == length(out$id)
  )
  out
}
