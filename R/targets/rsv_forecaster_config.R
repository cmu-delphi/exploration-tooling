#' Configuration parameters for RSV hospitalization forecasting
#'
#' This file contains configuration parameters used by the RSV hospitalization
#' forecasting pipeline. The grid follows the covid grid, since both target raw
#' NHSN counts from a single-source archive.

#' Get forecaster parameter combinations for RSV forecasting
#'
#' Variables with 'g_' prefix are globals defined in the calling script.
#'
#' Differences from the covid grid:
#' - no `seasonal_method = "covid"` (PCA over past seasons); RSV has at most
#'   two usable seasons, too few for a stable seasonal PC.
#' - no va_respiratory or nwss exogenous sources; neither is wired up for RSV.
#'
#' @return A list of forecaster parameter combinations
#' @export
get_rsv_forecaster_params <- function() {
  # Families that also forecast h−1, and the aheads each runs.
  nowcast_run_aheads <- list(
    revision_aware = c(g_nowcast_aheads, g_aheads),
    revision_ratio = g_nowcast_aheads
  )
  whitening_bundles <- tibble(
    scale_method = c("quantile", "none"),
    center_method = c("median", "none"),
    nonlin_method = c("quart_root", "none"),
    residual_tail = c(0.70, 0.97),
    residual_center = c(0.127, 0.097)
  )
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
    scaled_pop_exogenous = expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      extra_sources = list2("nssp"),
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
        list2("window"),
        list2("climatological"),
        list2("climatological", "window")
      )
    ),
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
      finalization_coverage = 0.8,
      needs_archive = TRUE,
      sort_quantiles = TRUE
    ),
    # Nowcast baseline for h−1: the reported value times recent revision ratios.
    revision_ratio = tibble(
      forecaster = "revision_ratio_nowcast",
      needs_archive = TRUE
    ),
    revision_aware_nssp = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal_revision",
      trainer = "quantreg_fn",
      lags = list2(c(0, 7)),
      extra_sources = list("nssp"),
      pop_scaling = TRUE,
      scale_method = "none",
      center_method = "none",
      nonlin_method = "none",
      seasonal_backward_window = 5 * 7,
      seasonal_forward_window = 3 * 7,
      finalization_coverage = 0.8,
      needs_archive = TRUE,
      sort_quantiles = TRUE,
      outlier_n_weeks = c(NA_integer_, 4L)
    ),
    # Beds as revision proxy. Version history only exists from Dec 2024+, so
    # training rows before that drop out (NA beds lags).
    revision_aware_beds_no_season = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal_revision",
      trainer = "quantreg_fn",
      lags = list2(c(0, 7)),
      extra_sources = list(
        "inpatient_beds_ew",
        c("inpatient_beds_ew", "nssp")
      ),
      pop_scaling = TRUE,
      scale_method = "none",
      center_method = "none",
      nonlin_method = "none",
      use_seasonal_window = FALSE,
      finalization_coverage = 0.8,
      needs_archive = TRUE,
      sort_quantiles = TRUE,
      outlier_n_weeks = c(NA_integer_, 4L)
    ),
    # Whitening is all on or all off; the residual widths are tuned per state.
    climate_linear = bind_rows(
      expand_grid(
        forecaster = "climate_linear_ensembled",
        whitening_bundles,
        model_used = c("climate_linear", "climatological_forecaster"),
        filter_agg_level = "state",
        quantiles_by_geo = TRUE,
        aheads = list(g_aheads)
      ),
      # quantiles_by_geo has no effect on the linear model.
      expand_grid(
        forecaster = "climate_linear_ensembled",
        whitening_bundles,
        model_used = "linear",
        filter_agg_level = "state",
        aheads = list(g_aheads)
      )
    )
  ) %>%
    imap(function(x, family) {
      if (g_dummy_mode) {
        x$forecaster <- "dummy_forecaster"
      }
      x <- add_id(x)
      x$outcome <- "hhs"
      # Set after add_id so the spec column stays out of the id hash.
      x$sort_quantiles <- x$forecaster %in% c("scaled_pop", "scaled_pop_seasonal", "scaled_pop_seasonal_revision")
      if (family %in% names(nowcast_run_aheads)) {
        x$run_aheads <- list(nowcast_run_aheads[[family]])
      }
      x
    })

  # Make sure all ids are unique.
  all_ids <- unlist(map(out, "id"))
  stopifnot(!anyDuplicated(names(out)), !anyDuplicated(all_ids))
  out
}
