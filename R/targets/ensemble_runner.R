# Shared runner for the prod ensemble layer (analog of run_forecaster() in
# R/targets/forecaster_runner.R). Each disease declares a small ensemble spec
# (see g_ensemble_specs in scripts/flu_hosp_prod.R / scripts/covid_hosp_prod.R)
# describing the three ensembles per signal -- climate_linear, ens_ar_only,
# ensemble_mix -- and build_prod_ensemble_targets() (R/targets/prod_shared.R)
# calls run_ensemble() once per (ensemble, signal, date) cell with the spec
# values spliced in as literals.

# Fail loudly if a declared component forecaster is missing from the input
# rather than letting mean(na.rm = TRUE) or a join silently drop it.
assert_components_present <- function(forecasts, components, id) {
  missing <- setdiff(components, unique(forecasts$forecaster))
  if (length(missing) > 0) {
    cli::cli_abort("ensemble {id}: missing declared component forecaster{?s} {.field {missing}}.")
  }
}

#' Run one prod ensemble cell and validate its output.
#'
#' @param method             "climate_linear", "mean", or "weighted".
#' @param id                 ensemble id, stamped onto the output's `forecaster`
#'                           column and used in error messages.
#' @param forecasts          the per-signal `forecast_filtered` frame. For
#'                           "climate_linear" this must be the FULL frame
#'                           (not pre-filtered to `components`):
#'                           ensemble_climate_linear() computes its own
#'                           `last_data`/`forecast_date` latency from
#'                           `min(target_end_date)`/`min(forecast_date)` on the
#'                           input BEFORE it filters internally via
#'                           `grepl("climate|linear", forecaster)`;
#'                           pre-filtering here could change that latency
#'                           computation. `components` is used only for the
#'                           presence assert in that case.
#' @param components         declared component forecaster ids for this
#'                           ensemble/signal; asserted present in `forecasts`
#'                           and (for "mean"/"weighted") used to filter it.
#' @param weights            geo_weights$<signal> tibble ("climate_linear" and
#'                           "weighted" only).
#' @param aheads             the `aheads` target ("climate_linear" only).
#' @param climate_caps       c(max_ahead_weight, max_quantile_weight)
#'                           ("climate_linear" only).
#' @param geo_exclusions     geo_value vector to drop, or NULL to skip the
#'                           filter (only climate_linear applies it).
#' @param drop_negative_aheads for "weighted": keep only `forecast_date <
#'                           target_end_date` rows (positive aheads) of the AR
#'                           components before combining.
#' @param extra_forecasts    for "weighted": additional already-computed rows
#'                           (the ensemble_clim_lin output) bound in before
#'                           ensemble_weighted().
#' @param sort_quantiles     enforce quantile monotonicity on the output
#'                           (climate_linear and ens_ar_only; NOT ensemble_mix
#'                           -- preserve verbatim, see notes/2026-07-19).
#' @return validated forecast tibble stamped `forecaster = id`.
run_ensemble <- function(
  method, id, forecasts, components,
  weights = NULL, aheads = NULL, climate_caps = NULL,
  geo_exclusions = NULL, drop_negative_aheads = FALSE,
  extra_forecasts = NULL, sort_quantiles = FALSE
) {
  assert_components_present(forecasts, components, id)
  out <- switch(method,
    climate_linear = {
      forecasts %>%
        ensemble_climate_linear(
          aheads,
          other_weights = weights,
          max_climate_ahead_weight = climate_caps[1],
          max_climate_quantile_weight = climate_caps[2]
        ) %>%
        filter(geo_value %nin% geo_exclusions) %>%
        ungroup()
    },
    mean = {
      forecasts %>%
        filter(forecaster %in% components) %>%
        group_by(geo_value, forecast_date, target_end_date, quantile) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
    },
    weighted = {
      ar <- forecasts %>% filter(forecaster %in% components)
      if (drop_negative_aheads) {
        ar <- ar %>% filter(forecast_date < target_end_date)
      }
      bind_rows(extra_forecasts, ar) %>%
        ensemble_weighted(weights)
    },
    cli::cli_abort("run_ensemble: unknown method {.val {method}}")
  )
  if (sort_quantiles) {
    out <- out %>% sort_by_quantile()
  }
  out %>%
    mutate(forecaster = id) %>%
    validate_forecast_output(id)
}
