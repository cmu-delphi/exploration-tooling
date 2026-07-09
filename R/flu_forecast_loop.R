# The flu forecaster grid as a plain loop over the signal-expanded grid
# (flu_build_prod2_grid). Each row is one (forecaster, outcome_signal) config;
# flu_assemble builds its input (honest labels, no spoof), the forecaster is called
# with (extra_sources, primary_source), and rows are split back into
# forecast_nhsn_full / forecast_nssp_full by outcome_signal. Replaces the old
# forecast_nhsn/forecast_nssp tar_map + tar_combine in _flu_prod_shared.R.
#
# Seeding: a single hardcoded global seed per (cell x ahead) -- enough to make the
# deterministic forecasters reproducible and the golden test stable. Does NOT match
# targets' per-target seed, so the three stochastic forecasters (cdc_baseline,
# linear, linear_no_population_scale) won't match flu_hosp_prod exactly (see
# REFACTOR.md finding 4). Per-cell seeding is deferred.
flu_run_forecast_grid <- function(grid,
                                   forecast_dates,
                                   forecast_generation_dates,
                                   aheads,
                                   archives,
                                   insufficient_data_geos = g_insufficient_data_geos_default,
                                   seed = 42) {
  cells <- tidyr::expand_grid(
    grid,
    tibble(
      forecast_date_int = forecast_dates,
      forecast_generation_date_int = forecast_generation_dates
    )
  )

  rows <- purrr::pmap(cells, function(id, forecaster, version_policy, outcome_signal,
                                      exogenous, primary_source, forecast_date_int,
                                      forecast_generation_date_int) {
    fn <- get(forecaster)
    frame <- flu_assemble(
      archives,
      outcome_signal = outcome_signal,
      exogenous = exogenous,
      version_policy = version_policy,
      generation_date = forecast_generation_date_int,
      forecast_date = forecast_date_int,
      insufficient_data_geos = insufficient_data_geos
    )
    out <- purrr::map(aheads, function(ahead) {
      set.seed(seed)
      frame %>%
        fn(ahead, extra_sources = exogenous, primary_source = primary_source) %>%
        mutate(forecaster = id, geo_value = as.factor(geo_value))
    }) %>% bind_rows()
    list(signal = outcome_signal, out = out)
  })

  list(
    nhsn = purrr::map(purrr::keep(rows, ~ .x$signal == "nhsn"), "out") %>% bind_rows(),
    nssp = purrr::map(purrr::keep(rows, ~ .x$signal == "nssp"), "out") %>% bind_rows()
  )
}
