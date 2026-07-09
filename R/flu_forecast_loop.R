# SPIKE (REFACTOR.md Exp 5): the forecaster grid as a plain loop instead of a
# tar_map + tar_combine. Builds forecaster input via the SAME functions the
# console uses (R/flu_forecast_input.R), so the batch path and the dev path
# cannot drift. No caching (the open Exp 5 question); throwaway code.
flu_run_forecast_grid <- function(grid,
                                   forecast_dates,
                                   forecast_generation_dates,
                                   aheads,
                                   archives,
                                   insufficient_data_geos = g_insufficient_data_geos_default) {
  cells <- tidyr::expand_grid(
    grid,
    tibble(
      forecast_date_int = forecast_dates,
      forecast_generation_date_int = forecast_generation_dates
    )
  )

  results <- purrr::pmap(cells, function(id, forecaster, params, param_names,
                                         version_policy, forecast_date_int,
                                         forecast_generation_date_int) {
    forecaster_obj <- get(as.character(forecaster))

    one_signal <- function(signal) {
      inp <- flu_forecast_input(
        forecast_date = forecast_date_int,
        signal = signal,
        version_policy = version_policy,
        generation_date = forecast_generation_date_int,
        archives = archives,
        insufficient_data_geos = insufficient_data_geos
      )
      # one call per ahead (was `pattern = map(aheads)`). NULL param columns
      # must become empty lists so set_names() in the partial applier is happy
      # (tar_map substituted these as literals; a plain loop passes NULL).
      params <- params %||% list()
      param_names <- param_names %||% list()
      purrr::map(aheads, function(ahead) {
        forecaster_fn <- get_partially_applied_forecaster(forecaster_obj, ahead, params, param_names)
        inp$epi_data %>%
          forecaster_fn(extra_data = inp$extra_data) %>%
          mutate(forecaster = id, geo_value = as.factor(geo_value))
      }) %>% bind_rows()
    }

    list(nhsn = one_signal("nhsn"), nssp = one_signal("nssp"))
  })

  list(
    nhsn = purrr::map(results, "nhsn") %>% bind_rows(),
    nssp = purrr::map(results, "nssp") %>% bind_rows()
  )
}
