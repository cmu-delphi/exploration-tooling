# SPIKE (REFACTOR.md Exp 5): the forecaster grid as a plain loop instead of a
# tar_map + tar_combine. Reproduces the full_data / forecast_nssp / forecast_nhsn
# cells for every (forecaster x date x ahead) and returns the two bound frames
# that the tar_map produced as forecast_nhsn_full / forecast_nssp_full.
#
# No caching (the open question in Exp 5). This is throwaway code to see the shape.
flu_run_forecast_grid <- function(grid,
                                   forecast_dates,
                                   forecast_generation_dates,
                                   aheads,
                                   nhsn_archive_data,
                                   nssp_archive_data,
                                   joined_latest_extra_data,
                                   flu_data_substitutions,
                                   insufficient_data_geos) {
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

    # ---- full_data (once per forecaster x date; was the `full_data` target) ----
    train_data <- flu_slice_archive(nhsn_archive_data, version_policy, forecast_generation_date_int)
    train_data <- train_data %>%
      add_season_info() %>%
      mutate(
        geo_value = ifelse(geo_value == "usa", "us", geo_value),
        time_value = time_value - 3,
        source = "nhsn"
      )
    if (version_policy != "latest") {
      train_data <- train_data %>%
        data_substitutions(flu_data_substitutions, as.Date(forecast_generation_date_int))
    }
    train_data <- train_data %>% filter(geo_value %nin% insufficient_data_geos)
    attributes(train_data)$metadata$as_of <- as.Date(forecast_date_int)
    full_data <- train_data %>% bind_rows(joined_latest_extra_data)
    attributes(full_data)$metadata$other_keys <- "source"
    attributes(full_data)$metadata$as_of <- as.Date(forecast_date_int)

    # ---- one forecast per ahead (was `pattern = map(aheads)`) ----
    nssp_rows <- purrr::map(aheads, function(ahead) {
      nssp_data <- flu_slice_archive(nssp_archive_data, version_policy, forecast_generation_date_int)
      forecaster_fn <- get_partially_applied_forecaster(forecaster_obj, ahead, params, param_names)
      nssp_data <- nssp_data %>%
        rename(value = nssp) %>%
        mutate(time_value = floor_date(time_value, "week", week_start = 7) + 3) %>%
        mutate(source = "nhsn") %>%
        add_season_info()
      attributes(nssp_data)$metadata$as_of <- as.Date(forecast_date_int)
      attributes(nssp_data)$metadata$other_keys <- "source"
      full_data_modified <- full_data %>%
        rename(nssp = value) %>%
        filter(source == "nhsn") %>%
        select(-c(source, epiweek, epiyear, season, season_week))
      nssp_data %>%
        forecaster_fn(extra_data = full_data_modified) %>%
        mutate(forecaster = id, geo_value = as.factor(geo_value))
    }) %>% bind_rows()

    nhsn_rows <- purrr::map(aheads, function(ahead) {
      # NOTE: latest cutoff is the forecast date here, not the generation date
      # (asymmetry vs full_data / forecast_nssp), preserved from the original.
      nssp_data <- flu_slice_archive(
        nssp_archive_data, version_policy, forecast_generation_date_int,
        latest_cutoff = forecast_date_int
      )
      forecaster_fn <- get_partially_applied_forecaster(forecaster_obj, ahead, params, param_names)
      full_data %>%
        forecaster_fn(extra_data = nssp_data) %>%
        mutate(forecaster = id, geo_value = as.factor(geo_value))
    }) %>% bind_rows()

    list(nhsn = nhsn_rows, nssp = nssp_rows)
  })

  list(
    nhsn = purrr::map(results, "nhsn") %>% bind_rows(),
    nssp = purrr::map(results, "nssp") %>% bind_rows()
  )
}
