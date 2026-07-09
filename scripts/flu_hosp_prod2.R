# SPIKE (REFACTOR.md Exp 5): flu prod with the forecaster grid as a loop.
#
# Stops at forecast_nhsn_full / forecast_nssp_full -- the only outputs worth
# comparing, since every downstream target (ensembles, scores) consumes those two
# frames unchanged. Diff these against the `flu_hosp_prod` targets of the same
# name to check the loop reproduces the tar_map grid.
#
# Contrast with scripts/_flu_prod_shared.R:160-248: the whole FORECAST TARGETS
# tar_map + build_combined_forecast_targets tar_combine collapse to one
# tar_target calling flu_run_forecast_grid().
suppressPackageStartupMessages(source("R/load_all.R"))
source("scripts/_flu_prod_shared.R") # globals + g_forecaster_params_grid
source("R/flu_forecast_loop.R")

g_backtest_mode <- FALSE
g_forecast_generation_dates <- Sys.Date()
g_forecast_dates <- round_date(g_forecast_generation_dates, "weeks", week_start = 3)

# ---- data targets (the archive prefix of build_flu_prod_pipeline, verbatim) ----
data_targets <- rlang::list2(
  tar_target(aheads, command = g_aheads),
  tar_target(forecast_dates, command = g_forecast_dates),
  tar_file(flu_data_substitutions, command = "scripts/flu_data_substitutions.csv"),
  create_flu_data_targets(),
  tar_target(
    joined_latest_extra_data,
    command = {
      joined_archive_data %>%
        epix_as_of(joined_archive_data$versions_end) %>%
        mutate(epiweek = epiweek(time_value), epiyear = epiyear(time_value)) %>%
        filter((agg_level == "state") | (agg_level == "nation")) %>%
        select(geo_value, source, time_value, hhs, season, season_week, epiweek, epiyear) %>%
        rename(value = hhs) %>%
        filter(source != "nhsn")
    }
  ),
  tar_target(
    name = nhsn_archive_data,
    command = get_nhsn_data_archive("flu"),
    cue = tar_cue("always")
  ),
  tar_target(
    name = nssp_archive_data,
    command = up_to_date_nssp_state_archive("influenza"),
    cue = tar_cue("always")
  )
)

# ---- the bet: grid tar_map + tar_combine -> one looping target ----
forecast_full_targets <- rlang::list2(
  tar_target(
    forecast_full,
    command = flu_run_forecast_grid(
      grid = g_forecaster_params_grid,
      forecast_dates = g_forecast_dates,
      forecast_generation_dates = g_forecast_generation_dates,
      aheads = g_aheads,
      nhsn_archive_data = nhsn_archive_data,
      nssp_archive_data = nssp_archive_data,
      joined_latest_extra_data = joined_latest_extra_data,
      flu_data_substitutions = flu_data_substitutions,
      insufficient_data_geos = g_insufficient_data_geos
    )
  ),
  tar_target(forecast_nhsn_full, command = forecast_full$nhsn),
  tar_target(forecast_nssp_full, command = forecast_full$nssp)
)

list2(data_targets, forecast_full_targets)
