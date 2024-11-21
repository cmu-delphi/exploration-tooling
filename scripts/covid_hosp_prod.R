# The COVID Hospitalization Production Forecasting Pipeline.
#

# date,forecaster,geo,weight
# 2024-11-20,linear,all,3     # Set a global weight of 3
# 2024-11-20,linear,all,0     # Exclude linear for all forecasts
# 2024-11-20,all,ak,0         # Exclude all forecasters for Alaska
# 2024-11-20,linear,ak,0.1    # Set a weight of 0.1 for linear in Alaska

source("scripts/targets-common.R")

submission_directory <- Sys.getenv("COVID_SUBMISSION_DIRECTORY", "cache")
insufficient_data_geos <- c("as", "mp", "vi", "gu")
# date to cut the truth data off at, so we don't have too much of the past
truth_data_date <- "2023-09-01"
# Generically set the generation date to the next Wednesday (or today if it's Wednesday)
forecast_generation_date <- ceiling_date(Sys.Date() - 1, unit = "week", week_start = 3)

all_states <- unique(readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv", show_col_types = FALSE)$state_id)
forecaster_fns <- list2(
  linear = function(...) {
    forecaster_baseline_linear(..., residual_tail = 0.97, residual_center = 0.097)
  },
  # linearlog = function(...) {
  #   forecaster_baseline_linear(..., log = TRUE)
  # },
  climate_base = function(...) {
    climatological_model(
      ...,
    )
  },
  climate_geo_agged = function(...) {
    climatological_model(
      ...,
      geo_agg = TRUE
    )
  },
  climate_quantile_extrapolated = function(...) {
    climatological_model(
      ...,
      quantile_method = "epipredict"
    )
  },
)
geo_forecasters_weights <- parse_prod_weights(here::here("covid_geo_exclusions.csv"))
geo_exclusions <- exclude_geos(geo_forecasters_weights)


rlang::list2(
  tar_target(
    aheads,
    command = {
      -1:3
    }
  ),
  tar_target(
    forecasters,
    command = {
      seq_along(forecaster_fns)
    }
  ),
  tar_map(
    values = tidyr::expand_grid(
      tibble(
        forecast_generation_date = forecast_generation_date
      )
    ),
    names = "forecast_generation_date",
    tar_target(
      nhsn_latest_data,
      command = {
        nhsn_archive <- s3readRDS(object = "nhsn_archive.rds", bucket = "forecasting-team-data")
        nhsn_archive %>%
          epix_as_of(nhsn_archive$versions_end) %>%
          filter(disease == "nhsn_covid") %>%
          select(-disease) %>%
          filter(geo_value %nin% insufficient_data_geos)
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      forecast_res,
      command = {
        nhsn_latest_data %>%
          as_epi_df(as_of = as.Date(forecast_generation_date)) %>%
          forecaster_fns[[forecasters]](ahead = aheads) %>%
          mutate(
            forecaster = names(forecaster_fns[forecasters]),
            geo_value = as.factor(geo_value)
          )
      },
      pattern = cross(aheads, forecasters),
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      name = ensemble_res,
      command = {
        forecast_res %>%
          mutate(quantile = round(quantile, digits = 3)) %>%
          left_join(geo_forecasters_weights) %>%
          mutate(value = value * weight) %>%
          group_by(forecast_date, geo_value, target_end_date, quantile) %>%
          summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          filter(geo_value %nin% geo_exclusions)
      },
    ),
    tar_target(
      name = make_submission_csv,
      command = {
        ensemble_res %>%
          format_flusight(disease = "covid") %>%
          write_submission_file(as.Date(forecast_generation_date), submission_directory)
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      name = truth_data,
      command = {
        nssp_state <- pub_covidcast(
          source = "nssp",
          signal = "pct_ed_visits_covid",
          time_type = "week",
          geo_type = "state",
          geo_values = "*",
          fetch_args = epidatr::fetch_args_list(timeout_seconds = 400)
        ) %>%
          select(geo_value, source, target_end_date = time_value, value) %>%
          filter(target_end_date > truth_data_date, geo_value %nin% insufficient_data_geos) %>%
          mutate(target_end_date = target_end_date + 6)
        truth_data <- nhsn_latest_data %>%
          mutate(target_end_date = time_value) %>%
          filter(time_value > truth_data_date) %>%
          mutate(source = "nhsn") %>%
          select(geo_value, target_end_date, source, value)
        nssp_renormalized <-
          nssp_state %>%
          left_join(
            nssp_state %>%
              rename(nssp = value) %>%
              full_join(
                truth_data %>%
                  select(geo_value, target_end_date, value),
                by = join_by(geo_value, target_end_date)
              ) %>%
              group_by(geo_value) %>%
              summarise(rel_max_value = max(value, na.rm = TRUE) / max(nssp, na.rm = TRUE)),
            by = join_by(geo_value)
          ) %>%
          mutate(value = value * rel_max_value) %>%
          select(-rel_max_value)
        truth_data %>% bind_rows(nssp_renormalized)
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      notebook,
      command = {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          "scripts/forecast_report.Rmd",
          output_file = here::here(
            "reports",
            sprintf("covid_forecast_report_%s.html", as.Date(forecast_generation_date))
          ),
          params = list(
            disease = "covid",
            forecast_res = forecast_res,
            ensemble_res = ensemble_res,
            forecast_generation_date = as.Date(forecast_generation_date),
            truth_data = truth_data
          )
        )
      },
      cue = tar_cue(mode = "always")
    )
  ),
)
