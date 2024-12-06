# The Flu Hospitalization Production Forecasting Pipeline.
#

source("scripts/targets-common.R")

submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
insufficient_data_geos <- c("as", "mp", "vi", "gu")
# date to cut the truth data off at, so we don't have too much of the past
truth_data_date <- "2023-09-01"
# Generically set the generation date to the next Wednesday (or today if it's Wednesday)
forecast_generation_date <- Sys.Date()

forecaster_fns <- list2(
  linear = function(...) {
    forecaster_baseline_linear(..., residual_tail = 0.99, residual_center = 0.35)
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
)
geo_forecasters_weights <- parse_prod_weights(here::here("flu_geo_exclusions.csv"), forecast_generation_date)
geo_exclusions <- exclude_geos(geo_forecasters_weights)
if (nrow(geo_forecasters_weights %>% filter(forecast_date == forecast_generation_date)) == 0) {
  geo_forecasters_weights
  cli_abort("there are no weights  for the forecast date {forecast_generation_date}")
}


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
        if (wday(Sys.Date()) < 6 & wday(Sys.Date()) > 3) {
          # download from the preliminary data source from Wednesday to Friday
          most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
        } else {
          most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
        }
        most_recent_result %>%
          process_nhsn_data() %>%
          filter(disease == "nhsn_flu") %>%
          select(-disease) %>%
          filter(geo_value %nin% insufficient_data_geos)
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      forecast_res,
      command = {
        nhsn <- nhsn_latest_data
        nhsn <- nhsn %>%
          as_epi_df(as_of = as.Date(forecast_generation_date))
        attributes(nhsn)$metadata$as_of <- as.Date(forecast_generation_date)
        nhsn %>%
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
        forecasts <- forecast_res
        forecasts %>%
          mutate(quantile = round(quantile, digits = 3)) %>%
          left_join(geo_forecasters_weights, by = join_by(forecast_date, forecaster, geo_value)) %>%
          mutate(value = value * weight) %>%
          group_by(forecast_date, geo_value, target_end_date, quantile) %>%
          summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          filter(geo_value %nin% geo_exclusions)
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      name = ensemble_mixture_res,
      command = {
        forecast_res %>%
          ensemble_linear_climate(aheads, other_weights = geo_forecasters_weights) %>%
          filter(geo_value %nin% geo_exclusions) %>%
          ungroup()
      },
    ),
    tar_target(
      name = make_submission_csv,
      command = {
        ensemble_mixture_res %>%
          format_flusight(disease = "flu") %>%
          write_submission_file(
            get_forecast_reference_date(as.Date(forecast_generation_date)),
            file.path(submission_directory, "model-output/CMU-TimeSeries")
          )
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      name = validate_result,
      command = {
        make_submission_csv
        # only validate if we're saving the result to a hub
        if (submission_directory != "cache") {
          validation <- validate_submission(
            submission_directory,
            file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(as.Date(forecast_generation_date)))
          )
        } else {
          validation <- "not validating when there is no hub (set submission_directory)"
        }
        validation
      },
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      name = truth_data,
      command = {
        nssp_state <- pub_covidcast(
          source = "nssp",
          signal = "pct_ed_visits_influenza",
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
          "scripts/reports/forecast_report.Rmd",
          output_file = here::here(
            "reports",
            sprintf("%s_flu_prod.html", as.Date(forecast_generation_date))
          ),
          params = list(
            disease = "flu",
            forecast_res = forecast_res %>% bind_rows(ensemble_mixture_res %>% mutate(forecaster = "ensemble_mix")),
            ensemble_res = ensemble_res,
            forecast_generation_date = as.Date(forecast_generation_date),
            truth_data = truth_data
          )
        )
      },
      cue = tar_cue(mode = "always")
    )
  ),
  tar_target(
    new_data_notebook,
    command = {
      rmarkdown::render("scripts/reports/new_data.Rmd", output_file = here::here("reports", "new_data.html"))
    }
  )
)
