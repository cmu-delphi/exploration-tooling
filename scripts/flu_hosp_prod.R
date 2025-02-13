# The Flu Hospitalization Production Forecasting Pipeline.
source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

submit_climatological <- TRUE
submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
insufficient_data_geos <- c("as", "mp", "vi", "gu")
excluded_geos <- c("as", "gu", "mh")
# date to cut the truth data off at, so we don't have too much of the past
truth_data_date <- "2023-09-01"
# needed to create the aux data targets
end_date <- Sys.Date()

# This is the as_of for the forecast. If run on our typical schedule, it's
# today, which is a Wednesday. Sometimes, if we're doing a delayed forecast,
# it's a Thursday. It's used for stamping the data and for determining the
# appropriate as_of when creating the forecast.
forecast_generation_date <- Sys.Date()
# Usually, the forecast_date is the same as the generation date, but you can
# override this. It should be a Wednesday.
forecast_date <- round_date(forecast_generation_date, "weeks", week_start = 3)
# If doing backfill, you can set the forecast_date to a sequence of dates.
# forecast_date <- seq.Date(as.Date("2024-11-20"), Sys.Date(), by = 7L)
# forecast_generation_date needs to follow suit, but it's more complicated
# because sometimes we forecast on Thursday.
# forecast_generation_date <- c(as.Date(c("2024-11-21", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")), seq.Date(as.Date("2025-01-08"), Sys.Date(), by = 7L))

very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))

forecaster_fns <- list2(
  linear = function(epi_data, ahead, extra_data, ...) {
    epi_data %>%
      filter(source == "nhsn") %>%
      forecaster_baseline_linear(
        ahead, ...,
        residual_tail = 0.99,
        residual_center = 0.35,
        no_intercept = TRUE
      )
  },
  # linearlog = function(...) {
  #   forecaster_baseline_linear(..., log = TRUE)
  # },
  climate_base = function(epi_data, ahead, extra_data, ...) {
    epi_data %>%
      filter(source == "nhsn") %>%
      climatological_model(ahead, ...)
  },
  climate_geo_agged = function(epi_data, ahead, extra_data, ...) {
    epi_data %>%
      filter(source == "nhsn") %>%
      climatological_model(ahead, ..., geo_agg = TRUE)
  },
  windowed_seasonal = function(epi_data, ahead, extra_data, ...) {
    scaled_pop_seasonal(
      epi_data,
      outcome = "value",
      ahead = ahead * 7,
      ...,
      trainer = epipredict::quantile_reg(),
      seasonal_method = "window",
      pop_scaling = FALSE,
      lags = c(0, 7),
      keys_to_ignore = very_latent_locations
    ) %>%
      mutate(target_end_date = target_end_date + 3)
  },
  windowed_seasonal_extra_sources = function(epi_data, ahead, extra_data, ...) {
    fcst <-
      epi_data %>%
      left_join(extra_data, by = join_by(geo_value, time_value)) %>%
      scaled_pop_seasonal(
        outcome = "value",
        ahead = ahead * 7,
        extra_sources = "nssp",
        ...,
        seasonal_method = "window",
        trainer = epipredict::quantile_reg(),
        drop_non_seasons = TRUE,
        pop_scaling = FALSE,
        lags = list(c(0, 7), c(0, 7)),
        keys_to_ignore = very_latent_locations
      ) %>%
      mutate(target_end_date = target_end_date + 3)
    fcst
  }
)
indices <- seq_along(forecaster_fns)

# This is needed to build the data archive
ref_time_values_ <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7L)

smooth_last_n <- function(x, n = 1, k = 2) {
  x[(length(x) - (n - 1)):length(x)] <- mean(x[(length(x) - (k - 1)):length(x)], na.rm = TRUE)
  x
}

rlang::list2(
  rlang::list2(
    tar_target(aheads, command = -1:3),
    tar_target(forecasters, command = indices),
    tar_target(name = ref_time_values, command = ref_time_values_),
  ),
  make_historical_flu_data_targets(),
  tar_target(
    current_nssp_archive,
    command = {
      up_to_date_nssp_state_archive("influenza")
    },
    cue = tar_cue(mode = "always")
  ),
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
    nhsn_latest_data,
    command = {
      if (wday(Sys.Date()) < 6 & wday(Sys.Date()) > 3) {
        # download from the preliminary data source from Wednesday to Friday
        most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
      } else {
        most_recent_result <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
      }
      most_recent_result <-
        most_recent_result %>%
        process_nhsn_data() %>%
        filter(disease == "nhsn_flu") %>%
        select(-disease) %>%
        filter(geo_value %nin% insufficient_data_geos) %>%
        mutate(
          source = "nhsn",
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value - 3
        ) %>%
        filter(version == max(version)) %>%
        select(-version) %>%
        data_substitutions(disease = "flu", forecast_generation_date) %>%
        as_epi_df(other_keys = "source", as_of = Sys.Date())
      most_recent_result
    },
    description = "Download the result, and update the file only if it's actually different",
    priority = 1,
    cue = tar_cue(mode = "always")
  ),
  tar_map(
    # Because targets relies on R metaprogramming, it loses the Date class.
    values = tibble(
      forecast_date_int = forecast_date,
      forecast_generation_date_int = forecast_generation_date,
      forecast_date_chr = as.character(forecast_date_int)
    ),
    names = "forecast_date_chr",
    tar_change(
      name = geo_forecasters_weights,
      command = {
        geo_forecasters_weights <- parse_prod_weights(here::here("flu_geo_exclusions.csv"), forecast_date_int, forecaster_fns)
        if (nrow(geo_forecasters_weights %>% filter(forecast_date == as.Date(forecast_date_int))) == 0) {
          cli_abort("there are no weights  for the forecast date {forecast_date}")
        }
        geo_forecasters_weights
      },
      change = here::here("flu_geo_exclusions.csv")
    ),
    tar_target(
      name = geo_exclusions,
      command = exclude_geos(geo_forecasters_weights)
    ),
    tar_target(
      full_data,
      command = {
        if (as.Date(forecast_generation_date_int) < Sys.Date()) {
          train_data <- nhsn_archive_data %>%
            epix_as_of(as.Date(forecast_generation_date_int)) %>%
            add_season_info() %>%
            mutate(
              source = "nhsn",
              geo_value = ifelse(geo_value == "usa", "us", geo_value),
              time_value = time_value - 3
            )
        } else {
          train_data <- nhsn_latest_data
        }
        full_data <- train_data %>%
          bind_rows(joined_latest_extra_data)
        attributes(full_data)$metadata$other_keys <- "source"
        attributes(full_data)$metadata$as_of <- as.Date(forecast_date_int)
        full_data
      }
    ),
    tar_target(
      forecast_res,
      command = {
        forecast_date <- as.Date(forecast_date_int)
        nssp <- current_nssp_archive %>% epix_as_of(min(forecast_date, current_nssp_archive$versions_end))
        full_data %>%
          forecaster_fns[[forecasters]](ahead = aheads, extra_data = nssp) %>%
          mutate(
            forecaster = names(forecaster_fns[forecasters]),
            geo_value = as.factor(geo_value)
          )
      },
      pattern = cross(aheads, forecasters)
    ),
    # A hack to model our uncertainty in the data. We smooth the last few points
    # to make the forecast more stable.
    tar_target(
      forecast_res_modified,
      command = {
        as_of <- attributes(full_data)$metadata$as_of
        other_keys <- attributes(full_data)$metadata$other_keys
        forecast_date <- as.Date(forecast_date_int)
        nssp <- current_nssp_archive %>% epix_as_of(min(forecast_date, current_nssp_archive$versions_end))

        # Smooth last few points for every geo.
        # TODO: This is a hack, we can try some more sophisticated
        # smoothing/nowcasting here.
        modified_full_data <- full_data %>%
          filter(source == "nhsn") %>%
          arrange(geo_value, time_value) %>%
          group_by(geo_value) %>%
          mutate(value = smooth_last_n(value)) %>%
          ungroup()
        # Add back in the non-nhsn data.
        modified_full_data <- modified_full_data %>%
          bind_rows(full_data %>% filter(source != "nhsn"))

        attributes(modified_full_data)$metadata$as_of <- as_of
        attributes(modified_full_data)$metadata$other_keys <- other_keys
        modified_full_data %>%
          forecaster_fns[[forecasters]](ahead = aheads, extra_data = nssp) %>%
          mutate(
            forecaster = names(forecaster_fns[forecasters]),
            geo_value = as.factor(geo_value)
          )
      },
      pattern = cross(aheads, forecasters),
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      name = climate_linear,
      command = {
        forecast_res %>%
          # Apply the ahead-by-quantile weighting scheme
          ensemble_linear_climate(aheads, other_weights = geo_forecasters_weights) %>%
          filter(geo_value %nin% geo_exclusions) %>%
          ungroup() %>%
          sort_by_quantile()
      }
    ),
    tar_target(
      name = ens_climate_linear_window_season,
      command = {
        climate_linear %>%
          # Ensemble with windowed_seasonal and windowed_seasonal_extra_sources
          bind_rows(forecast_res %>% filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))) %>%
          group_by(geo_value, forecast_date, target_end_date, quantile) %>%
          summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          sort_by_quantile()
      }
    ),
    tar_target(
      name = ens_ar_only,
      command = {
        forecast_res %>%
          filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources")) %>%
          group_by(geo_value, forecast_date, target_end_date, quantile) %>%
          summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          sort_by_quantile()
      }
    ),
    tar_target(
      name = climate_linear_modified,
      command = {
        forecast_res_modified %>%
          # Apply the ahead-by-quantile weighting scheme
          ensemble_linear_climate(aheads, other_weights = geo_forecasters_weights) %>%
          filter(geo_value %nin% geo_exclusions) %>%
          ungroup() %>%
          sort_by_quantile()
      }
    ),
    tar_target(
      name = ens_climate_linear_window_season_modified,
      command = {
        climate_linear_modified %>%
          # Ensemble with windowed_seasonal
          bind_rows(forecast_res_modified %>% filter(forecaster %in% c("windowed_seasonal", "windowed_seasonal_extra_sources"))) %>%
          group_by(geo_value, forecast_date, target_end_date, quantile) %>%
          summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          sort_by_quantile()
      }
    ),
    tar_target(
      name = combo_ens_climate_linear_window_season,
      command = {
        inner_join(
          ens_climate_linear_window_season, ens_climate_linear_window_season_modified,
          by = join_by(geo_value, forecast_date, target_end_date, quantile)
        ) %>%
          rowwise() %>%
          mutate(value = ifelse(quantile > 0.5, max(value.x, value.y), NA)) %>%
          mutate(value = ifelse(quantile < 0.5, min(value.x, value.y), value)) %>%
          mutate(value = ifelse(quantile == 0.5, (value.x + value.y) / 2, value)) %>%
          select(geo_value, forecast_date, target_end_date, quantile, value) %>%
          ungroup()
      }
    ),
    tar_target(
      name = forecasts_and_ensembles,
      command = {
        bind_rows(
          forecast_res,
          climate_linear %>% mutate(forecaster = "climate_linear"),
          ens_ar_only %>% mutate(forecaster = "ens_ar_only"),
          ens_climate_linear_window_season %>% mutate(forecaster = "ensemble_linclim_windowed_seasonal"),
          combo_ens_climate_linear_window_season %>% mutate(forecaster = "ensemble_combo")
        )
      }
    ),
    tar_target(
      name = make_submission_csv,
      command = {
        combo_ens_climate_linear_window_season %>%
          format_flusight(disease = "flu") %>%
          write_submission_file(
            get_forecast_reference_date(forecast_date_int),
            file.path(submission_directory, "model-output/CMU-TimeSeries")
          )
      }
    ),
    tar_target(
      name = make_climate_submission_csv,
      command = {
        if (submit_climatological) {
          forecasts <- forecast_res
          forecasts %>%
            filter(forecaster %in% c("climate_base", "climate_geo_agged")) %>%
            group_by(geo_value, target_end_date, quantile) %>%
            summarize(forecast_date = first(forecast_date), value = mean(value, na.rm = TRUE), .groups = "drop") %>%
            ungroup() %>%
            filter(!(geo_value %in% excluded_geos)) %>%
            format_flusight(disease = "flu") %>%
            filter(location %nin% c("60", "66", "78")) %>%
            write_submission_file(
              get_forecast_reference_date(forecast_date_int),
              submission_directory = file.path(submission_directory, "model-output/CMU-climate_baseline"),
              file_name = "CMU-climate_baseline"
            )
        }
      },
      priority = 0.99
    ),
    tar_target(
      name = validate_result,
      command = {
        make_submission_csv
        # only validate if we're saving the result to a hub
        if (submission_directory != "cache") {
          validation <- validate_submission(
            submission_directory,
            file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(forecast_date_int))
          )
        } else {
          validation <- "not validating when there is no hub (set submission_directory)"
        }
        validation
      },
    ),
    tar_target(
      name = validate_climate_result,
      command = {
        make_climate_submission_csv
        # only validate if we're saving the result to a hub
        if (submission_directory != "cache" && submit_climatological) {
          validation <- validate_submission(
            submission_directory,
            file_path = sprintf("CMU-climate_baseline/%s-CMU-climate_baseline.csv", get_forecast_reference_date(forecast_date_int))
          )
        } else {
          validation <- "not validating when there is no hub (set submission_directory)"
        }
        validation
      },
    ),
    tar_target(
      name = truth_data,
      command = {
        date <- forecast_generation_date_int
        nssp_state <-
          current_nssp_archive %>%
          epix_as_of(min(as.Date(date), current_nssp_archive$versions_end)) %>%
          rename(target_end_date = time_value) %>%
          filter(target_end_date > truth_data_date, geo_value %nin% insufficient_data_geos) %>%
          mutate(target_end_date = target_end_date + 6)
        if (as.Date(forecast_generation_date_int) < Sys.Date()) {
          truth_dat <- nhsn_archive_data %>% epix_as_of(as.Date(forecast_generation_date_int))
        } else {
          truth_dat <- nhsn_latest_data
        }
        truth_dat <- truth_dat %>%
          mutate(target_end_date = time_value) %>%
          filter(time_value > truth_data_date) %>%
          mutate(source = "nhsn") %>%
          select(geo_value, target_end_date, source, value)
        nssp_renormalized <-
          nssp_state %>%
          rename(value = nssp) %>%
          left_join(
            nssp_state %>%
              full_join(
                truth_dat %>%
                  select(geo_value, target_end_date, value),
                by = c("geo_value", "target_end_date")
              ) %>%
              group_by(geo_value) %>%
              summarise(rel_max_value = max(value, na.rm = TRUE) / max(nssp, na.rm = TRUE)),
            by = "geo_value"
          ) %>%
          mutate(value = value * rel_max_value) %>%
          select(-rel_max_value)
        truth_dat %>% bind_rows(nssp_renormalized)
      },
    ),
    tar_target(
      notebook,
      command = {
        if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
        rmarkdown::render(
          "scripts/reports/forecast_report.Rmd",
          output_file = here::here(
            "reports",
            sprintf("%s_flu_prod_on_%s.html", as.Date(forecast_date_int), Sys.Date())
          ),
          params = list(
            disease = "flu",
            forecast_res = forecasts_and_ensembles,
            forecast_date = as.Date(forecast_date_int),
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
