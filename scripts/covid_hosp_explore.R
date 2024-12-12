source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# These globals are needed by make_forecasts_and_scores (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
hhs_signal <- "confirmed_admissions_covid_1d"
ref_time_values_ <- as.Date(c("2023-11-08", "2023-11-22"))
if (!exists("ref_time_values_")) {
  # Alternatively you can let slide_forecaster figure out ref_time_values
  start_date <- as.Date("2023-10-04")
  end_date <- as.Date("2024-04-24")
  date_step <- 7L
  ref_time_values_ <- NULL
}
time_value_adjust <- 3 # this moves the week marker from Saturday to Wednesday

# Debug mode will replace all forecasters with a fast dummy forecaster. Helps
# with prototyping the pipeline.
dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))

# these are for datasets which have some locations with extreme latency; currently that's only nwss
# note that this has a vector of names first, and then the list of things to
# exclude, because targets hates named lists and strips their names
very_latent_locations <- list(list(
  c("geo_value"),
  c("la", "ms", "nd")
))
# TODO decide on this, nwss is latent in these locations by ~56 days. might be ok

# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations_ <- rlang::list2(
  scaled_pop_main = tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("linreg", "quantreg"),
    lags = list(
      c(0, 7),
      c(0, 7, 14),
      c(0, 7, 14, 21),
      c(0, 7, 14, 21, 28)
    ),
    pop_scaling = FALSE,
    n_training = Inf
  ),
  tidyr::expand_grid(
    forecaster = "flatline_fc",
  ),
  # using exogenous variables
  scaled_pop_exogenous = bind_rows(
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "google_symptoms_4_bronchitis", "google_symptoms", "nwss", "nwss_region"),
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
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "",
      filter_agg_level = "",
      n_training = Inf,
      drop_non_seasons = FALSE,
    ),
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      extra_sources = list2(
        c("dr_visits", "google_symptoms"),
        c("dr_visits", "nssp"),
        c("dr_visits", "nwss"),
        c("dr_visits", "nwss_region"),
        c("nssp", "google_symptoms"),
        c("nssp", "nwss"),
        c("nssp", "nwss_region"),
        c("google_symptoms", "nwss"),
        c("google_symptoms", "nwss_region"),
        c("nwss", "nwss_region")
      ),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7), # first feature
          c(0, 7) # second feature
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "",
      filter_agg_level = "",
      n_training = Inf,
      drop_non_seasons = FALSE,
    ),
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      extra_sources = list2(
        c("dr_visits", "nssp", "google_symptoms", "nwss", "nwss_region"),
      ),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7), # dr visits
          c(0, 7), # nssp
          c(0, 7), # google symptoms
          c(0, 7), # nwss
          c(0, 7), # nwss_region
        ),
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7, 14), # dr visits
          c(0, 7), # nssp
          c(0, 7, 14), # google symptoms
          c(0, 7, 14), # nwss
          c(0, 7, 14), # nwss_region
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "",
      filter_agg_level = "",
      n_training = Inf,
      drop_non_seasons = FALSE,
    )
  ),
  scled_pop_season = tidyr::expand_grid(
    forecaster = "scaled_pop_seasonal",
    trainer = "quantreg",
    lags = list(
      c(0, 7, 14, 21),
      c(0, 7)
    ),
    pop_scaling = FALSE,
    n_training = Inf,
    seasonal_method = list(c("covid"), c("window"), c("covid", "window"), c("climatological"), c("climatological", "window"))
  )
) %>%
  map(function(x) {
    if (dummy_mode) {
      x$forecaster <- "dummy_forecaster"
    }
    x
  }) %>%
  map(add_id)

# Make sure all ids are unique.
stopifnot(length(forecaster_parameter_combinations_$id %>% unique()) == length(forecaster_parameter_combinations_$id))
# Build targets-internal tibble to map over.
forecaster_grid <- forecaster_parameter_combinations_ %>%
  map(make_forecaster_grid) %>%
  bind_rows()
forecaster_families_ <- setdiff(forecaster_parameter_combinations_ %>% names(), c("flusion_grf"))

scaled_pop_not_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "quantreg",
  lags = c(0, 7, 14, 28),
  pop_scaling = FALSE,
  n_training = Inf
)
scaled_pop_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "quantreg",
  lags = c(0, 7, 14, 28),
  pop_scaling = TRUE,
  n_training = Inf
)
smooth_scaled <- list(
  forecaster = "smoothed_scaled",
  trainer = "quantreg",
  lags =
  # list(smoothed, sd)
    list(c(0, 7, 14, 21, 28), c(0)),
  smooth_width = as.difftime(2, units = "weeks"),
  sd_width = as.difftime(4, units = "weeks"),
  sd_mean_width = as.difftime(2, units = "weeks"),
  pop_scaling = TRUE,
  n_training = Inf
)
# Human-readable object to be used for inspecting the ensembles in the pipeline.
ensemble_parameter_combinations_ <- tribble(
  ~ensemble, ~ensemble_args, ~forecasters,
  # mean forecaster
  "ensemble_average",
  list(average_type = "mean"),
  list(
    scaled_pop_scaled,
    list(forecaster = "flatline_fc")
  ),
  # median forecaster
  "ensemble_average",
  list(average_type = "median"),
  list(
    scaled_pop_scaled,
    scaled_pop_not_scaled,
    smooth_scaled
  ),
  # mean forecaster with baseline
  "ensemble_average",
  list(average_type = "mean"),
  list(
    scaled_pop_not_scaled,
    list(forecaster = "flatline_fc")
  ),
  # median forecaster with baseline
  "ensemble_average",
  list(average_type = "median"),
  list(
    scaled_pop_not_scaled,
    list(forecaster = "flatline_fc")
  )
) %>%
  {
    if (dummy_mode) {
      .$forecasters <- map(.$forecasters, function(x) {
        map(x, function(y) {
          y$forecaster <- "dummy_forecaster"
          y
        })
      })
    }
    .
  } %>%
  mutate(
    children_ids = map(.$forecasters, function(x) {
      map_chr(x, function(y) {
        get_single_id(y)
      })
    })
  ) %>%
  add_id(exclude = "forecasters")
# spoofing ensembles for right now
ensemble_parameter_combinations_ <- tibble::tibble(id = character(), ensemble = character(), ensemble_args = character(), children_ids = character())
# Check that every ensemble dependent is actually included.
missing_forecasters <- setdiff(
  ensemble_parameter_combinations_ %>% pull(children_ids) %>% unlist() %>% unique(),
  forecaster_grid$id
)
if (length(missing_forecasters) > 0) {
  cli_abort("Ensemble depends on forecasters not included in pipeline: {missing_forecasters}.")
}
# Build targets-internal tibble to map over.
ensemble_grid <- make_ensemble_grid(ensemble_parameter_combinations_)

fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
# These globals are needed by make_external_names_and_scores
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
project_path <- Sys.getenv("TAR_PROJECT", "")

# Data targets
rlang::list2(
  # Parameter targets
  list2(
    tar_target(name = forecaster_parameter_combinations, command = forecaster_parameter_combinations_),
    tar_target(name = ensemble_forecasters, command = ensemble_parameter_combinations_),
    tar_target(name = aheads, command = c(0, 7, 14, 21)),
    tar_target(name = ref_time_values, command = ref_time_values_),
    tar_target(name = forecaster_families, command = forecaster_families_)
  ),
  # Data targets
  rlang::list2(
    tar_target(
      name = hhs_archive_data_asof,
      command = {
        get_health_data(as.Date(ref_time_values)) %>%
          mutate(version = as.Date(ref_time_values)) %>%
          relocate(geo_value, time_value, version, hhs)
      },
      pattern = map(ref_time_values)
    ),
    tar_target(
      name = hhs_archive,
      command = {
        hhs_archive <- hhs_archive_data_asof %>%
          rename(value = hhs) %>%
          as_epi_archive(compactify = TRUE) %>%
          daily_to_weekly_archive(agg_columns = "value")
        hhs_archive$geo_type <- "state"
        hhs_archive
      }
    ),
    tar_target(
      name = hhs_latest_data,
      command = {
        epidatr::pub_covidcast(
          source = "hhs",
          signals = hhs_signal,
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = "*",
          fetch_args = fetch_args
        )
      }
    ),
    tar_target(
      name = hhs_evaluation_data,
      command = {
        hhs_latest_data %>%
          select(signal, geo_value, time_value, value) %>%
          daily_to_weekly(keys = c("geo_value", "signal")) %>%
          rename(
            true_value = value,
            target_end_date = time_value
          ) %>%
          select(signal, geo_value, target_end_date, true_value)
      }
    ),
    tar_target(
      name = hhs_latest_data_2022,
      command = {
        hhs_latest_data # %>% filter(time_value >= "2022-01-01", time_value < "2022-04-01")
      }
    ),
    tar_target(
      name = nssp_archive,
      command = {
        nssp_state <- pub_covidcast(
          source = "nssp",
          signal = "pct_ed_visits_covid",
          time_type = "week",
          geo_type = "state",
          geo_values = "*",
          fetch_args = epidatr::fetch_args_list(timeout_seconds = 400)
        )
        nssp_hhs <- pub_covidcast(
          source = "nssp",
          signal = "pct_ed_visits_covid",
          time_type = "week",
          geo_type = "hhs",
          geo_values = "*"
        )
        nssp_archive <- nssp_state %>%
          bind_rows(nssp_hhs) %>%
          select(geo_value, time_value, issue, nssp = value) %>%
          as_epi_archive(compactify = TRUE) %>%
          `$`("DT") %>%
          # weekly data is indexed from the start of the week
          mutate(time_value = time_value + 6 - time_value_adjust) %>%
          mutate(version = time_value) %>%
          as_epi_archive(compactify = TRUE)
        nssp_archive
      }
    ),
    tar_target(
      name = hosp_admissions_archive_raw,
      command = {
        # this data set is not worth the effort
      }
    ),
    tar_target(
      name = google_symptoms_archive,
      command = {
        used_searches <- c(4, 5)
        # not using actual versions here because the only revision behavior is the
        # source going down completely, which means we're actually just comparing
        # with the version without this source
        all_of_them <- lapply(used_searches, \(search_name) {
          google_symptoms_state_archive <- pub_covidcast(
            source = "google-symptoms",
            signal = glue::glue("s0{search_name}_smoothed_search"),
            time_type = "day",
            geo_type = "state",
            geo_values = "*"
          )
          google_symptoms_hhs_archive <- pub_covidcast(
            source = "google-symptoms",
            signal = glue::glue("s0{search_name}_smoothed_search"),
            time_type = "day",
            geo_type = "hhs",
            geo_values = "*"
          )
          google_symptoms_archive_min <-
            google_symptoms_state_archive %>%
            bind_rows(google_symptoms_hhs_archive) %>%
            select(geo_value, time_value, value) %>%
            daily_to_weekly() %>%
            mutate(version = time_value) %>%
            as_epi_archive(compactify = TRUE)
          google_symptoms_archive_min$DT %>%
            filter(!is.na(value)) %>%
            relocate(geo_value, time_value, version, value) %>%
            as_epi_archive(compactify = TRUE)
        })
        all_of_them[[1]]$DT %<>% rename(google_symptoms_4_bronchitis = value)
        all_of_them[[2]]$DT %<>% rename(google_symptoms_5_ageusia = value)
        google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]])
        google_symptoms_archive <- google_symptoms_archive$DT %>%
          mutate(google_symptoms = google_symptoms_4_bronchitis + google_symptoms_5_ageusia) %>%
          as_epi_archive(compactify = TRUE)
        # not just using dplyr to allow for na.rm
        google_symptoms_archive$DT$google_symptoms <-
          rowSums(google_symptoms_archive$DT[, c("google_symptoms_4_bronchitis", "google_symptoms_5_ageusia")],
            na.rm = TRUE
          )
        pre_pipeline <- google_symptoms_archive %>%
          epix_as_of(as.Date("2023-10-04")) %>%
          mutate(source = "none")
        colnames <- c("google_symptoms_4_bronchitis", "google_symptoms_5_ageusia", "google_symptoms")
        for (colname in colnames) {
          learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
          google_symptoms_archive$DT %<>% data_whitening(colname = colname, learned_params)
        }
        google_symptoms_archive$DT %>%
          select(-starts_with("source")) %>%
          as_epi_archive(compactify = TRUE)
      }
    ),
    # TODO: DV takes too long
    # tar_target(
    #   name = doctor_visits_archive_raw,
    #   command = {
    #     years <- seq(from = 0, to = 00040000, by = 00010000)
    #     all_of_them <- lapply(years, \(year) {
    #       time_values1 <- epidatr::epirange(20200102 + year, 20200601 + year)
    #       time_values2 <- epidatr::epirange(20200602 + year, 20210101 + year)
    #       dr_visits1 <- pub_covidcast(
    #         source = "doctor-visits",
    #         signal = "smoothed_cli",
    #         time_type = "day",
    #         time_values = time_values1,
    #         geo_type = "state",
    #         geo_values = "*",
    #         issues = "*"
    #       )
    #       dr_visits2 <- pub_covidcast(
    #         source = "doctor-visits",
    #         signal = "smoothed_cli",
    #         time_type = "day",
    #         time_values = time_values2,
    #         geo_type = "state",
    #         geo_values = "*",
    #         issues = "*"
    #       )
    #       bind_rows(
    #         dr_visits1,
    #         dr_visits2
    #       ) %>%
    #         select(
    #           geo_value, time_value, issue, value
    #         )
    #     })
    #     all_of_them %>%
    #       bind_rows()
    #   }
    # ),
    # tar_target(
    #   name = doctor_visits_weekly_archive,
    #   command = {
    #     # compactify to remove reduntant issues
    #     doctor_visits_archive <- doctor_visits_archive_raw %>%
    #       rename(dr_visits = value) %>%
    #       drop_na() %>%
    #       as_epi_archive(compactify = TRUE)
    #     # filter to versions at the end of the week (when we'd be doing the sum)
    #     doctor_visits_archive$DT %<>% filter(wday(version, week_start = 7) == 7)
    #     doctor_visits_weekly_archive <- doctor_visits_archive %>%
    #       daily_to_weekly_archive(agg_columns = "dr_visits")
    #     doctor_visits_weekly_archive <- doctor_visits_weekly_archive$DT %>%
    #       as_epi_archive(compactify = TRUE)
    #     doctor_visits_weekly_archive
    #   }
    # ),
    tar_target(
      name = nwss_coarse,
      command = {
        files <- file.info(list.files(here::here("aux_data/nwss_covid_data/"), full.names = TRUE))
        most_recent <- rownames(files)[which.max(files$mtime)]
        nwss <- readr::read_csv(most_recent) %>%
          rename(value = state_med_conc) %>%
          arrange(geo_value, time_value)
        state_code <- readr::read_csv(here::here("aux_data", "flusion_data", "state_codes_table.csv"), show_col_types = FALSE)
        hhs_codes <- readr::read_csv(here::here("aux_data", "flusion_data", "state_code_hhs_table.csv"), show_col_types = FALSE)
        state_to_hhs <- hhs_codes %>%
          left_join(state_code, by = "state_code") %>%
          select(hhs_region = hhs, geo_value = state_id)
        nwss %<>%
          add_pop_and_density() %>%
          drop_na() %>%
          select(-agg_level, -year, -agg_level, -population, -density)
        pop_data <- gen_pop_and_density_data()
        nwss_hhs_region <-
          nwss %>%
          left_join(state_to_hhs, by = "geo_value") %>%
          mutate(year = year(time_value)) %>%
          left_join(pop_data, by = join_by(geo_value, year)) %>%
          select(-year, density) %>%
          group_by(time_value, hhs_region) %>%
          summarize(
            value = sum(value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            activity_level = sum(activity_level * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            region_value = mean(region_value * population) / sum(population, na.rm = TRUE),
            national_value = sum(national_value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(agg_level = "hhs_region", hhs_region = as.character(hhs_region)) %>%
          rename(geo_value = hhs_region)
        nwss %>%
          mutate(agg_level = "state") %>%
          bind_rows(nwss_hhs_region) %>%
          select(geo_value, time_value, nwss = value, nwss_region = region_value, nwss_national = national_value) %>%
          mutate(time_value = time_value - 3, version = time_value) %>%
          arrange(geo_value, time_value) %>%
          as_epi_archive(compactify = TRUE)
      }
    ),
    tar_target(
      name = hhs_region,
      command = {
        hhs_region <- readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv")
        state_id <- readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv")
        hhs_region %>%
          left_join(state_id, by = "state_code") %>%
          select(hhs_region = hhs, geo_value = state_id) %>%
          mutate(hhs_region = as.character(hhs_region))
      }
    ),
    tar_target(
      name = joined_archive_data,
      command = {
        # reformt hhs_archive, remove data spotty locations
        joined_archive_data <- hhs_archive$DT %>%
          select(geo_value, time_value, value, version) %>%
          rename("hhs" := value) %>%
          add_hhs_region_sum(hhs_region) %>%
          filter(geo_value != "us") %>%
          as_epi_archive(
            compactify = TRUE
          )
        joined_archive_data$geo_type <- "custom"
        # drop aggregated geo_values
        joined_archive_data <- joined_archive_data %>%
          epix_merge(nwss_coarse, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        # TODO: Maybe bring these back
        # epix_merge(doctor_visits_weekly_archive, sync = "locf") %>%
        joined_archive_data %<>%
          epix_merge(nssp_archive, sync = "locf")
        joined_archive_data$geo_type <- "custom"
        joined_archive_data %<>%
          epix_merge(google_symptoms_archive, sync = "locf")
        joined_archive_data$DT %<>% filter(grepl("[a-z]{2}", geo_value), !(geo_value %in% c("as", "pr", "vi", "gu", "mp")))
        joined_archive_data$geo_type <- "state"
        slide_forecaster(
          epi_archive = joined_archive_data,
          outcome = "hhs",
          ahead = 0,
          forecaster = dummy_forecaster,
          n_training_pad = 30L,
          forecaster_args = list(),
          forecaster_args_names = list(),
          ref_time_values = ref_time_values,
          start_date = start_date,
          end_date = end_date,
          date_range_step_size = date_step,
          cache_key = "joined_archive_data"
        )
        joined_archive_data
      }
    )
  ),
  make_forecasts_and_scores(),
  make_ensembles_and_scores(),
  # make_external_names_and_scores(),
  tar_target(
    external_forecasts,
    command = {
      s3load("flusight_forecasts_2023.rds", bucket = "forecasting-team-data")
      flusight_forecasts_2023
    }
  ),
  tar_target(
    external_scores,
    command = {
      actual_eval_data <- hhs_evaluation_data %>%
        mutate(target_end_date = target_end_date + 3)
      cmu_forecast_dates <- ref_time_values_ + 3
      filtered_forecasts <- external_forecasts %>%
        filter(forecast_date %in% cmu_forecast_dates) %>%
        rename(model = forecaster)
      evaluate_predictions(predictions_cards = filtered_forecasts, truth_data = actual_eval_data) %>%
        rename(forecaster = model)
    }
  ),
  # TODO: Score here also.
  # make_ensembles_and_scores()
  tar_target(
    joined_forecasts,
    command = {
      delphi_forecasts %>% bind_rows(external_forecasts)
    }
  ),
  tar_target(
    joined_scores,
    command = {
      delphi_scores %>% bind_rows(external_scores)
    }
  ),
  tar_target(
    family_notebooks,
    command = {
      actual_eval_data <- hhs_evaluation_data %>%
        select(-population) %>%
        mutate(target_end_date = target_end_date + 3)
      delphi_forecaster_subset <- forecaster_parameter_combinations[[forecaster_families]]$id
      outside_forecaster_subset <- c("COVIDhub-baseline", "COVIDhub-ensemble")
      filtered_forecasts <- joined_forecasts %>%
        filter(forecaster %in% c(delphi_forecaster_subset, outside_forecaster_subset))
      filtered_scores <- joined_scores %>%
        filter(forecaster %in% c(delphi_forecaster_subset, outside_forecaster_subset))
      forecaster_parameters <- forecaster_parameter_combinations[[forecaster_families]]
      rmarkdown::render(
        "scripts/reports/comparison-notebook.Rmd",
        params = list(
          forecaster_parameters = forecaster_parameters,
          forecaster_family = forecaster_families,
          forecasts = filtered_forecasts,
          scores = filtered_scores,
          truth_data = actual_eval_data,
          disease = "covid"
        ),
        output_file = here::here(reports_dir, paste0("covid-notebook-", forecaster_families, ".html"))
      )
    },
    pattern = map(forecaster_families)
  ),
)
