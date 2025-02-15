source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# ================================ GLOBALS =================================
# These globals are needed by make_forecasts_and_scores (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
hhs_signal <- "confirmed_admissions_covid_1d"
# The date when the forecast was generated (this is effectively the AS OF date).
forecast_generation_dates <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7L)
# The reference date for the forecast.
forecast_dates <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7L)
# This moves the week marker from Saturday to Wednesday
time_value_adjust <- 3
# Directory for reports.
reports_dir <- "reports"
# Fetch arguments for epidatr.
fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
# Debug mode will replace all forecaster functions with a fast dummy forecaster. Helps
# with prototyping the pipeline.
dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))
# For testing, reduce dates
# forecast_generation_dates <- forecast_generation_dates[1:10]
# forecast_dates <- forecast_dates[1:10]


# ================================ FORECASTER PARAMETERS ====================
# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations <- rlang::list2(
  scaled_pop_main = tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = list("linreg", "quantreg"),
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
        ## c("dr_visits", "google_symptoms"),
        ## c("dr_visits", "nssp"),
        ## c("dr_visits", "nwss"),
        ## c("dr_visits", "nwss_region"),
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
        c("nssp", "google_symptoms", "nwss", "nwss_region"),
      ),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7), # nssp
          c(0, 7), # google symptoms
          c(0, 7), # nwss
          c(0, 7), # nwss_region
        ),
        list2(
          c(0, 7, 14, 21), # hhs
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
  scaled_pop_season = tidyr::expand_grid(
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
  map(add_id) %>%
  # Add the outcome to each forecaster.
  map(function(x) {
    x$outcome <- "hhs"
    x
  })

# Make sure all ids are unique.
stopifnot(length(forecaster_parameter_combinations$id %>% unique()) == length(forecaster_parameter_combinations$id))

# Internals for targets pipeline.
# Three lists which are used at runtime and must maintain the same order.
# - forecaster_functions_list: list of forecaster base functions (e.g. scaled_pop)
# - forecaster_names_list: list of forecaster names (e.g. 'spiniferous.lcont')
# - params_list: list of parameters for each forecaster (a list of lists)
# The last one can also be used for parameter lookup, given a forecaster name.
forecaster_functions_list <- forecaster_parameter_combinations %>%
  map(function(x) {
    x %>% select(forecaster)
  }) %>%
  bind_rows() %>%
  pull(forecaster)
forecaster_names_list <- forecaster_parameter_combinations %>%
  map(function(x) {
    x %>% select(id)
  }) %>%
  bind_rows() %>%
  pull(id)
names(forecaster_functions_list) <- forecaster_names_list
params_list <- forecaster_parameter_combinations %>%
  map(make_params_list) %>%
  set_names(NULL) %>%
  unlist(recursive = FALSE)
names(params_list) <- forecaster_names_list
# Create a partially applied forecaster function for each id. This function
# depends on params_list and forecaster_functions_list, which are defined above.
# If we don't define these variables here, then targets won't have access at
# runtime.
get_partially_applied_forecaster <- function(id) {
  function(epi_data, ...) {
    forecaster_args <- rlang::dots_list(
      ...,
      !!!params_list[[id]],
      .homonyms = "last"
    )
    # This uses string lookup to get the function.
    forecaster_fn <- get(forecaster_functions_list[[id]])
    rlang::inject(forecaster_fn(epi_data = epi_data, !!!forecaster_args))
  }
}


# ================================ TARGETS =================================
# ================================ PARAMETERS TARGETS ======================
parameter_targets <- list2(
  tar_target(name = aheads, command = c(0, 7, 14, 21, 28)),
  tar_target(name = ref_time_values, command = forecast_dates),
  # This is used for parameter lookup.
  tar_target(name = forecaster_parameter_grid, command = forecaster_parameter_combinations),
  # This is used for generating notebooks.
  tar_target(name = forecaster_families, command = forecaster_parameter_combinations %>% names()),
)
# ================================ DATA TARGETS ==============================
data_targets <- list2(
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
    name = hhs_evaluation_data,
    command = {
      retry_fn(
        max_attempts = 10,
        wait_seconds = 1,
        fn = pub_covidcast,
        source = "hhs",
        signals = hhs_signal,
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = "*",
        fetch_args = fetch_args
      ) %>%
        select(signal, geo_value, time_value, value) %>%
        daily_to_weekly(keys = c("geo_value", "signal")) %>%
        select(signal, geo_value, target_end_date = time_value, true_value = value) %>%
        # Correction for timing offsets
        mutate(target_end_date = target_end_date + 3)
    }
  ),
  tar_target(
    name = state_geo_values,
    command = {
      hhs_evaluation_data %>%
        pull(geo_value) %>%
        unique()
    }
  ),
  tar_target(
    name = nssp_archive,
    command = {
      nssp_state <- retry_fn(
        max_attempts = 10,
        wait_seconds = 1,
        fn = pub_covidcast,
        source = "nssp",
        signal = "pct_ed_visits_covid",
        time_type = "week",
        geo_type = "state",
        geo_values = "*",
        fetch_args = epidatr::fetch_args_list(timeout_seconds = 400)
      )
      nssp_hhs <- retry_fn(
        max_attempts = 10,
        wait_seconds = 1,
        fn = pub_covidcast,
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
    name = google_symptoms_archive,
    command = {
      used_searches <- c(4, 5)
      # not using actual versions here because the only revision behavior is the
      # source going down completely, which means we're actually just comparing
      # with the version without this source
      all_of_them <- lapply(used_searches, \(search_name) {
        google_symptoms_state_archive <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "google-symptoms",
          signal = glue::glue("s0{search_name}_smoothed_search"),
          time_type = "day",
          geo_type = "state",
          geo_values = "*"
        )
        google_symptoms_hhs_archive <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
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
    name = hhs_region_data,
    command = {
      state_to_hhs <- readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv")
      state_to_id <- readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv")
      state_to_hhs %>%
        left_join(state_to_id, by = "state_code") %>%
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
        add_hhs_region_sum(hhs_region_data) %>%
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
      # TODO: This is a hack to ensure the as_of data is cached. Maybe there's a better way.
      epix_slide_simple(joined_archive_data, dummy_forecaster, ref_time_values, cache_key = "joined_archive_data")
      joined_archive_data
    }
  ),
  tar_target(
    validate_joined_archive_data,
    command = {
      # TODO: This can be a bit more granular (per geo, per source, etc.)
      min_time_value <- joined_archive_data$DT %>%
        distinct(time_value) %>%
        pull(time_value) %>%
        min()
      if (min_time_value > (ref_time_values[1] - 30)) {
        stop("Joined archive data does not have at least 30 days of training data for the earliest forecast date.
             Update your forecast_dates to be later than ", min_time_value + 30, ".")
      }
    }
  )
)

forecasts_and_scores <- tar_map(
  values = list(id = forecaster_names_list),
  unlist = FALSE,
  tar_target(
    name = forecast,
    command = {
      forecaster_fn <- function(epi_data) get_partially_applied_forecaster(id)(epi_data = epi_data, ahead = aheads)
      # if (aheads == 14) {
      #   browser()
      # }

      browser()
      out <- epix_slide_simple(
        joined_archive_data,
        forecaster_fn,
        ref_time_values,
        cache_key = "joined_archive_data"
      ) %>%
        rename(prediction = value) %>%
        mutate(ahead = as.numeric(target_end_date - forecast_date)) %>%
        mutate(id = id)
      gc()
      return(out)
    },
    pattern = map(aheads)
  ),
  tar_target(
    name = score,
    command = {
      # If the data has already been scaled, hhs needs to include the
      # population and undo scaling.
      if ("population" %in% colnames(hhs_evaluation_data)) {
        actual_eval_data <- hhs_evaluation_data %>% select(-population)
        forecast_scaled <- forecast %>%
          left_join(
            hhs_evaluation_data %>% distinct(geo_value, population),
            by = "geo_value"
          ) %>%
          mutate(prediction = prediction * population / 10L**5)
      } else {
        forecast_scaled <- forecast
        actual_eval_data <- hhs_evaluation_data
      }
      forecast_scaled <- forecast_scaled %>%
        mutate(forecast_date = forecast_date + 3, target_end_date = target_end_date + 3) %>%
        rename("model" = "id")

      # browser()
      # Score
      evaluate_predictions(forecasts = forecast_scaled, truth_data = actual_eval_data) %>%
        rename("id" = "model")
    }
  )
)
combined_forecasts_and_scores <- rlang::list2(
  forecasts_and_scores,
  tar_combine(
    delphi_forecasts,
    forecasts_and_scores[["forecast"]],
    command = {
      dplyr::bind_rows(!!!.x) %>%
        rename(forecaster = id) %>%
        filter(geo_value %in% state_geo_values) %>%
        mutate(forecast_date = forecast_date + 3, target_end_date = target_end_date + 3)
    }
  ),
  tar_combine(
    delphi_scores,
    forecasts_and_scores[["score"]],
    command = {
      dplyr::bind_rows(!!!.x) %>%
        rename(forecaster = id) %>%
        filter(geo_value %in% state_geo_values)
    }
  ),
)
external_forecasts_and_scores <- rlang::list2(
  tar_target(
    outside_forecaster_subset,
    command = c("COVIDhub-baseline", "COVIDhub-trained_ensemble", "COVIDhub_CDC-ensemble")
  ),
  tar_target(
    external_forecasts_file,
    command = {
      s3load("covid19_forecast_hub_2023_full_summed.rds", bucket = "forecasting-team-data", verbose = FALSE)
      full_results %>%
        mutate(target_end_date = as.Date(forecast_date) + 7 * as.numeric(week_ahead)) %>%
        rename(ahead = week_ahead)
    }
  ),
  tar_target(
    external_forecasts,
    command = {
      external_forecasts_file %>%
        filter(geo_value %in% state_geo_values, forecaster %in% outside_forecaster_subset) %>%
        mutate(forecast_date = forecast_date + 5, target_end_date = target_end_date + 5) %>%
        filter(forecast_date %in% (ref_time_values + 3)) %>%
        rename(prediction = value) %>%
        mutate(prediction = prediction * 7)
    }
  ),
  tar_target(
    external_scores,
    command = {
      actual_eval_data <- hhs_evaluation_data
      filtered_forecasts <- external_forecasts %>%
        rename(model = forecaster)
      evaluate_predictions(forecasts = filtered_forecasts, truth_data = actual_eval_data) %>%
        rename(forecaster = model)
    }
  )
)
joined_forecasts_and_scores <- rlang::list2(
  tar_target(joined_forecasts, command = delphi_forecasts %>% bind_rows(external_forecasts)),
  tar_target(joined_scores, command = delphi_scores %>% bind_rows(external_scores)),
  tar_target(
    family_notebooks,
    command = {
      actual_eval_data <- hhs_evaluation_data
      delphi_forecaster_subset <- forecaster_parameter_combinations[[forecaster_families]]$id

      filtered_forecasts <- joined_forecasts %>%
        filter(forecaster %in% c(delphi_forecaster_subset, outside_forecaster_subset))
      filtered_scores <- joined_scores %>%
        filter(forecaster %in% c(delphi_forecaster_subset, outside_forecaster_subset))

      # TODO: Write an assert to make sure that these dates are similar. It's a bit tricky.
      # actual_eval_data %>%
      #   filter(target_end_date > "2023-09-01") %>%
      #   distinct(target_end_date) %>%
      #   pull(target_end_date) %>%
      #   sort()
      # filtered_forecasts %>%
      #   distinct(target_end_date) %>%
      #   pull(target_end_date) %>%
      #   sort()
      # filtered_scores %>%
      #   distinct(target_end_date) %>%
      #   pull(target_end_date) %>%
      #   sort()
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
  )
)

rlang::list2(
  parameter_targets,
  data_targets,
  combined_forecasts_and_scores,
  external_forecasts_and_scores,
  joined_forecasts_and_scores
)
