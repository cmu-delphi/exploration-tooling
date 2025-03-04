source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# ================================ GLOBALS =================================
# These globals are needed by make_forecasts_and_scores (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
config <- list(
  aheads = 0:4 * 7,
  hhs_signal = "confirmed_admissions_covid_1d",
  # The date when the forecast was generated (this is effectively the AS OF date).
  forecast_generation_dates = seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L),
  # The reference date for the forecast.
  forecast_dates = seq.Date(as.Date("2023-11-08"), as.Date("2024-04-24"), by = 7L),
  # This moves the week marker from Saturday to Wednesday
  time_value_adjust = 3,
  # Directory for reports.
  reports_dir = "reports",
  # Fetch arguments for epidatr.
  fetch_args = epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400),
  # Debug mode will replace all forecaster functions with a fast dummy forecaster. Helps
  # with prototyping the pipeline.
  dummy_mode = as.logical(Sys.getenv("DUMMY_MODE", FALSE)),
  insufficient_data_geos = c("as", "pr", "vi", "gu", "mp")
)
# For testing, reduce dates
config$forecast_generation_dates <- config$forecast_generation_dates[1:3]
config$forecast_dates <- config$forecast_dates[1:3]

# ================================ FORECASTER PARAMETERS ====================
# Human-readable object to be used for inspecting the forecasters in the pipeline.
config$forecaster_parameter_combinations <- rlang::list2(
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
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
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
      n_training = Inf
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
      n_training = Inf
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
      n_training = Inf
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
    seasonal_method = list(
      c("covid"),
      c("window"),
      c("covid", "window"),
      c("climatological"),
      c("climatological", "window")
    )
  )
) %>%
  map(function(x) {
    if (config$dummy_mode) {
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
stopifnot(
  length(config$forecaster_parameter_combinations$id %>% unique()) ==
    length(config$forecaster_parameter_combinations$id)
)

# scaled_pop_not_scaled <- list(
#   forecaster = "scaled_pop",
#   trainer = "quantreg",
#   lags = c(0, 7, 14, 28),
#   pop_scaling = FALSE,
#   n_training = Inf
# )
# scaled_pop_scaled <- list(
#   forecaster = "scaled_pop",
#   trainer = "quantreg",
#   lags = c(0, 7, 14, 28),
#   pop_scaling = TRUE,
#   n_training = Inf
# )
# smooth_scaled <- list(
#   forecaster = "smoothed_scaled",
#   trainer = "quantreg",
#   # lags = list(smoothed, sd)
#   lags = list(c(0, 7, 14, 21, 28), c(0)),
#   smooth_width = as.difftime(2, units = "weeks"),
#   sd_width = as.difftime(4, units = "weeks"),
#   sd_mean_width = as.difftime(2, units = "weeks"),
#   pop_scaling = TRUE,
#   n_training = Inf
# )
# # Human-readable object to be used for inspecting the ensembles in the pipeline.
# # fmt: skip
# ensemble_parameter_combinations_ <- tribble(
#   ~ensemble, ~ensemble_args, ~forecasters,
#   # mean forecaster
#   "ensemble_average",
#   list(average_type = "mean"),
#   list(
#     scaled_pop_scaled,
#     list(forecaster = "flatline_fc")
#   ),
#   # median forecaster
#   "ensemble_average",
#   list(average_type = "median"),
#   list(
#     scaled_pop_scaled,
#     scaled_pop_not_scaled,
#     smooth_scaled
#   ),
#   # mean forecaster with baseline
#   "ensemble_average",
#   list(average_type = "mean"),
#   list(
#     scaled_pop_not_scaled,
#     list(forecaster = "flatline_fc")
#   ),
#   # median forecaster with baseline
#   "ensemble_average",
#   list(average_type = "median"),
#   list(
#     scaled_pop_not_scaled,
#     list(forecaster = "flatline_fc")
#   )
# ) %>%
#   {
#     if (dummy_mode) {
#       .$forecasters <- map(.$forecasters, function(x) {
#         map(x, function(y) {
#           y$forecaster <- "dummy_forecaster"
#           y
#         })
#       })
#     }
#     .
#   } %>%
#   mutate(
#     children_ids = map(.$forecasters, function(x) {
#       map_chr(x, function(y) {
#         get_single_id(y)
#       })
#     })
#   ) %>%
#   add_id(exclude = "forecasters")
# # spoofing ensembles for right now
# ensemble_parameter_combinations_ <- tibble::tibble(
#   id = character(),
#   ensemble = character(),
#   ensemble_args = character(),
#   children_ids = character()
# )
# # Check that every ensemble dependent is actually included.
# missing_forecasters <- setdiff(
#   ensemble_parameter_combinations_ %>% pull(children_ids) %>% unlist() %>% unique(),
#   forecaster_grid$id
# )
# if (length(missing_forecasters) > 0) {
#   cli_abort("Ensemble depends on forecasters not included in pipeline: {missing_forecasters}.")
# }
# Internals for targets pipeline.
# Three lists which are used at runtime and must maintain the same order.
# - forecaster_functions_list: list of forecaster base functions (e.g. scaled_pop)
# - forecaster_names_list: list of forecaster names (e.g. 'spiniferous.lcont')
# - params_list: list of parameters for each forecaster (a list of lists)
# The last one can also be used for parameter lookup, given a forecaster name.
config$forecaster_functions_list <- config$forecaster_parameter_combinations %>%
  map(function(x) {
    x %>% select(forecaster)
  }) %>%
  bind_rows() %>%
  pull(forecaster)
config$forecaster_names_list <- config$forecaster_parameter_combinations %>%
  map(function(x) {
    x %>% select(id)
  }) %>%
  bind_rows() %>%
  pull(id)
names(config$forecaster_functions_list) <- config$forecaster_names_list
config$params_list <- config$forecaster_parameter_combinations %>%
  map(make_params_list) %>%
  set_names(NULL) %>%
  unlist(recursive = FALSE)
names(config$params_list) <- config$forecaster_names_list
# Create a partially applied forecaster function for each id. This function
# depends on params_list and forecaster_functions_list, which are defined above.
# If we don't define these variables here, then targets won't have access at
# runtime.
get_partially_applied_forecaster <- function(id) {
  function(epi_data, ...) {
    forecaster_args <- rlang::dots_list(
      ...,
      !!!config$params_list[[id]],
      .homonyms = "last"
    )
    # This uses string lookup to get the function.
    forecaster_fn <- get(config$forecaster_functions_list[[id]])
    rlang::inject(forecaster_fn(epi_data = epi_data, !!!forecaster_args))
  }
}


# ================================ TARGETS =================================
# ================================ PARAMETERS TARGETS ======================
parameter_targets <- list2(
  tar_target(name = aheads, command = config$aheads),
  tar_target(name = forecast_dates, command = config$forecast_dates),
  # This is used for parameter lookup.
  tar_target(name = forecaster_parameter_combinations, command = config$forecaster_parameter_combinations),
  # This is used for generating notebooks.
  tar_target(name = forecaster_families, command = config$forecaster_parameter_combinations %>% names()),
)


# ================================ DATA TARGETS ==============================
data_targets <- list2(
  tar_target(
    name = hhs_archive_data_asof,
    command = {
      get_health_data(as.Date(forecast_dates), disease = "covid") %>%
        mutate(version = as.Date(forecast_dates)) %>%
        relocate(geo_value, time_value, version, hhs)
    },
    pattern = map(forecast_dates)
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
        signals = config$hhs_signal,
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = "*",
        fetch_args = config$fetch_args
      ) %>%
        select(signal, geo_value, time_value, value) %>%
        # This aggregates the data to the week and labels each Sunday - Saturday
        # summation with the Wednesday of that week.
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
        fetch_args = config$fetch_args
      )
      nssp_hhs <- retry_fn(
        max_attempts = 10,
        wait_seconds = 1,
        fn = pub_covidcast,
        source = "nssp",
        signal = "pct_ed_visits_covid",
        time_type = "week",
        geo_type = "hhs",
        geo_values = "*",
        fetch_args = config$fetch_args
      )
      nssp_state %>%
        bind_rows(nssp_hhs) %>%
        select(geo_value, time_value, issue, nssp = value) %>%
        as_epi_archive(compactify = TRUE) %>%
        `$`("DT") %>%
        # weekly data is indexed from the start of the week
        mutate(time_value = time_value + 6 - config$time_value_adjust) %>%
        mutate(version = time_value) %>%
        # Always convert to data.frame after dplyr operations on data.table.
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(compactify = TRUE)
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
          signals = glue::glue("s0{search_name}_smoothed_search"),
          time_type = "day",
          geo_type = "state",
          geo_values = "*",
          fetch_args = config$fetch_args
        )
        google_symptoms_hhs_archive <- retry_fn(
          max_attempts = 10,
          wait_seconds = 1,
          fn = pub_covidcast,
          source = "google-symptoms",
          signals = glue::glue("s0{search_name}_smoothed_search"),
          time_type = "day",
          geo_type = "hhs",
          geo_values = "*",
          fetch_args = config$fetch_args
        )
        google_symptoms_archive_min <- google_symptoms_state_archive %>%
          bind_rows(google_symptoms_hhs_archive) %>%
          select(geo_value, time_value, value) %>%
          daily_to_weekly() %>%
          mutate(version = time_value) %>%
          filter(!is.na(value)) %>%
          relocate(geo_value, time_value, version, value) %>%
          # Always convert to data.frame after dplyr operations on data.table.
          # https://github.com/cmu-delphi/epiprocess/issues/618
          as.data.frame() %>%
          as_epi_archive(compactify = TRUE)
      })
      all_of_them[[1]] <- all_of_them[[1]]$DT %>%
        rename(google_symptoms_4_bronchitis = value) %>%
        # Always convert to data.frame after dplyr operations on data.table.
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(compactify = TRUE)
      all_of_them[[2]] <- all_of_them[[2]]$DT %>%
        rename(google_symptoms_5_ageusia = value) %>%
        # Always convert to data.frame after dplyr operations on data.table.
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(compactify = TRUE)
      google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]])
      pre_pipeline <- google_symptoms_archive %>%
        epix_as_of(as.Date("2023-10-04")) %>%
        mutate(source = "none")
      # Google Symptoms has two signals that have different and unknown scales,
      # so we need to whiten them.
      colnames <- c("google_symptoms_4_bronchitis", "google_symptoms_5_ageusia")
      for (colname in colnames) {
        learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
        google_symptoms_archive$DT %<>% data_whitening(colname = colname, learned_params, join_cols = "geo_value")
      }
      # Sum the two signals.
      google_symptoms_archive$DT %>%
        mutate(
          google_symptoms = ifelse(is.na(google_symptoms_4_bronchitis), 0, google_symptoms_4_bronchitis) +
            ifelse(is.na(google_symptoms_5_ageusia), 0, google_symptoms_5_ageusia)
        ) %>%
        select(-starts_with("source")) %>%
        # Always convert to data.frame after dplyr operations on data.table
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(compactify = TRUE)
      }
    ),
  tar_target(
    name = state_to_hhs_crosswalk,
    command = get_state_codes_crosswalk() %>% select(hhs_region = hhs, geo_value = state_id)
  ),
  # TODO: Might be able to simplify this with some utilities:
  # - add_geo_column, aggregate_to_hhs_region, etc.
  tar_target(
    name = nwss_coarse,
    command = {
      nwss <- get_nwss_coarse_data("covid") %>%
        rename(value = state_med_conc) %>%
        arrange(geo_value, time_value) %>%
        add_pop_and_density() %>%
        drop_na() %>%
        select(-agg_level, -year, -agg_level, -population, -density)
      pop_data <- gen_pop_and_density_data()
      cw <- readr::read_csv(
        "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_codes_table.csv",
        show_col_types = FALSE,
        progress = FALSE
      ) %>%
        left_join(
          readr::read_csv(
            "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_code_hhs_table.csv",
            show_col_types = FALSE,
            progress = FALSE
          ),
          by = join_by(state_code == state_code)
        ) %>%
        mutate(hhs = as.character(hhs)) %>%
        select(geo_value = state_id, hhs_region = hhs)
      nwss_hhs_region <- nwss %>%
        left_join(cw, by = "geo_value") %>%
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
        mutate(time_value = time_value - config$time_value_adjust, version = time_value) %>%
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
        # Always convert to data.frame after dplyr operations on data.table
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(compactify = TRUE)
      joined_archive_data$geo_type <- "custom"
      joined_archive_data <- joined_archive_data %>% epix_merge(nwss_coarse, sync = "locf")
      joined_archive_data$geo_type <- "custom"
      joined_archive_data %<>% epix_merge(nssp_archive, sync = "locf")
      joined_archive_data$geo_type <- "custom"
      joined_archive_data %<>% epix_merge(google_symptoms_archive, sync = "locf")
      joined_archive_data <- joined_archive_data$DT %>%
        filter(grepl("[a-z]{2}", geo_value), !(geo_value %in% config$insufficient_data_geos)) %>%
        # Always convert to data.frame after dplyr operations on data.table
        # https://github.com/cmu-delphi/epiprocess/issues/618
        as.data.frame() %>%
        as_epi_archive(compactify = TRUE)
      joined_archive_data$geo_type <- "state"
      # TODO: This is a hack to ensure the as_of data is cached. Maybe there's a better way.
      epix_slide_simple(joined_archive_data, dummy_forecaster, forecast_dates, cache_key = "joined_archive_data")
      joined_archive_data
    }
  ),
  tar_target(
    validate_joined_archive_data,
    command = {
      # TODO: This can be a bit more granular (per geo, per source, etc.)
      min_time_value <- joined_archive_data$DT %>%
        filter(if_all(all_of(c("hhs")), ~ !is.na(.))) %>%
        distinct(time_value) %>%
        pull(time_value) %>%
        min()
      if (min_time_value > (forecast_dates[1] - 30)) {
        stop(
          "Joined archive data does not have at least 30 days of training data for the earliest forecast date.
             Update your forecast_dates to be later than ",
          min_time_value + 30,
          "."
        )
      }
    }
  )
)

forecasts_and_scores <- tar_map(
  values = list(forecaster_id = config$forecaster_names_list),
  unlist = FALSE,
  tar_target(
    name = forecast,
    command = {
      forecaster_fn <- function(epi_data)
        get_partially_applied_forecaster(forecaster_id)(epi_data = epi_data, ahead = aheads)
      # debugonce(scaled_pop)
      # debugonce(run_workflow_and_format)
      # browser()
      out <- epix_slide_simple(
        joined_archive_data,
        forecaster_fn,
        forecast_dates,
        cache_key = "joined_archive_data"
      ) %>%
        rename(prediction = value) %>%
        mutate(ahead = as.numeric(target_end_date - forecast_date)) %>%
        mutate(id = forecaster_id)
      # browser()
      out
    },
    pattern = map(aheads)
  ),
  tar_target(
    name = score,
    command = {
      forecasts <- forecast %>%
        # Push the Wednesday markers to Saturday, to match targets with truth data.
        mutate(forecast_date = forecast_date + 3, target_end_date = target_end_date + 3) %>%
        rename("model" = "id")
      # browser()
      evaluate_predictions(forecasts = forecasts, truth_data = hhs_evaluation_data) %>%
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
        # Push the Wednesday markers to Saturday, to match targets with truth data.
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
    command = s3read_using(
      nanoparquet::read_parquet,
      object = "covid19_forecast_hub_2023_full_summed.parquet",
      bucket = "forecasting-team-data"
    )
  ),
  tar_target(
    external_forecasts,
    command = {
      external_forecasts_file %>%
        filter(geo_value %in% state_geo_values, forecaster %in% outside_forecaster_subset) %>%
        rename(ahead = week_ahead, prediction = value) %>%
        mutate(target_end_date = as.Date(forecast_date) + 7 * as.numeric(ahead)) %>%
        mutate(forecast_date = forecast_date + 5, target_end_date = target_end_date + 5) %>%
        filter(forecast_date %in% (forecast_dates + 3)) %>%
        mutate(prediction = prediction * 7)
    }
  ),
  tar_target(
    external_scores,
    command = {
      evaluate_predictions(
        forecasts = external_forecasts %>% rename(model = forecaster),
        truth_data = hhs_evaluation_data
      ) %>%
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
      forecaster_family_subset <- forecaster_parameter_combinations[[forecaster_families]]$id

      filtered_forecasts <- joined_forecasts %>%
        filter(forecaster %in% c(forecaster_family_subset, outside_forecaster_subset))
      filtered_scores <- joined_scores %>%
        filter(forecaster %in% c(forecaster_family_subset, outside_forecaster_subset))

      rmarkdown::render(
        "scripts/reports/comparison-notebook.Rmd",
        params = list(
          forecaster_parameters = forecaster_parameter_combinations[[forecaster_families]],
          forecaster_family = forecaster_families,
          forecasts = filtered_forecasts,
          scores = filtered_scores,
          truth_data = hhs_evaluation_data,
          disease = "covid"
        ),
        output_file = here::here(config$reports_dir, paste0("covid-notebook-", forecaster_families, ".html"))
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
