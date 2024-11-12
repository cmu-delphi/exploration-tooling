eval_dates <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7)
source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# Debug mode will replace all forecasters with a fast dummy forecaster. Helps
# with prototyping the pipeline.
dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))

# these are locations we shouldn't take into account when deciding on latency,
# since e.g. flusurv stopped updating, and the various geos stopped updating for
# ILI+
# note that this has a vector of names first, and then the list of things to exclude, because targets hates named lists and strips their names
very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))

# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations_ <- rlang::list2(
  # just the data, possibly population scaled; likely to run into troubles
  # because of the scales of the different sources
  scaled_pop_main = tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = "quantreg",
    lags = list2(
      c(0, 7),
      c(0, 7, 14, 21),
    ),
    pop_scaling = FALSE,
    filter_source = "nhsn",
    filter_agg_level = "state",
    n_training = Inf,
    drop_non_seasons = TRUE,
    keys_to_ignore = very_latent_locations
  ),
  scaled_pop_data_augmented = tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = "quantreg",
    lags = list(
      c(0, 7, 14, 21),
      c(0, 7)
    ),
    pop_scaling = FALSE,
    scale_method = "quantile",
    center_method = "median",
    nonlin_method = c("quart_root", "none"),
    filter_source = "",
    filter_agg_level = "",
    n_training = Inf,
    drop_non_seasons = TRUE,
    keys_to_ignore = very_latent_locations
  ),
  ## # The covid forecaster, ported over to flu. Also likely to struggle with the
  ## # extra data
  # smoothed_scaled_main = tidyr::expand_grid(
  #   forecaster = "smoothed_scaled",
  #   trainer = "quantreg",
  #   lags = list2(
  #     # list(smoothed, sd)
  #     list2(c(0, 7, 14, 21), c(0)),
  #     list2(c(0, 7), c(0))
  #   ),
  #   smooth_width = as.difftime(8, units = "weeks"),
  #   sd_width = as.difftime(12, units = "weeks"),
  #   sd_mean_width = as.difftime(8, units = "weeks"),
  #   pop_scaling = FALSE,
  #   n_training = Inf,
  #   filter_source = "nhsn",
  #   filter_agg_level = "state",
  #   keys_to_ignore = very_latent_locations
  # ),
  # smoothed_only_main = tidyr::expand_grid(
  #   forecaster = "smoothed_scaled",
  #   trainer = "quantreg",
  #   lags = list2(
  #     # list(smoothed, sd)
  #     list2(c(0, 7, 14, 21)),
  #     list2(c(0, 7))
  #   ),
  #   smooth_width = as.difftime(8, units = "weeks"),
  #   sd_width = as.difftime(as.integer(NA), units = "weeks"),
  #   sd_mean_width = as.difftime(8, units = "weeks"),
  #   pop_scaling = FALSE,
  #   n_training = Inf,
  #   filter_source = "nhsn",
  #   filter_agg_level = "state",
  #   keys_to_ignore = very_latent_locations
  # ),
  # smoothed_scaled_data_augmented = tidyr::expand_grid(
  #   forecaster = "smoothed_scaled",
  #   trainer = "quantreg",
  #   lags = list2(
  #     # list(smoothed, sd)
  #     list2(c(0, 7, 14, 21), c(0)),
  #   ),
  #   smooth_width = as.difftime(8, units = "weeks"),
  #   sd_width = as.difftime(12, units = "weeks"),
  #   sd_mean_width = as.difftime(8, units = "weeks"),
  #   pop_scaling = FALSE,
  #   n_training = Inf,
  #   scale_method = "quantile",
  #   center_method = "median",
  #   nonlin_method = "quart_root",
  #   filter_source = "",
  #   filter_agg_level = "",
  #   keys_to_ignore = very_latent_locations
  # ),
  # smoothed_only_data_augmented = tidyr::expand_grid(
  #   forecaster = "smoothed_scaled",
  #   trainer = "quantreg",
  #   lags = list2(
  #     # list(smoothed, sd)
  #     list2(c(0, 7, 14, 21)),
  #   ),
  #   smooth_width = as.difftime(8, units = "weeks"),
  #   sd_width = as.difftime(as.integer(NA), units = "weeks"),
  #   sd_mean_width = as.difftime(8, units = "weeks"),
  #   pop_scaling = FALSE,
  #   n_training = Inf,
  #   scale_method = "quantile",
  #   center_method = "median",
  #   nonlin_method = "quart_root",
  #   filter_source = "",
  #   filter_agg_level = "",
  #   keys_to_ignore = very_latent_locations
  # ),
  # the thing to beat (a simplistic baseline forecast)
  flatline = tidyr::expand_grid(
    forecaster = "flatline_fc",
    filter_source = "nhsn",
    filter_agg_level = "state"
  ),
  # using exogenous variables
  scaled_pop_exogenous = bind_rows(
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_rate", "nwss_region", "nwss_rate_region"),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7) # exogenous feature
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "",
      filter_agg_level = "",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = very_latent_locations,
    ),
    expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms"),
        c("nssp", "nwss"),
        c("nssp", "nwss_rate"),
        c("nssp", "nwss_region"),
        c("google_symptoms", "nwss"),
        c("google_symptoms", "nwss_rate"),
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
      drop_non_seasons = TRUE,
      keys_to_ignore = very_latent_locations,
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
          c(0, 7), # nwss_rate
          c(0, 7), # nwss_rate region
          c(0, 7) # hhs region
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "",
      filter_agg_level = "",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = very_latent_locations,
    )
  ),
  # flusion_quant = tidyr::expand_grid(
  #   forecaster = "flusion",
  #   trainer = "quantreg",
  #   lags = list(c(0, 7, 14)),
  #   dummy_states = FALSE,
  #   dummy_source = c(TRUE, FALSE),
  #   nonlin_method = "quart_root",
  #   derivative_estimator = "growth_rate",
  #   keys_to_ignore = very_latent_locations
  # ),
  # variations on flusion
  ## flusion_grf = tidyr::expand_grid(
  ##   forecaster = "flusion",
  ##   trainer = c("randforest_grf"),
  ##   lags = list(c(0, 7, 21)),
  ##   dummy_states = FALSE,
  ##   dummy_source = TRUE,
  ##   nonlin_method = "quart_root",
  ##   derivative_estimator = "growth_rate",
  ##   keys_to_ignore = very_latent_locations
  ## ),
  ## # another kind of baseline forecaster
  no_recent_quant = tidyr::expand_grid(
    forecaster = "no_recent_outcome",
    trainer = "quantreg",
    scale_method = "quantile",
    nonlin_method = "quart_root",
    filter_source = "",
    use_population = c(FALSE, TRUE),
    use_density = c(FALSE, TRUE),
    week_method = "sine",
    keys_to_ignore = very_latent_locations
  ),
  ## no_recent_grf = tidyr::expand_grid(
  ##   forecaster = "no_recent_outcome",
  ##   trainer = "randforest_grf",
  ##   scale_method = "quantile",
  ##   nonlin_method = "quart_root",
  ##   filter_source = "",
  ##   use_population = TRUE,
  ##   use_density = FALSE,
  ##   week_method = "linear",
  ##   keys_to_ignore = very_latent_locations
  ## ),
  scaled_pop_season = tidyr::expand_grid(
    forecaster = "scaled_pop_seasonal",
    trainer = "quantreg",
    lags = list(
      c(0, 7, 14, 21),
      c(0, 7)
    ),
    seasonal_method = c("flu", "indicator", "window"),
    pop_scaling = FALSE,
    filter_source = "nhsn",
    filter_agg_level = "state",
    drop_non_seasons = TRUE,
    n_training = Inf,
    keys_to_ignore = very_latent_locations
  ),
  scaled_pop_season_data_augmented = tidyr::expand_grid(
    forecaster = "scaled_pop_seasonal",
    trainer = "quantreg",
    lags = list(
      c(0, 7, 14, 21),
      c(0, 7)
    ),
    seasonal_method = c("flu", "indicator", "window"),
    pop_scaling = FALSE,
    scale_method = "quantile",
    center_method = "median",
    nonlin_method = "quart_root",
    filter_source = "",
    filter_agg_level = "",
    n_training = Inf,
    drop_non_seasons = TRUE,
    keys_to_ignore = very_latent_locations
  )
) %>%
  map(function(x) {
    if (dummy_mode) {
      x$forecaster <- "dummy_forecaster"
    }
    x
  }) %>%
  map(add_id)
# scale_method and filter_source being empty are exclusive
# also population and density are exclusive
forecaster_parameter_combinations_$no_recent_quant %>% filter(xor(scale_method == "none", filter_source == ""), xor(use_population, use_density))
s3save(forecaster_parameter_combinations_, object = "flu_2023_forecaster_parameter_combinations.rds", bucket = "forecasting-team-data")

forecaster_parameter_combinations_$scaled_pop_all_exogenous

# Make sure all ids are unique.
stopifnot(length(forecaster_parameter_combinations_$id %>% unique()) == length(forecaster_parameter_combinations_$id))
# Build targets-internal tibble to map over.
forecaster_grid <- forecaster_parameter_combinations_ %>%
  map(make_forecaster_grid) %>%
  bind_rows()
no_recent_outcome_params <- list(
  forecaster = "no_recent_outcome",
  trainer = "quantreg",
  scale_method = "quantile",
  nonlin_method = "quart_root",
  filter_source = "nhsn",
  use_population = TRUE,
  use_density = TRUE,
  week_method = "sine",
  keys_to_ignore = very_latent_locations[[1]]
)
# this is the 3 lag, infinite training window
best_scaled_pop <- forecaster_parameter_combinations_$scaled_pop_main %>%
  filter(id == "majestic.schnauzer") %>%
  as.list()
scaled_pop_short_window <- forecaster_parameter_combinations_$scaled_pop_main %>%
  filter(id == "sensualist.papillon") %>%
  as.list()
scaled_pop_long_window <- list(
  forecaster = "scaled_pop",
  trainer = "quantreg",
  lags = c(0, 7, 14, 21),
  pop_scaling = FALSE,
  filter_source = "nhsn",
  filter_agg_level = "state",
  n_training = Inf,
  drop_non_seasons = FALSE,
  keys_to_ignore = very_latent_locations
)
# Human-readable object to be used for inspecting the ensembles in the pipeline.
ensemble_parameter_combinations_ <- tribble(
  ~ensemble, ~ensemble_args, ~forecasters,
  # mean forecaster
  "ensemble_average",
  list(average_type = "mean"),
  list2(
    no_recent_outcome_params,
    list(forecaster = "flatline_fc")
  ),
  # median forecaster
  "ensemble_average",
  list(average_type = "median"),
  list2(
    no_recent_outcome_params,
    list(forecaster = "flatline_fc"),
  ),
  # scaled_pop averaging long and short training windows
  "ensemble_average",
  list(average_type = "median"),
  list2(
    scaled_pop_short_window,
    scaled_pop_long_window
  ),
  # ensembling the auxilary data examples with their underlying forecaster
  # insures we're forecasting something at every location, regardless of whether
  # we have exogenous data for that location
  ## "ensemble_average",
  ## list(average_type = "median"),
  ## expand_grid(list(best_scaled_pop), map(1:6, \(ii) forecaster_parameter_combinations_$scaled_pop_two_exogenous[ii,] %>% as.list)) %>% as.list %>% unname,
  ## "ensemble_average",
  ## list(average_type = "median"),
  ## expand_grid(list(best_scaled_pop), map(1:4, \(ii) forecaster_parameter_combinations_$scaled_pop_one_exogenous[ii,] %>% as.list)) %>% as.list %>% unname,
  ## "ensemble_average",
  ## list(average_type = "median"),
  ## list(best_scaled_pop,
  ##      list(forecaster_parameter_combinations_$scaled_pop_all_exogenous %>% as.list)),
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


# These globals are needed by the make_forecasts_and_scores (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
if (!exists("ref_time_values")) {
  start_date <- as.Date("2023-10-04")
  end_date <- as.Date("2024-04-24")
  ref_time_values <- NULL
  date_step <- 7L
}



# data is sufficiently different that it needs to be run separately
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
data_targets <- rlang::list2(
  tar_target(
    name = flusion_data_archive,
    command = {
      a <- 1
      flusion_data_archive <-
        qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
        filter(
          !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
          !is.na(value),
          time_value <= max(eval_dates)
        ) %>%
        rename(hhs = value) %>%
        relocate(source, geo_value, time_value, version, hhs, agg_level, season, season_week, year, population, density) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  tar_target(
    name = nssp_archive,
    command = {
      nssp_state <- pub_covidcast(
        source = "nssp",
        signal = "pct_ed_visits_influenza",
        time_type = "week",
        geo_type = "state",
        geo_values = "*"
      )
      nssp_hhs <- pub_covidcast(
        source = "nssp",
        signal = "pct_ed_visits_influenza",
        time_type = "week",
        geo_type = "hhs",
        geo_values = "*"
      )
      nssp_archive <- nssp_state %>%
        bind_rows(nssp_hhs) %>%
        select(geo_value, time_value, issue, nssp = value) %>%
        as_epi_archive(compactify = TRUE) %>%
        `$`("DT") %>%
        # End of week to midweek correction.
        mutate(time_value = time_value + 3) %>%
        # Artifically add in a one-week latency.
        mutate(version = time_value + 7) %>%
        mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
        unnest(cols = "source") %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      nssp_archive
    }
  ),
  tar_target(
    name = google_symptoms_archive,
    command = {
      used_searches <- c(1, 3, 4)
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
          mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
          unnest(cols = "source") %>%
          filter(!is.na(value)) %>%
          relocate(source, geo_value, time_value, version, value) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
      })
      all_of_them[[1]]$DT %<>% rename(google_symptoms_1_cough = value)
      all_of_them[[2]]$DT %<>% rename(google_symptoms_3_fever = value)
      all_of_them[[3]]$DT %<>% rename(google_symptoms_4_bronchitis = value)
      google_symptoms_archive <- epix_merge(all_of_them[[1]], all_of_them[[2]]) %>% epix_merge(all_of_them[[3]])
      google_symptoms_archive <- google_symptoms_archive$DT %>%
        mutate(google_symptoms = google_symptoms_1_cough + google_symptoms_3_fever + google_symptoms_4_bronchitis) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      google_symptoms_archive
    }
  ),
  tar_target(
    name = nwss_coarse,
    command = {
      files <- file.info(list.files(here::here("aux_data/nwss_flu_data/"), full.names = TRUE))
      most_recent <- rownames(files)[which.max(files$mtime)]
      nwss <- readr::read_csv(most_recent)
      state_code <- readr::read_csv(here::here("aux_data", "flusion_data", "state_codes_table.csv"), show_col_types = FALSE)
      hhs_codes <- readr::read_csv(here::here("aux_data", "flusion_data", "state_code_hhs_table.csv"), show_col_types = FALSE)
      state_to_hhs <- hhs_codes %>%
        left_join(state_code, by = "state_code") %>%
        select(hhs_region = hhs, geo_value = state_id)
      pop_data <- gen_pop_and_density_data()
      nwss %<>%
        add_pop_and_density() %>%
        mutate(
          nwss_rate = value / population * 100000,
          nwss_region_rate = region_value / population * 100000,
          nwss_national_rate = national_value / population * 100000
        ) %>%
        drop_na() %>%
        select(-agg_level, -year, -agg_level, -population, -density)
      nwss_hhs_region <-
        nwss %>%
        left_join(state_to_hhs, by = "geo_value") %>%
        mutate(year = year(time_value)) %>%
        left_join(pop_data, by = join_by(geo_value, year)) %>%
        select(-year, density) %>%
        group_by(time_value, hhs_region) %>%
        summarize(
          value = sum(value * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
          nwss_rate = sum(nwss_rate * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
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
        select(
          geo_value,
          time_value,
          nwss = value,
          nwss_rate,
          nwss_region = region_value,
          nwss_national = national_value
        ) %>%
        mutate(time_value = time_value - 3, version = time_value) %>%
        arrange(geo_value, time_value) %>%
        mutate(source = list(c("ILI+", "nhsn", "flusurv"))) %>%
        unnest(cols = "source") %>%
        as_epi_archive(other_keys = "source")
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
    name = hhs_evaluation_data,
    command = {
      new_flu_data <- flusion_data_archive$DT %>%
        filter(
          source == "nhsn",
          agg_level %in% c("state", "nation"),
          time_value %in% eval_dates
        ) %>%
        drop_na() %>%
        mutate(hhs = hhs * population / 10**5) %>%
        as_epi_archive(compactify = TRUE)
      new_flu_data %>%
        epix_as_of(new_flu_data$versions_end) %>%
        rename(
          true_value = hhs,
          target_end_date = time_value,
          signal = source
        ) %>%
        select(
          signal,
          geo_value,
          target_end_date,
          true_value,
          population
        )
    }
  ),
  tar_target(
    name = joined_archive_data,
    command = {
      joined_archive_data <-
        flusion_data_archive %>%
        epix_merge(google_symptoms_archive, sync = "locf") %>%
        epix_merge(nwss_coarse, sync = "locf") %>%
        epix_merge(nssp_archive, sync = "locf") %>%
        `$`("DT") %>%
        drop_na(agg_level) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      # Jank adding of hhs_region column to the data
      joined_archive_data <- joined_archive_data$DT %>%
        add_hhs_region_sum(hhs_region) %>%
        as_epi_archive(other_keys = "source")
      # make the data cache via the dummy forecaster not expected to be returned
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
)
# TODO missing Alaska?

forecasts_and_scores <- make_forecasts_and_scores()

ensembles_and_scores <- make_ensembles_and_scores()

# TODO external

rlang::list2(
  list2(
    tar_target(
      name = forecaster_parameter_combinations,
      command = {
        forecaster_parameter_combinations_
      },
      priority = 0.99
    ),
    tar_target(
      name = ensemble_forecasters,
      command = {
        ensemble_parameter_combinations_
      },
      priority = 0.99
    )
  ),
  tar_target(
    name = aheads,
    command = {
      c(0, 7, 14, 21)
    }
  ),
  data_targets,
  forecasts_and_scores,
  # ensembles_and_scores
)
