source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# These globals are needed by make_forecasts_and_scores (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
if (!exists("ref_time_values_")) {
  ref_time_values_ <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7L)
  # Alternatively you can let slide_forecaster figure out ref_time_values
  start_date <- as.Date("2023-10-04")
  end_date <- as.Date("2024-04-24")
  date_step <- 7L
  # ref_time_values_ <- NULL
}

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
  scaled_pop_main = bind_rows(
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      lags = list2(
        c(0, 7),
        c(0, 7, 14, 21),
      ),
      pop_scaling = FALSE,
      filter_source = "nhsn",
      filter_agg_level = "state",
      scale_method = "none",
      center_method = "median",
      nonlin_method = c("quart_root", "none"),
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = very_latent_locations
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      lags = list2(
        c(0, 7),
        c(0, 7, 14, 21),
      ),
      pop_scaling = FALSE,
      filter_source = "nhsn",
      filter_agg_level = "state",
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = very_latent_locations
    )
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
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
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
      filter_source = "nhsn",
      filter_agg_level = "state",
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
      filter_source = "nhsn",
      filter_agg_level = "state",
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
          c(0, 7), # nwss
          c(0, 7), # nwss_region
        )
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = "quart_root",
      filter_source = "nhsn",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = very_latent_locations,
    )
  ),
  ## # another kind of baseline forecaster
  ## no_recent_quant = tidyr::expand_grid(
  ##   forecaster = "no_recent_outcome",
  ##   trainer = "quantreg",
  ##   scale_method = "quantile",
  ##   nonlin_method = "quart_root",
  ##   filter_source = "",
  ##   use_population = c(TRUE, FALSE),
  ##   use_density = c(TRUE, FALSE),
  ##   week_method = "sine",
  ##   keys_to_ignore = very_latent_locations
  ## ),
  no_recent_but_exogenous = bind_rows(
    expand_grid(
      forecaster = "no_recent_outcome",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
      lags = list2(
        list2(
          # no hhs
          c(0, 7) # exogenous feature
        )
      ),
      scale_method = "quantile",
      nonlin_method = "quart_root",
      filter_source = c("", "nhsn"),
      use_population = TRUE,
      use_density = FALSE,
      week_method = "sine",
      n_training = Inf,
      keys_to_ignore = very_latent_locations
    ),
    expand_grid(
      forecaster = "no_recent_outcome",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms"),
        c("nssp", "nwss"),
        c("nssp", "nwss_region"),
        c("google_symptoms", "nwss"),
        c("google_symptoms", "nwss_region"),
        c("nwss", "nwss_region")
      ),
      lags = list2(
        list2(
          # no hhs
          c(0, 7), # first feature
          c(0, 7) # second feature
        )
      ),
      scale_method = "quantile",
      nonlin_method = "quart_root",
      filter_source = c("", "nhsn"),
      use_population = TRUE,
      use_density = FALSE,
      week_method = "sine",
      n_training = Inf,
      keys_to_ignore = very_latent_locations
    ),
    expand_grid(
      forecaster = "no_recent_outcome",
      trainer = "quantreg",
      extra_sources = list2(
        c("nssp", "google_symptoms", "nwss", "nwss_region"),
      ),
      lags = list2(
        list2(
          # no hhs
          c(0, 7), # nssp
          c(0, 7), # google symptoms
          c(0, 7), # nwss
          c(0, 7) # nwss_region
        )
      ),
      scale_method = "quantile",
      nonlin_method = "quart_root",
      filter_source = c("", "nhsn"),
      use_population = TRUE,
      use_density = FALSE,
      week_method = "sine",
      n_training = Inf,
      keys_to_ignore = very_latent_locations
    )
  ),
  scaled_pop_season = bind_rows(
    tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7, 14, 21),
        c(0, 7)
      ),
      seasonal_method = list("flu", "indicator", "climatological"),
      pop_scaling = FALSE,
      train_residual = c(TRUE, FALSE),
      filter_source = "nhsn",
      filter_agg_level = "state",
      drop_non_seasons = c(TRUE, FALSE),
      n_training = Inf,
      season_backward_window = 5,
      keys_to_ignore = very_latent_locations
    ),
    # Window-based seasonal method shouldn't drop non-seasons
    tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7)
      ),
      seasonal_method = list("window", c("window", "flu"), c("window", "climatological")),
      pop_scaling = FALSE,
      train_residual = c(FALSE, TRUE),
      filter_source = c("", "nhsn"),
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      season_backward_window = 5,
      keys_to_ignore = very_latent_locations
    ),
    tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7, 14, 21)
      ),
      seasonal_method = list("window", c("window", "flu"), c("window", "climatological")),
      pop_scaling = FALSE,
      train_residual = c(FALSE, TRUE),
      filter_source = c("", "nhsn"),
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      season_backward_window = 8,
      keys_to_ignore = very_latent_locations
    )
    # trying various window sizes
  ),
  scaled_pop_season_exogenous = bind_rows(
    expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      # since it's a list, this gets expanded out to a single one in each row
      extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"), # removing google_symptoms for lack of data for now
      lags = list2(
        list2(
          c(0, 7), # hhs
          c(0, 7) # exogenous feature
        )
      ),
      seasonal_method = "window",
      pop_scaling = FALSE,
      filter_source = "",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = FALSE,
      keys_to_ignore = very_latent_locations
    ),
    expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      extra_sources = list2(
        #c("nssp", "google_symptoms"),
        #c("nssp", "nwss"),
        c("nssp", "nwss_region"),
        c("google_symptoms", "nwss"),
        #c("google_symptoms", "nwss_region"),
        c("nwss", "nwss_region")
      ),
      lags = list2(
        list2(
          c(0, 7, 14, 21), # hhs
          c(0, 7), # first feature
          c(0, 7) # second feature
        )
      ),
      seasonal_method = "window",
      pop_scaling = FALSE,
      filter_source = "",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = FALSE,
      keys_to_ignore = very_latent_locations
    ),
    expand_grid(
      forecaster = "scaled_pop_seasonal",
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
        )
      ),
      seasonal_method = "window",
      pop_scaling = FALSE,
      filter_source = "",
      filter_agg_level = "state",
      n_training = Inf,
      drop_non_seasons = FALSE,
      keys_to_ignore = very_latent_locations
    )
  ),
  season_window_sizes = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7)
      ),
      seasonal_method = list("window"),
      pop_scaling = FALSE,
      train_residual = FALSE,
      filter_source = "",
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      season_backward_window = c(3,5,7,9,52),
      season_forward_window = c(3,5,7),
      keys_to_ignore = very_latent_locations
    ),
  climate_linear = expand_grid(
    forecaster = "climate_linear_ensembled",
    scale_method = c("quantile", "none"),
    center_method = "median",
    nonlin_method = "quart_root",
    filter_source = c("", "nhsn"),
    filter_agg_level = "state",
    drop_non_seasons = c(FALSE),
    aheads = list(c(0, 7, 14, 21))
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


# data is sufficiently different that it needs to be run separately
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
forecaster_families_ <- setdiff(forecaster_parameter_combinations_ %>% names(), c("flusion_grf"))
reports_dir <- "reports"


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
        get_health_data(as.Date(ref_time_values), disease = "flu") %>%
          mutate(version = as.Date(ref_time_values)) %>%
          relocate(geo_value, time_value, version, hhs)
      },
      pattern = map(ref_time_values)
    ),
    tar_target(
      name = hhs_archive,
      command = {
        hhs_archive_data <- hhs_archive_data_asof %>%
          as_epi_archive(compactify = TRUE) %>%
          daily_to_weekly_archive(agg_columns = "hhs")
        hhs_archive_data$DT %>%
          mutate(epiyear = epiyear(time_value), epiweek = epiweek(time_value)) %>%
          left_join(
            (.) %>%
              distinct(epiyear, epiweek) %>%
              mutate(season = convert_epiweek_to_season(epiyear, epiweek)) %>%
              mutate(season_week = convert_epiweek_to_season_week(epiyear, epiweek))
          ) %>%
          mutate(agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse("us" == geo_value, "nation", "state"))) %>%
          add_pop_and_density() %>%
          mutate(hhs = hhs / population * 10L^5) %>%
          mutate(source = "nhsn") %>%
          mutate(agg_level = ifelse(geo_value == "us", "nation", "state")) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE) %>%
          `$`("DT") %>%
          select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
      }
    ),
    tar_target(
      name = flusurv,
      command = {
        flusurv_adjusted <- generate_flusurv_adjusted()
        flusurv_adjusted$DT %>%
          mutate(time_value = time_value + 3, version = version + 3) %>%
          mutate(source = "flusurv") %>%
          select(geo_value, time_value, version, hhs = adj_hosp_rate, source, agg_level, season, season_week)
      }
    ),
    tar_target(
      name = ili_plus,
      command = {
        ili_plus <- gen_ili_data()
        ili_plus <- ili_plus$DT %>%
          drop_na() %>%
          filter(hhs > 0.0001) %>%
          mutate(time_value = time_value + 3, version = time_value) %>%
          select(geo_value, time_value, version, hhs, source, agg_level, season, season_week)
        to_keep <-
          ili_plus %>%
          group_by(geo_value, season) %>%
          summarize(total_count = length(hhs), .groups = "drop") %>%
          filter(total_count >= 20) %>%
          select(geo_value, season)
        to_keep %>%
          left_join(
            ili_plus,
            by = join_by(geo_value, season),
            relationship = "many-to-many"
          ) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE) %>%
          `$`("DT")
      }
    ),
    tar_target(
      name = flusion_data_archive,
      command = {
        flusion_data_archive <-
          bind_rows(ili_plus, flusurv, hhs_archive) %>%
          add_pop_and_density() %>%
          as_epi_archive(compactify = TRUE, other_keys = "source")
        max_date <- min(max(ili_plus$time_value), max(flusurv$time_value), max(hhs_archive$time_value))
        flusion_data_archive <- flusion_data_archive$DT %>%
          filter(
            !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
            !is.na(hhs),
            time_value <= end_date
          ) %>%
          relocate(source, geo_value, time_value, version, hhs, agg_level, season, season_week, year, population, density) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)
        flusion_data_archive
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
        pre_pipeline <- google_symptoms_archive %>%
          epix_as_of(as.Date("2023-10-04"))
        colnames <- c("google_symptoms_1_cough", "google_symptoms_3_fever", "google_symptoms_4_bronchitis", "google_symptoms")
        for (colname in colnames) {
          learned_params <- calculate_whitening_params(pre_pipeline, colname = colname)
          google_symptoms_archive$DT %<>% data_whitening(colname = colname, learned_params, join_cols = c("geo_value", "source"))
        }
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
          mutate(agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse("us" == geo_value, "nation", "state"))) %>%
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
            time_value >= as.Date("2023-10-04")
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
          start_date = as.Date("2023-10-04"),
          end_date = as.Date("2024-04-24"),
          date_range_step_size = 7L,
          cache_key = "joined_archive_data"
        )
        joined_archive_data
      }
    )
  ),
  make_forecasts_and_scores(),
  # TODO: Score here also.
  # make_ensembles_and_scores()
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
        select(-population) %>%
        mutate(target_end_date = target_end_date + 3)
      cmu_forecast_dates <- ref_time_values_ + 3
      filtered_forecasts <- external_forecasts %>%
        filter(forecast_date %in% cmu_forecast_dates) %>%
        rename(model = forecaster)
      evaluate_predictions(predictions_cards = filtered_forecasts, truth_data = actual_eval_data) %>%
        rename(forecaster = model)
    }
  ),
  tar_target(
    rescaled_delphi_forecasts,
    command = {
      delphi_forecasts %>%
        mutate(forecast_date = ceiling_date(forecast_date, "weeks", week_start = 6)) %>%
        left_join(
          hhs_evaluation_data %>% distinct(geo_value, population),
          by = "geo_value"
        ) %>%
        mutate(prediction = prediction * population / 10L**5) %>%
        select(-population)
    }
  ),
  tar_target(
    joined_forecasts,
    command = {
      rescaled_delphi_forecasts %>% bind_rows(external_forecasts)
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
      outside_forecaster_subset <- c("FluSight-baseline", "FluSight-ensemble", "UMass-flusion")
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
          disease = "flu"
        ),
        output_file = here::here(reports_dir, paste0("flu-notebook-", forecaster_families, ".html"))
      )
    },
    pattern = map(forecaster_families)
  ),
  tar_target(
    overall_notebook,
    command = {
      actual_eval_data <- hhs_evaluation_data %>%
        select(-population) %>%
        mutate(target_end_date = target_end_date + 3)
      rmarkdown::render(
        "scripts/reports/flu-overall-comparison-notebook.Rmd",
        params = list(
          forecaster_parameters = forecaster_parameter_combinations,
          forecasts = joined_forecasts,
          scores = joined_scores,
          truth_data = actual_eval_data,
          disease = "flu"
        ),
        output_file = here::here(reports_dir, "flu-overall-notebook.html")
      )
    }
  ),
  tar_target(
    new_data_notebook,
    command = {
      rmarkdown::render("scripts/reports/new_data.Rmd", output_file = here::here("reports", "new_data.html"))
    }
  )
)
