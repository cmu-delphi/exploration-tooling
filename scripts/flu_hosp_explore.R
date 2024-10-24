eval_dates <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7)
source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# Debug mode will replace all forecasters with a fast dummy forecaster. Helps
# with prototyping the pipeline.
dummy_mode <- as.logical(Sys.getenv("DUMMY_MODE", FALSE))

# these are locations we shouldn't take into account when deciding on latency,
# since e.g. flusurv stopped updating, and the various geos stopped updating for
# ILI+
very_latent_locations <- list(list(
  geo_value = c("dc", "nh", "nv", "de", "ak", "me", "nd", "ut", "wy", "nc", "id"),
  source = c("flusurv", "ILI+")
))

# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations_ <- rlang::list2(
  # just the data, possibly population scaled; likely to run into troubles
  # because of the scales of the different sources
  tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("quantreg", "randforest_grf"),
    lags = list(c(0, 7, 14, 21)),
    pop_scaling = c(TRUE, FALSE),
    filter_source = c("", "nhsn"),
    filter_agg_level = c("", "state"),
    keys_to_ignore = very_latent_locations
  ),
  # The covid forecaster, ported over to flu. Also likely to struggle with the
  # extra data
  # tidyr::expand_grid(
  #   forecaster = "smoothed_scaled",
  #   trainer = c("quantreg", "randforest_grf"),
  #   lags = list(
  #     # list(smoothed, sd)
  #     list(c(0, 7, 14, 21, 28), c(0))
  #   ),
  #   smooth_width = as.difftime(2, units = "weeks"),
  #   sd_width = as.difftime(4, units = "weeks"),
  #   sd_mean_width = as.difftime(2, units = "weeks"),
  #   pop_scaling = c(TRUE, FALSE),
  #   filter_source = c("", "nhsn"),
  #   filter_agg_level = c("", "state"),
  #   keys_to_ignore = very_latent_locations
  # ),
  # # the thing to beat (a simplistic baseline forecast)
  # tidyr::expand_grid(
  #   forecaster = "flatline_fc",
  # ),
  # tidyr::expand_grid(
  #   forecaster = "flusion",
  #   lags = list(c(0, 7, 21)),
  #   dummy_states = FALSE,
  #   dummy_source = c(TRUE, FALSE),
  #   nonlin_method = c("quart_root", "none"),
  #   derivative_estimator = c("growth_rate", "none"),
  #   keys_to_ignore = very_latent_locations
  # ),
  # # another kind of baseline forecaster
  # tidyr::expand_grid(
  #   forecaster = "no_recent_outcome",
  #   trainer = c("quantreg", "randforest_grf"),
  #   scale_method = c("quantile", "none"),
  #   nonlin_method = c("quart_root", "none"),
  #   filter_source = c("", "nhsn"),
  #   use_population = c(FALSE, TRUE),
  #   use_density = c(FALSE, TRUE),
  #   week_method = c("linear", "sine"),
  #   keys_to_ignore = very_latent_locations
  # )
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
# Human-readable object to be used for inspecting the ensembles in the pipeline.
ensemble_parameter_combinations_ <- tribble(
  ~ensemble, ~ensemble_args, ~forecasters,
  # mean forecaster
  "ensemble_average",
  list(average_type = "mean"),
  list(
    no_recent_outcome_params,
    list(forecaster = "flatline_fc")
  ),
  # median forecaster
  "ensemble_average",
  list(average_type = "median"),
  list(
    no_recent_outcome_params,
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
ensemble_parameter_combinations_ <- tibble::tibble(children_ids = character())
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

# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
hhs_signal <- "confirmed_admissions_influenza_1d_prop_7dav"
chng_signal <- "smoothed_adj_outpatient_flu"
eval_time <- epidatr::epirange(from = "2020-01-01", to = "2024-01-01")
training_time <- epidatr::epirange(from = "2021-01-01", to = "2023-06-01")
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
data_targets <- list(
  tar_target(
    name = joined_archive_data,
    command = {
      joined_archive_data <- qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
        filter(
          !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
          !is.na(value),
          time_value <= max(eval_dates)
        ) %>%
        rename(hhs = value) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      joined_archive_data
    }
  ),
  tar_target(
    name = hhs_evaluation_data,
    command = {
      new_flu_data <- joined_archive_data$DT %>%
        filter(
          source == "nhsn",
          agg_level %in% c("state", "nation"),
          time_value %in% eval_dates
        ) %>%
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
          true_value
        )
    }
  )
)



# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
if (!exists("ref_time_values")) {
  start_date <- as.Date("2023-10-04")
  end_date <- as.Date("2024-04-24")
  ref_time_values <- NULL
  date_step <- 7L
}
forecasts_and_scores <- make_forecasts_and_scores()

# ensembles_and_scores <- make_ensembles_and_scores()

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
      c(-7, 0, 7, 14, 21)
    }
  ),
  data_targets,
  forecasts_and_scores,
  # ensembles_and_scores
)
