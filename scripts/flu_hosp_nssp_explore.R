eval_time_weeks <- c(paste0("2023", stringr::str_pad(40:52, 2, pad = "0")), paste0("2024", stringr::str_pad(1:17, 2, pad = "0")))
eval_dates <- seq.Date(as.Date("2023-10-04"), as.Date("2024-04-24"), by = 7)

eval_dates
## testing garbage
flusion_merged <- qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
  filter(geo_value != "as") %>%
  as_epi_archive(other_keys = "source", compactify = TRUE)
flusion_merged$DT %>%
  filter(source == "nhsn", time_value %in% eval_dates) %>%
  as_epi_archive(other_keys = "source", compactify = TRUE)
flusion_merged$DT %>%
  filter(source == "nhsn", season == "2023/24", !is.na(value)) %>%
  arrange(time_value) %>%
  as_epi_archive(other_keys = "source", compactify = TRUE)
tmp %>%
  as_epi_archive(compactify = TRUE) %>%
  epix_as_of(as.Date("2024-09-29")) %>%
  ggplot(aes(x = time_value, y = value, color = geo_value)) +
  geom_line()
# end testing garbage
list(
  tar_target(
    name = flusion_data,
    command = {
      flusion_data <- qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
        filter(
          geo_value != "as",
          time_value <= max(eval_dates)
        ) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      flusion_data
    }
  ),
  tar_target(
    name = hhs_evaluation_data,
    command = {
      new_flu_data <- flusion_data$DT %>%
        filter(
          source == "nhsn",
          agg_level %in% c("state", "nation"),
          time_value %in% eval_dates
        ) %>%
        as_epi_archive(compactify = TRUE)
      new_flu_data %>%
        epix_as_of(new_flu_data$versions_end) %>%
        rename(
          true_value = value,
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
  ),
  tar_target(
    name = nssp_archive_data,
    command = {
      nssp_rec <- epidatr::pub_covidcast(
        source = "nssp",
        signals = "pct_ed_visits_influenza",
        geo_type = "state",
        time_type = "week",
        geo_values = "*",
        time_values = "*",
        issues = "*",
        fetch_args = fetch_args
      )
      nssp_rec_hhs <- epidatr::pub_covidcast(
        source = "nssp",
        signals = "pct_ed_visits_influenza",
        geo_type = "hhs",
        time_type = "week",
        geo_values = "*",
        time_values = "*",
        issues = "*",
        fetch_args = fetch_args
      ) %>%
        mutate(geo_type = "hhs_region")
      nssp_archive_data <- nssp_rec %>%
        bind_rows(nssp_rec_hhs) %>%
        select(
          geo_value,
          time_value,
          value,
          version = issue,
          agg_level = geo_type
        ) %>%
        mutate(time_value = time_value + 3) %>%
        as_epi_archive(compactify = TRUE)
      nssp_archive_data
    }
  ),
  tar_target(
    name = joined_archive_data,
    command = {
      flusion_data <- flusion_data$DT %>%
        rename("flusion" := value) %>%
        filter(
          !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
          !is.na(flusion)
        ) %>%
        as_epi_archive(compactify = TRUE, other_keys = c("source", "agg_level"))
      needed_keys <- flusion_data$DT %>%
        select(geo_value, time_value, source, agg_level) %>%
        distinct() %>%
        filter(time_value >= min(nssp_archive_data$DT$time_value))
      needed_keys

      # nssp doesn't have a source column, so it may be present across multiple
      # sources this adds just enough keys so that the values are only
      # replicated for sources which are present on that day
      nssp_joint <-
        nssp_archive_data$DT %>%
        rename("nssp_flu" := value) %>%
        filter(
          !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
          !is.na(nssp_flu)
        ) %>%
        full_join(needed_keys,
          by = join_by(geo_value, time_value, agg_level),
          relationship = "many-to-many"
        ) %>%
        filter(!is.na(source)) %>% # these are timevalues that don't exist in the flusion data
        arrange(geo_value, time_value, version, desc(source)) %>%
        group_by(geo_value, time_value, version) %>%
        fill(nssp_flu, version) %>%
        filter(!is.na(nssp_flu)) %>%
        as_epi_archive(compactify = TRUE, other_keys = c("source", "agg_level"))

      nssp_archive_data$DT %>% filter(time_value == "2024-04-10", geo_value == "wy")
      as_epi_archive(compactify = TRUE, other_keys = c("source", "agg_level")) %>%
        epix_merge(flusion_data, sync = "locf")
      full_join(needed_sources, by = join_by(geo_value, time_value)) %>%
        arrange(geo_value, time_value, version) %>%
        fill(nssp_flu)
      expand_grid(source = c("ILI+", "nhsn", "flusurv")) %>%
        as_epi_archive(compactify = TRUE, other_keys = "source")
      nssp_archive_data$geo_type <- "custom"
      joint <- epix_merge(flusion_data, nssp_archive_data, sync = "locf")
      joint$DT %>% filter(!is.na(nssp_flu))
      flusion_data$DT %>% filter(time_value == "2022-09-28", geo_value == "ak")
      nssp_archive_data$DT %>% filter(time_value == "2022-09-28", geo_value == "ak")
    }
  )
)


source("scripts/targets-common.R")
source("scripts/targets-exploration-common.R")

# Debug mode will replace all forecasters with a fast dummy forecaster. Helps
# with prototyping the pipeline.
debug <- as.logical(Sys.getenv("DEBUG_MODE", TRUE))
# TODO: Need DUMMY_MODE

# Human-readable object to be used for inspecting the forecasters in the pipeline.
forecaster_parameter_combinations_ <- list(
  tidyr::expand_grid(
    forecaster = "scaled_pop",
    trainer = c("linreg", "quantreg"),
    lags = list(c(0, 3, 5, 7, 14), c(0, 7, 14), c(0, 7, 14, 24)),
    pop_scaling = c(TRUE, FALSE)
  ),
  tidyr::expand_grid(
    forecaster = "smoothed_scaled",
    trainer = c("quantreg"),
    lags = list(
      # list(smoothed, sd)
      list(c(0, 3, 5, 7, 14), c(0)),
      list(c(0, 7, 14, 21, 28), c(0)),
      list(c(0, 2, 4, 7, 14, 21, 28), c(0))
    ),
    pop_scaling = c(TRUE, FALSE)
  ),
  tidyr::expand_grid(
    forecaster = "flatline_fc",
  ),
  tidyr::expand_grid(
    forecaster = "flusion",
    lags = list(
      list(c(0, 1, 3))
    )
  ),
) %>%
  map(function(x) {
    if (debug) {
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

scaled_pop_not_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "linreg",
  pop_scaling = FALSE,
  lags = list(c(0, 3, 5, 7, 14))
)
scaled_pop_scaled <- list(
  forecaster = "scaled_pop",
  trainer = "linreg",
  pop_scaling = FALSE,
  lags = list(c(0, 3, 5, 7, 14))
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
    scaled_pop_not_scaled
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
    if (debug) {
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
data_targets <- make_data_targets()


# These globals are needed by the function below (and they need to persist
# during the actual targets run, since the commands are frozen as expressions).
date_step <- 7L
forecasts_and_scores <- make_forecasts_and_scores()

ensembles_and_scores <- make_ensembles_and_scores()
external_names_and_scores <- make_external_names_and_scores()


rlang::list2(
  list(
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
      c(-1, 0, 1, 2, 3)
    }
  ),
  data_targets,
  forecasts_and_scores,
  ensembles_and_scores,
  external_names_and_scores
)
