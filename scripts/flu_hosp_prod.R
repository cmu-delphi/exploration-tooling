# The Flu Hospitalization Production Forecasting Pipeline.
#
# Two targets projects share this script (see _targets.yaml):
#   flu_hosp_prod        - weekly production run, as_of = today
#   flu_hosp_evaluation  - historical replay over past forecast dates, scored
#                          against latest truth
# Dispatch is on the project name; each project keeps its own store, so an
# evaluation run cannot invalidate the production cache.
suppressPackageStartupMessages(source("R/load_all.R"))


# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).

# Setup targets config.
set_targets_config()
g_aheads <- -1:3
g_submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
g_insufficient_data_geos <- c("as", "mp", "vi", "gu")
g_insufficient_data_geos_nssp <- g_insufficient_data_geos
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "flu"
g_s3_prefix <- "exploration"
# Trainer used by the seasonal forecasters; stored as a global and referenced by
# symbol in the grid params so tar_map embeds the symbol, not the model_spec.
g_quantreg <- epipredict::quantile_reg()
g_external_object_name <- glue::glue("2024/2024-2025_{g_disease}_hosp_forecasts.parquet")
# needed for windowed_seasonal
g_very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))
# Date to cut the truth data off at, so we don't have too much of the past for
# plotting.
g_truth_data_date <- "2023-09-01"
# Whether we're running in evaluation (historical replay) mode. Dispatch is on
# the targets project name: scripts/run.R and the oracle pass TAR_RUN_PROJECT;
# direct tar_* invocations (e.g. make prune-flu-evaluation) set TAR_PROJECT.
# TAR_RUN_PROJECT wins so a stale TAR_PROJECT in the shell can't flip the mode.
# If TRUE, we skip the weekly report notebook (each week's report is preserved
# as an ASOF snapshot) and instead run the scoring notebook, which scores the
# historical forecasts against the truth data and compares them to the ensemble.
g_evaluation_mode <-
  Sys.getenv("TAR_RUN_PROJECT", Sys.getenv("TAR_PROJECT", "flu_hosp_prod")) == "flu_hosp_evaluation"
# The pipeline's notion of "today", read once here so a whole run can be pinned
# for reproducible oracle captures (scripts/oracle/capture.R): forecast dates
# and the "cheating" as-of slices otherwise move with the calendar. Unset ->
# Sys.Date(), i.e. current production behavior.
g_reference_date <- {
  raw <- Sys.getenv("FORECAST_REFERENCE_DATE", "")
  if (nzchar(raw)) as.Date(raw) else Sys.Date()
}
# The forecast schedule, held as one tibble so forecast_date / generation_date
# stay row-aligned by construction. Columns are consumed directly by the
# forecast/ensemble tar_maps: forecast_date_int/forecast_generation_date_int are
# Date objects; forecast_date_chr is the tar_map branch-naming key.
if (!g_evaluation_mode) {
  # generation_date is the as_of for the forecast. If run on our typical
  # schedule, it's today, a Wednesday; on a delayed forecast it's a Thursday.
  # It's used for stamping the data and picking the as_of. Usually forecast_date
  # equals it, but forecast_date can be overridden and should be a Wednesday.
  gen_dates <- g_reference_date
  fc_dates <- round_date(gen_dates, "weeks", week_start = 3)
  # the forecast is actually for the wednesday beforehand for these days
  if (gen_dates %in% as.Date(c("2025-12-29"))) {
    fc_dates <- as.Date("2025-12-24")
  }
} else {
  # Pin FORECAST_REFERENCE_DATE for a reproducible evaluation window; must be
  # >= 2025-12-31 so the trailing seq.Date() stays non-empty.
  gen_dates <- c(
    as.Date(c("2024-11-21", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
    seq.Date(as.Date("2025-01-08"), as.Date("2025-12-17"), by = 7L),
    as.Date(c("2025-12-29")),
    seq.Date(as.Date("2025-12-31"), g_reference_date, by = 7L)
  )
  # Every Wednesday since mid-Nov 2024
  fc_dates <- seq.Date(as.Date("2024-11-20"), g_reference_date, by = 7L)
}
g_forecast_schedule <- tibble(
  forecast_date_int = fc_dates,
  forecast_generation_date_int = gen_dates,
  forecast_date_chr = as.character(fc_dates)
)
if (g_evaluation_mode) {
  # Optional: keep only the last N dates for a fast partial evaluation
  # (scripts/oracle/capture.R). Inert when unset.
  g_evaluation_n_dates <- as.integer(Sys.getenv("EVALUATION_N_DATES", "0"))
  if (!is.na(g_evaluation_n_dates) && g_evaluation_n_dates > 0) {
    g_forecast_schedule <- slice_tail(g_forecast_schedule, n = g_evaluation_n_dates)
  }
}
# Thin derived views; shared R/ code and other targets here consume these.
g_forecast_dates <- g_forecast_schedule$forecast_date_int
g_forecast_generation_dates <- g_forecast_schedule$forecast_generation_date_int

# Forecaster grid — behavior is defined by (id, bare forecaster function,
# params). Per-forecaster wrapping that isn't a modeling parameter (ahead
# multiplier, target-date shift, extra-source join, source/geo filtering, as-of
# policy) is declared inline as spec columns and applied by run_forecaster();
# make_forecaster_grid() separates them from the params list-column and fills
# defaults (see FORECASTER_SPEC_DEFAULTS) for whatever a forecaster omits. Each
# row is built via make_forecaster_grid() so the params list-column matches the
# exploration convention (trainer stored as a symbol, lags unwrapped).
g_forecaster_params_grid <- list(
  cdc_baseline = tibble(
    id = "cdc_baseline",
    forecaster = "g_baseline_forecaster"
  ),
  linear = tibble(
    id = "linear",
    forecaster = "forecaster_baseline_linear",
    residual_tail = 0.99,
    residual_center = 0.35,
    no_intercept = TRUE,
    filter_sources = list(c("nhsn", "nssp"))
  ),
  linear_no_population_scale = tibble(
    id = "linear_no_population_scale",
    forecaster = "forecaster_baseline_linear",
    residual_tail = 0.99,
    residual_center = 0.35,
    no_intercept = TRUE,
    population_scale = FALSE
  ),
  # The scaled_pop_seasonal family opts into sort_quantiles: its whitening step
  # has edge cases that emit crossing quantiles, and explore evaluates these
  # forecasters sorted, so shipping them unsorted would drift from what was
  # validated. Forecasters that are monotone by construction stay unsorted so
  # a crossing there surfaces as an error (validate_forecast_output).
  windowed_seasonal = tibble(
    id = "windowed_seasonal",
    forecaster = "scaled_pop_seasonal",
    outcome = "value",
    trainer = "g_quantreg",
    seasonal_method = "window",
    pop_scaling = FALSE,
    lags = list(c(0, 7)),
    keys_to_ignore = g_very_latent_locations,
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    sort_quantiles = TRUE
  ),
  windowed_seasonal_extra_sources = tibble(
    id = "windowed_seasonal_extra_sources",
    forecaster = "scaled_pop_seasonal",
    outcome = "value",
    extra_sources = "nssp",
    trainer = "g_quantreg",
    seasonal_method = "window",
    drop_non_seasons = TRUE,
    pop_scaling = FALSE,
    lags = list(list(c(0, 7), c(0, 7))),
    keys_to_ignore = g_very_latent_locations,
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    join_extra_data = TRUE,
    excluded_geos = list(c("mo", "wy")),
    sort_quantiles = TRUE
  ),
  climate_base = tibble(
    id = "climate_base",
    forecaster = "climatological_model",
    filter_sources = list(c("nhsn", "nssp"))
  ),
  climate_geo_agged = tibble(
    id = "climate_geo_agged",
    forecaster = "climatological_model",
    geo_agg = TRUE,
    filter_sources = list(c("nhsn", "nssp"))
  ),
  # Cheats by always using the latest available data revision (as a limit test).
  seasonal_nssp_cheating = tibble(
    id = "seasonal_nssp_cheating",
    forecaster = "scaled_pop_seasonal",
    outcome = "value",
    extra_sources = "nssp",
    trainer = "g_quantreg",
    seasonal_method = "window",
    drop_non_seasons = TRUE,
    pop_scaling = FALSE,
    lags = list(list(c(0, 7), c(0, 7))),
    keys_to_ignore = g_very_latent_locations,
    as_of_policy = "cheating",
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    join_extra_data = TRUE,
    excluded_geos = list(c("mo", "wy")),
    sort_quantiles = TRUE
  ),
  revision_aware_nssp = tibble(
    id = "revision_aware_nssp",
    forecaster = "scaled_pop_seasonal_revision",
    outcome = "value",
    extra_sources = "nssp",
    trainer = "g_quantreg",
    lags = list(c(0, 7, 14)),
    pop_scaling = FALSE,
    scale_method = "none",
    center_method = "none",
    nonlin_method = "none",
    seasonal_backward_window = 5 * 7,
    seasonal_forward_window = 3 * 7,
    train_sources = list(c("nhsn")),
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    sort_quantiles = TRUE,
    needs_archive = TRUE
  )
) %>%
  imap(\(tib, family) make_forecaster_grid(tib, family)) %>%
  bind_rows() %>%
  select(-family)


# ================================ PARAMETERS AND DATA TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  tar_target(aheads, command = g_aheads),
  # Needed by create_flu_data_targets()
  tar_target(forecast_dates, command = g_forecast_dates),
  tar_file(
    forecast_report_rmd,
    command = "scripts/reports/forecast_report.Rmd"
  ),
  tar_file(
    ongoing_score_report_rmd,
    command = "scripts/reports/ongoing_score_report.Rmd"
  ),
  tar_file(
    name = score_report_rmd,
    command = "scripts/reports/score_report.Rmd"
  ),
  tar_file(
    flu_geo_exclusions,
    command = "scripts/flu_geo_exclusions.csv"
  ),
  tar_file(
    flu_nssp_geo_exclusions,
    command = "scripts/flu_nssp_geo_exclusions.csv"
  ),
  tar_file(
    flu_data_substitutions,
    command = "scripts/flu_data_substitutions.csv"
  ),
  create_flu_data_targets(),
  tar_target(
    name = nhsn_archive_data,
    command = {
      get_nhsn_data_archive("flu")
    },
    cue = tar_cue("always")
  ),
  # Canonical training archive: the version-independent munging that used to run
  # per (forecaster x date) inside full_data, hoisted here so it runs once. The
  # per-date full_data target is now just an as-of slice + data substitutions +
  # metadata stamping.
  tar_target(
    name = nhsn_prod_archive,
    command = {
      # season info is computed on the Saturday-labeled time_values (matching the
      # old order) *before* the Wednesday shift, so the season columns are
      # identical to the previous per-date computation.
      nhsn_dt <- nhsn_archive_data$DT %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = time_value - 3,
          source = "nhsn"
        ) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
      # ILI+/flusurv historical augmentation rows (static, finalized data for the
      # seasonal forecasters). Previously bound at joined_archive_data's
      # versions_end into every full_data snapshot. Folded in here as faux-
      # versioned rows with version = time_value: these are historical seasons
      # (time_values predate the forecast window), so every as-of slice includes
      # all of them exactly as the old unconditional bind did, while keeping the
      # archive honestly versioned like the explore pipeline.
      extra_dt <- joined_archive_data %>%
        epix_as_of(joined_archive_data$versions_end) %>%
        mutate(epiweek = epiweek(time_value), epiyear = epiyear(time_value)) %>%
        filter((agg_level == "state") | (agg_level == "nation")) %>%
        select(geo_value, source, time_value, hhs, season, season_week, epiweek, epiyear) %>%
        rename(value = hhs) %>%
        filter(source != "nhsn") %>%
        mutate(version = time_value)
      # The faux-versioning above is only equivalent to the old unconditional
      # bind if no augmentation row lands inside the forecast window (flusurv is
      # a live fetch, currently bounded at 2020 by the burden-estimate join;
      # ILI+ ends mid-2024). Fail loudly if that ever drifts.
      stopifnot(max(extra_dt$time_value) < min(as.Date(g_forecast_generation_dates)))
      bind_rows(nhsn_dt, extra_dt) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  tar_target(
    name = nhsn_latest_data,
    command = {
      # Uses the *raw* archive on purpose: truth_data and scoring want
      # Saturday-labeled time_values and the raw `value`/geo names, not the
      # canonical Wednesday-shifted archive.
      nhsn_archive_data %>%
        epix_as_of(min(g_reference_date, nhsn_archive_data$versions_end)) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
    }
  ),
  tar_target(
    name = nssp_archive_data,
    command = {
      up_to_date_nssp_state_archive("influenza")
    },
    cue = tar_cue("always")
  ),
  # Canonical nssp-as-target archive: the version-independent transforms (rename
  # nssp -> value so nssp plays the target role, week-align to Wednesday, stamp
  # source honestly, add season info) hoisted out of the per-date forecast_nssp
  # target. The forecaster is pointed at these `nssp`-stamped rows via
  # run_forecaster(primary_source = "nssp") rather than mislabeling them "nhsn".
  tar_target(
    name = nssp_target_archive,
    command = {
      nssp_archive_data$DT %>%
        rename(value = nssp) %>%
        mutate(
          time_value = floor_date(time_value, "week", week_start = 7) + 3,
          source = "nssp"
        ) %>%
        add_season_info() %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  tar_target(
    name = nssp_latest_data,
    command = {
      # Raw archive on purpose (see nhsn_latest_data): keeps the `nssp` column and
      # Saturday time_values for truth_data / scoring.
      nssp_archive_data %>%
        epix_as_of(min(g_reference_date, nssp_archive_data$versions_end))
    }
  ),
  # nhsn_prod_archive with the nssp column merged in (stamped source = "nhsn"
  # so epix_merge joins onto nhsn rows). Handed to revision_aware_nssp via the
  # needs_archive branch in forecast_nhsn. nssp_archive_data is already
  # Wednesday-aligned by up_to_date_nssp_state_archive.
  tar_target(
    name = nhsn_nssp_prod_archive,
    command = {
      nssp_for_merge <- nssp_archive_data$DT %>%
        mutate(source = "nhsn") %>%
        as.data.frame() %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
      nhsn_prod_archive %>%
        epix_merge(nssp_for_merge, sync = "locf")
    }
  )
)


# ================================ FORECAST TARGETS ================================
forecast_targets <- tar_map(
  values = tidyr::expand_grid(g_forecaster_params_grid, g_forecast_schedule),
  names = c("id", "forecast_date_chr"),
  tar_target(
    name = full_data,
    command = {
      # As-of slice of the canonical archive (season info, geo rename, Wednesday
      # shift, source stamp, insufficient-geo drop, and the ILI+/flusurv extras
      # are already baked in). Only the version-dependent steps remain here.
      make_forecast_snapshot(
        nhsn_prod_archive,
        forecast_date = forecast_date_int,
        generation_date = forecast_generation_date_int,
        as_of_policy = as_of_policy,
        substitutions = flu_data_substitutions
      )
    }
  ),
  # As-of slice of the canonical nssp-as-target archive (rename, week-align,
  # source stamp, season info already baked in). Hoisted into its own snapshot
  # target so forecast_nssp mirrors forecast_nhsn: one snapshot target feeding a
  # single run_forecaster call, with only the exogenous extra_data prep inline.
  tar_target(
    name = nssp_forecast_data,
    command = {
      make_forecast_snapshot(
        nssp_target_archive,
        forecast_date = forecast_date_int,
        generation_date = forecast_generation_date_int,
        as_of_policy = as_of_policy
      )
    }
  ),
  tar_target(
    name = forecast_nssp,
    command = {
      # Seed from the semantic cell key, not the target name. `targets` otherwise
      # derives each target's seed from its name, so renaming a target silently
      # moves the forecast for the stochastic forecasters (linear, cdc_baseline,
      # linear_no_population_scale). Seeded here rather than inside the
      # forecasters because only the harness knows (signal, date, ahead), and so
      # the set of stochastic forecasters doesn't have to be tracked by hand.
      set.seed(targets::tar_seed_create(
        paste(id, "nssp", forecast_date_chr, aheads, sep = "/")
      ))
      # Exogenous input: full_data (nhsn) spoofed into the `nssp` column to switch
      # its role from target to predictor for the nssp-as-target forecast.
      full_data_modified <- full_data %>%
        rename(nssp = value) %>%
        filter(source == "nhsn") %>%
        select(-c(source, epiweek, epiyear, season, season_week))
      run_forecaster(
        snapshot = nssp_forecast_data, forecaster = forecaster, aheads = aheads * ahead_multiplier,
        params = params, id = id,
        target_date_shift = target_date_shift,
        join_extra_data = join_extra_data, extra_data = full_data_modified,
        filter_sources = filter_sources, excluded_geos = excluded_geos,
        sort_quantiles = sort_quantiles,
        # nssp is the target here; snapshot rows are stamped source = "nssp".
        primary_source = "nssp"
      )
    },
    pattern = map(aheads)
  ),
  # As-of slice of the raw nssp archive used as an exogenous predictor
  # (extra_sources = "nssp"). Keeps the `nssp` column, unlike nssp_forecast_data
  # (the renamed nssp-as-target archive). Routed through make_forecast_snapshot
  # so the cheating cutoff is generation_date, matching full_data.
  tar_target(
    name = nssp_exogenous_data,
    command = {
      make_forecast_snapshot(
        nssp_archive_data,
        forecast_date = forecast_date_int,
        generation_date = forecast_generation_date_int,
        as_of_policy = as_of_policy
      )
    }
  ),
  tar_target(
    name = forecast_nhsn,
    command = {
      # See forecast_nssp: seed from the semantic cell key, not the target name.
      set.seed(targets::tar_seed_create(
        paste(id, "nhsn", forecast_date_chr, aheads, sep = "/")
      ))
      # Revision-aware forecasters (needs_archive = TRUE) receive the truncated
      # archive directly; everyone else receives the as-of epi_df snapshot.
      snapshot <- if (needs_archive) {
        make_forecast_archive_snapshot(
          nhsn_nssp_prod_archive,
          forecast_date_int,
          forecast_generation_date_int
        )
      } else {
        full_data
      }
      run_forecaster(
        snapshot = snapshot, forecaster = forecaster, aheads = aheads * ahead_multiplier,
        params = params, id = id,
        target_date_shift = target_date_shift,
        join_extra_data = join_extra_data, extra_data = nssp_exogenous_data,
        filter_sources = filter_sources, excluded_geos = excluded_geos,
        sort_quantiles = sort_quantiles
      )
    },
    pattern = map(aheads)
  )
)

combined_forecast_targets <- build_combined_forecast_targets(forecast_targets)


# ================================ ENSEMBLE TARGETS ================================
# Shared with covid (build_prod_ensemble_targets / run_ensemble in
# R/targets/prod_shared.R, R/targets/ensemble_runner.R); per-disease asymmetries
# are the spec values below. AR components are windowed_seasonal(_extra_sources);
# climate_linear's declared components are used only for run_ensemble()'s
# presence assert, not to pre-filter (see run_ensemble() doc for why).
g_ensemble_specs <- list(
  climate_linear = list(
    id = "climate_linear",
    method = "climate_linear",
    components = list(
      nhsn = c("climate_base", "climate_geo_agged", "linear"),
      nssp = c("climate_base", "climate_geo_agged", "linear_no_population_scale")
    ),
    # flu nhsn historically ran uncapped, i.e. the ensemble_climate_linear defaults
    climate_caps = list(nhsn = c(0.9, 1), nssp = c(0.6, 0.6)),
    apply_geo_exclusions = TRUE,
    sort_quantiles = TRUE
  ),
  ar_only = list(
    id = "ens_ar_only",
    method = "mean",
    components = list(nhsn = c("windowed_seasonal", "windowed_seasonal_extra_sources")),
    apply_geo_exclusions = FALSE,
    sort_quantiles = TRUE
  ),
  ensemble_mix = list(
    id = "ensemble_mix",
    method = "weighted",
    components = list(
      nhsn = c("windowed_seasonal", "windowed_seasonal_extra_sources", "revision_aware_nssp"),
      nssp = c("windowed_seasonal", "windowed_seasonal_extra_sources")
    ),
    drop_negative_aheads = list(nhsn = FALSE, nssp = TRUE),
    apply_geo_exclusions = FALSE,
    sort_quantiles = FALSE
  )
)
ensemble_targets <- build_prod_ensemble_targets(
  g_forecast_schedule,
  disease = "flu",
  geo_exclusions_file = "flu_geo_exclusions",
  nssp_geo_exclusions_file = "flu_nssp_geo_exclusions",
  ensemble_spec = g_ensemble_specs,
  climate_submission_excluded_geos = c("as", "gu", "mh")
)


# ================================ SCORE TARGETS ================================
external_forecast_targets <- build_external_forecast_targets()

joined_targets <- list2(
  tar_combine(
    name = local_forecasts_and_ensembles_nhsn,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = purrr::map(list(!!!.x), "nhsn") %>% dplyr::bind_rows()
  ),
  tar_combine(
    name = local_forecasts_and_ensembles_nssp,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = purrr::map(list(!!!.x), "nssp") %>% dplyr::bind_rows()
  ),
  tar_target(
    name = local_scores_nhsn,
    command = {
      score_forecasts(nhsn_latest_data, local_forecasts_and_ensembles_nhsn, "wk inc flu hosp")
    }
  ),
  tar_target(
    name = local_scores_nssp,
    command = {
      nssp_latest_data %>%
        rename(value = nssp) %>%
        mutate(time_value = ceiling_date(time_value, unit = "week") - 1) %>%
        score_forecasts(local_forecasts_and_ensembles_nssp %>% mutate(value = value / 100), "wk inc flu prop ed visits")
    }
  ),
  tar_combine(
    name = joined_forecasts_and_ensembles_nhsn,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = filter_shared_geo_dates(
      purrr::map(list(!!!.x), "nhsn") %>% dplyr::bind_rows(),
      external_forecasts_full %>% filter(target == "wk inc flu hosp") %>% select(-target),
      min_locations = 52,
      min_dates = 40
    )
  ),
  tar_combine(
    name = joined_forecasts_and_ensembles_nssp,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = filter_shared_geo_dates(
      purrr::map(list(!!!.x), "nssp") %>% dplyr::bind_rows(),
      external_forecasts_full %>% filter(target == "wk inc flu prop ed visits") %>% select(-target) %>% mutate(value = value * 100),
      min_locations = 50,
      min_dates = 14
    )
  )
)

combined_targets <- build_combined_targets(external_forecast_targets)

if (g_evaluation_mode) {
  score_notebook <- build_backtest_score_targets()
} else {
  score_notebook <- list2(
    tar_target(
      ongoing_nhsn_score_notebook,
      command = {
        if (!dir.exists(here::here("reports"))) {
          dir.create(here::here("reports"))
        }
        if (
          external_forecasts_full %>%
            filter(
              forecast_date >= round_date(Sys.Date() - 3, "week", 6) - 4 * 7,
              target == "wk inc flu hosp"
            ) %>%
            distinct(forecast_date) %>%
            nrow() == 0
        ) {
          return()
        }
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_nhsn_scoring_individual.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            target = "nhsn",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc flu hosp") %>% select(-target),
            archive = nhsn_archive_data,
            scores = external_scores_nhsn_full,
            averaging_method = "individual"
          )
        )
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_nhsn_scoring_common.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            target = "nhsn",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc flu hosp") %>% select(-target),
            archive = nhsn_archive_data,
            scores = external_scores_nhsn_full,
            averaging_method = "common"
          )
        )
      }
    ),
    tar_target(
      ongoing_nssp_score_notebook,
      command = {
        if (!dir.exists(here::here("reports"))) {
          dir.create(here::here("reports"))
        }
        if (external_forecasts_full %>%
          filter(
            forecast_date >= round_date(Sys.Date() - 3, "week", 6) - 4 * 7,
            target == "wk inc flu prop ed visits"
          ) %>% distinct(forecast_date) %>% nrow() == 0) {
          return()
        }
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_nssp_scoring_individual.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            target = "nssp",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc flu prop ed visits") %>% select(-target),
            archive = nssp_archive_data,
            scores = external_scores_nssp_full,
            averaging_method = "individual"
          )
        )
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "reports",
            sprintf("%s_flu_nssp_scoring_common.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "flu",
            target = "nssp",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc flu prop ed visits") %>% select(-target),
            archive = nssp_archive_data,
            scores = external_scores_nssp_full,
            averaging_method = "common"
          )
        )
      }
    )
  )
}

list2(
  parameters_and_date_targets,
  forecast_targets,
  ensemble_targets,
  combined_forecast_targets,
  external_forecast_targets,
  combined_targets,
  joined_targets,
  score_notebook
)
