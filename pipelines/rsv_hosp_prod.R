# The RSV Hospitalization Production Forecasting Pipeline.
#
# Two targets projects share this script (see _targets.yaml):
#   rsv_hosp_prod        - weekly production run, as_of = today
#   rsv_hosp_evaluation  - historical replay over past forecast dates, scored
#                          against latest truth
# Dispatch is on the project name; each project keeps its own store, so an
# evaluation run cannot invalidate the production cache. The structure follows
# covid_hosp_prod.R; the differences are the grid, the ensemble spec, and no
# calibration or CMU-climate_baseline submission.
suppressPackageStartupMessages(source("R/load_all.R"))


# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions).

# Setup targets config.
set_targets_config()
g_aheads <- -1:3
g_submission_directory <- Sys.getenv("RSV_SUBMISSION_DIRECTORY", "cache")
g_insufficient_data_geos <- c("as", "mp", "vi", "gu")
# Iowa NSSP is out for an extended period (from Oct 2026).
g_insufficient_data_geos_nssp <- c(g_insufficient_data_geos, "ia")
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "rsv"
g_s3_prefix <- "exploration"
g_external_object_name <- glue::glue("exploration/2024-2025_{g_disease}_hosp_forecasts.parquet")
# date to cut the truth data off at, so we don't have too much of the past
g_truth_data_date <- "2023-09-01"
# Whether we're running in evaluation (historical replay) mode. Dispatch is on
# the targets project name (see flu_hosp_prod.R for the full rationale);
# TAR_RUN_PROJECT wins so a stale TAR_PROJECT in the shell can't flip the mode.
.g_project <- Sys.getenv("TAR_RUN_PROJECT", Sys.getenv("TAR_PROJECT", "rsv_hosp_prod"))
g_evaluation_mode <- .g_project == "rsv_hosp_evaluation"
# The pipeline's notion of "today"; see covid_hosp_prod.R.
g_reference_date <- {
  raw <- Sys.getenv("FORECAST_REFERENCE_DATE", "")
  if (nzchar(raw)) as.Date(raw) else Sys.Date()
}
# The forecast schedule, held as one tibble so forecast_date / generation_date
# stay row-aligned by construction (see flu_hosp_prod.R for column docs).
if (!g_evaluation_mode) {
  gen_dates <- g_reference_date
  fc_dates <- floor_date(gen_dates, "weeks", week_start = 3)
  # the forecast is actually for the wednesday beforehand for these days
  if (gen_dates %in% as.Date(c("2025-12-29"))) {
    fc_dates <- as.Date("2025-12-24")
  }
} else {
  # NHSN RSV versions start 2024-12-11, so the replay starts there.
  gen_dates <- c(
    as.Date(c("2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
    seq.Date(as.Date("2025-01-08"), as.Date("2025-12-17"), by = 7L),
    as.Date(c("2025-12-29")),
    seq.Date(as.Date("2025-12-31"), g_reference_date, by = 7L)
  )
  fc_dates <- seq.Date(as.Date("2024-12-11"), g_reference_date, by = 7L)
}
g_forecast_schedule <- tibble(
  forecast_date_int = fc_dates,
  forecast_generation_date_int = gen_dates,
  forecast_date_chr = as.character(fc_dates)
)
if (g_evaluation_mode) {
  g_forecast_schedule <- filter_evaluation_dates(g_forecast_schedule)
  g_evaluation_n_dates <- as.integer(Sys.getenv("EVALUATION_N_DATES", "0"))
  if (!is.na(g_evaluation_n_dates) && g_evaluation_n_dates > 0) {
    g_forecast_schedule <- slice_tail(g_forecast_schedule, n = g_evaluation_n_dates)
  }
}
g_forecast_dates <- g_forecast_schedule$forecast_date_int
g_forecast_generation_dates <- g_forecast_schedule$forecast_generation_date_int

if (!g_evaluation_mode) {
  stamp_default_weights("pipelines/rsv_geo_exclusions.csv", g_forecast_dates[[1L]])
  stamp_default_weights("pipelines/rsv_nssp_geo_exclusions.csv", g_forecast_dates[[1L]])
}

# Trainers referenced by symbol in the grid params so tar_map embeds the
# symbol, not the model_spec.
# "br" (the default solver) can loop forever on some inputs; "fn" finished on
# the one we hit. See notes/quantreg-solver.md.
g_quantreg <- epipredict::quantile_reg(method = "fn")
g_quantreg_fn <- epipredict::quantile_reg(method = "fn")

# Forecaster grid; see covid_hosp_prod.R for the spec columns. The three
# ensemble_mix components are rsv explore forecasters, chosen from the
# 2024/25 and 2025/26 notebooks; the explore id is in each comment.
g_forecaster_params_grid <- list(
  linear = tibble(
    id = "linear",
    forecaster = "forecaster_baseline_linear",
    residual_tail = 0.97,
    residual_center = 0.097,
    no_intercept = TRUE
  ),
  linear_no_population_scale = tibble(
    id = "linear_no_population_scale",
    forecaster = "forecaster_baseline_linear",
    residual_tail = 0.97,
    residual_center = 0.097,
    no_intercept = TRUE,
    population_scale = FALSE
  ),
  climate_base = tibble(
    id = "climate_base",
    forecaster = "climatological_model"
  ),
  climate_geo_agged = tibble(
    id = "climate_geo_agged",
    forecaster = "climatological_model",
    geo_agg = TRUE
  ),
  # porcelain.americanbulldog
  windowed_seasonal = tibble(
    id = "windowed_seasonal",
    forecaster = "scaled_pop_seasonal",
    outcome = "value",
    trainer = "g_quantreg",
    lags = list(c(0, 7, 14, 21)),
    pop_scaling = FALSE,
    n_training = Inf,
    seasonal_method = "window",
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    sort_quantiles = TRUE
  ),
  # emasculative.jackal
  nssp_exogenous = tibble(
    id = "nssp_exogenous",
    forecaster = "scaled_pop",
    outcome = "value",
    trainer = "g_quantreg",
    extra_sources = "nssp",
    lags = list(list(c(0, 7, 14, 21), c(0, 7, 14))),
    pop_scaling = FALSE,
    scale_method = "quantile",
    n_training = Inf,
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    join_extra_data = TRUE,
    sort_quantiles = TRUE,
    # At h-1 the quantile regression does not finish, and ensemble_mix drops
    # h-1 for this component.
    run_aheads = list(c(0, 7, 14, 21))
  ),
  # wellloved.husky
  revision_aware = tibble(
    id = "revision_aware",
    forecaster = "scaled_pop_seasonal_revision",
    outcome = "value",
    trainer = "g_quantreg_fn",
    lags = list(c(0, 7)),
    pop_scaling = FALSE,
    scale_method = "quantile",
    center_method = "median",
    nonlin_method = "quart_root",
    seasonal_backward_window = 35L,
    seasonal_forward_window = 21L,
    finalization_coverage = 0.8,
    needs_archive = TRUE,
    ahead_multiplier = 7L,
    target_date_shift = 3L,
    sort_quantiles = TRUE
  )
) %>%
  imap(\(tib, family) make_forecaster_grid(tib, family)) %>%
  bind_rows() %>%
  select(-family)
g_substitutions_arg <- rlang::sym("rsv_data_substitutions")
if (g_evaluation_mode) {
  g_forecaster_params_grid <- filter_evaluation_forecasters(g_forecaster_params_grid)
  g_substitutions_arg <- evaluation_substitutions_arg("rsv_data_substitutions")
}


# ================================ PARAMETERS AND DATA TARGETS ================================
parameters_and_date_targets <- rlang::list2(
  tar_target(aheads, command = g_aheads),
  tar_file(
    forecast_report_rmd,
    command = "pipelines/templates/forecast_report.Rmd"
  ),
  tar_file(
    ongoing_score_report_rmd,
    command = "pipelines/templates/ongoing_score_report.Rmd"
  ),
  tar_file(
    name = score_report_rmd,
    command = "pipelines/templates/score_report.Rmd"
  ),
  tar_file(
    name = rsv_geo_exclusions,
    command = "pipelines/rsv_geo_exclusions.csv"
  ),
  tar_file(
    name = rsv_nssp_geo_exclusions,
    command = "pipelines/rsv_nssp_geo_exclusions.csv"
  ),
  tar_file(
    name = rsv_data_substitutions,
    command = "pipelines/rsv_data_substitutions.csv"
  ),
  tar_target(
    name = nhsn_archive_data,
    command = {
      get_nhsn_data_archive("rsv")
    },
    cue = tar_cue("always")
  ),
  # Canonical training archive; see nhsn_prod_archive in covid_hosp_prod.R.
  tar_target(
    name = nhsn_prod_archive,
    command = {
      nhsn_archive_data$DT %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = floor_date(time_value, "week", week_start = 7) + 3,
          source = "nhsn"
        ) %>%
        filter(geo_value %nin% g_insufficient_data_geos) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  tar_target(
    name = nhsn_latest_data,
    command = {
      nhsn_archive_data %>%
        epix_as_of(min(g_reference_date, nhsn_archive_data$versions_end)) %>%
        filter(geo_value %nin% g_insufficient_data_geos)
    }
  ),
  tar_target(
    name = nssp_archive_data,
    command = {
      up_to_date_nssp_state_archive("rsv")
    },
    cue = tar_cue("always")
  ),
  # Canonical nssp-as-target archive; see nssp_target_archive in covid_hosp_prod.R.
  tar_target(
    name = nssp_target_archive,
    command = {
      nssp_archive_data$DT %>%
        rename(value = nssp) %>%
        add_season_info() %>%
        mutate(
          geo_value = ifelse(geo_value == "usa", "us", geo_value),
          time_value = floor_date(time_value, "week", week_start = 7) + 3,
          source = "nssp"
        ) %>%
        filter(geo_value %nin% g_insufficient_data_geos_nssp) %>%
        as_epi_archive(other_keys = "source", compactify = TRUE)
    }
  ),
  tar_target(
    name = nssp_latest_data,
    command = {
      nssp_archive_data %>%
        epix_as_of(min(g_reference_date, nssp_archive_data$versions_end))
    }
  ),
  # The nhsn archive without the source key, for the revision-aware forecaster.
  # It has no exogenous sources, so no nssp merge.
  tar_target(
    name = nhsn_revision_archive,
    command = {
      nhsn_prod_archive$DT %>%
        select(-source) %>%
        as_epi_archive(compactify = TRUE)
    }
  ),
)


# ================================ FORECAST TARGETS ================================
forecast_targets <- tar_map(
  values = tidyr::expand_grid(g_forecaster_params_grid, g_forecast_schedule),
  names = c("id", "forecast_date_chr"),
  tar_target(
    name = full_data,
    command = {
      make_forecast_snapshot(
        nhsn_prod_archive,
        forecast_date = forecast_date_int,
        generation_date = forecast_generation_date_int,
        as_of_policy = as_of_policy,
        substitutions = !!g_substitutions_arg
      )
    }
  ),
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
  # As-of slice of the raw nssp archive used as an exogenous predictor.
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
      if (!is.null(run_aheads) && !(aheads * ahead_multiplier) %in% run_aheads) {
        return(make_null_forecast())
      }
      # Seed from the semantic cell key; see covid_hosp_prod.R.
      set.seed(targets::tar_seed_create(
        paste(id, "nhsn", forecast_date_chr, aheads, sep = "/")
      ))
      snapshot <- if (needs_archive) {
        make_forecast_archive_snapshot(
          nhsn_revision_archive,
          forecast_date = forecast_date_int,
          generation_date = forecast_generation_date_int
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
    pattern = map(aheads),
    resources = forecast_resources()
  ),
  tar_target(
    name = forecast_nssp,
    command = {
      if (!is.null(run_aheads) && !(aheads * ahead_multiplier) %in% run_aheads) {
        return(make_null_forecast())
      }
      set.seed(targets::tar_seed_create(
        paste(id, "nssp", forecast_date_chr, aheads, sep = "/")
      ))
      # nhsn spoofed into the `nssp` column so it is the exogenous predictor
      # for the nssp-as-target forecast.
      full_data_modified <- full_data %>%
        rename(nssp = value) %>%
        select(geo_value, time_value, nssp)
      # finalization_min_value defaults to a count-scale threshold that zeroes
      # out nssp's proportion-scale values.
      params_effective <- if (needs_archive) {
        modifyList(params, list(finalization_min_value = 0))
      } else {
        params
      }
      snapshot <- if (needs_archive) {
        make_forecast_archive_snapshot(
          nssp_target_archive,
          forecast_date = forecast_date_int,
          generation_date = forecast_generation_date_int
        )
      } else {
        nssp_forecast_data
      }
      run_forecaster(
        snapshot = snapshot, forecaster = forecaster, aheads = aheads * ahead_multiplier,
        params = params_effective, id = id,
        target_date_shift = target_date_shift,
        join_extra_data = join_extra_data, extra_data = full_data_modified,
        filter_sources = filter_sources, excluded_geos = excluded_geos,
        sort_quantiles = sort_quantiles,
        primary_source = "nssp"
      )
    },
    pattern = map(aheads),
    resources = forecast_resources()
  )
)

combined_forecast_targets <- build_combined_forecast_targets(forecast_targets)


# ================================ ENSEMBLE TARGETS ================================
# ensemble_mix is an equal mix of its three components; the weights are in
# pipelines/rsv_geo_exclusions.csv and pipelines/rsv_nssp_geo_exclusions.csv.
# climate_linear keeps a small weight there so that a (geo, ahead) with no
# component forecast falls back to it.
g_ensemble_specs <- list(
  climate_linear = list(
    id = "climate_linear",
    method = "climate_linear",
    components = list(
      nhsn = c("climate_base", "climate_geo_agged", "linear"),
      nssp = c("climate_base", "climate_geo_agged", "linear_no_population_scale")
    ),
    climate_caps = list(nhsn = c(0.6, 0.6), nssp = c(0.6, 0.6)),
    apply_geo_exclusions = TRUE,
    sort_quantiles = TRUE
  ),
  ar_only = list(
    id = "ens_ar_only",
    method = "mean",
    components = list(nhsn = c("windowed_seasonal", "nssp_exogenous")),
    apply_geo_exclusions = FALSE,
    sort_quantiles = TRUE
  ),
  ensemble_mix = list(
    id = "ensemble_mix",
    method = "weighted",
    components = list(
      nhsn = c("windowed_seasonal", "nssp_exogenous", "revision_aware"),
      nssp = c("windowed_seasonal", "nssp_exogenous", "revision_aware")
    ),
    drop_negative_aheads = list(nhsn = TRUE, nssp = TRUE),
    # revision_aware is the only component at h-1.
    drop_negative_aheads_exempt = list(nhsn = c("revision_aware"), nssp = c("revision_aware")),
    apply_geo_exclusions = FALSE,
    sort_quantiles = FALSE
  )
)
g_run_ensembles <- !g_evaluation_mode || ensemble_components_present(g_ensemble_specs, g_forecaster_params_grid$id)
ensemble_targets <- build_prod_ensemble_targets(
  g_forecast_schedule,
  disease = "rsv",
  geo_exclusions_file = "rsv_geo_exclusions",
  nssp_geo_exclusions_file = "rsv_nssp_geo_exclusions",
  ensemble_spec = g_ensemble_specs,
  climate_submission = FALSE,
  calibrated = FALSE,
  run_ensembles = g_run_ensembles
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
      score_forecasts(nhsn_latest_data, local_forecasts_and_ensembles_nhsn, "wk inc rsv hosp")
    }
  ),
  tar_target(
    name = local_scores_nssp,
    command = {
      nssp_latest_data %>%
        rename(value = nssp) %>%
        mutate(time_value = ceiling_date(time_value, unit = "week") - 1) %>%
        score_forecasts(local_forecasts_and_ensembles_nssp %>% mutate(value = value / 100), "wk inc rsv prop ed visits")
    }
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
        if (!dir.exists(here::here("rendered_reports"))) {
          dir.create(here::here("rendered_reports"))
        }
        # Don't run if there aren't forecasts in the past 4 weeks to evaluate
        if (external_forecasts_full %>%
          filter(
            forecast_date >= round_date(Sys.Date() - 3, "week", 6) - 4 * 7,
            target == "wk inc rsv hosp"
          ) %>% distinct(forecast_date) %>% nrow() == 0) {
          return()
        }
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "rendered_reports",
            sprintf("%s_rsv_nhsn_scoring.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "rsv",
            target = "nhsn",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc rsv hosp") %>% select(-target),
            archive = nhsn_archive_data,
            scores = external_scores_nhsn_full
          )
        )
      }
    ),
    tar_target(
      ongoing_nssp_score_notebook,
      command = {
        if (!dir.exists(here::here("rendered_reports"))) {
          dir.create(here::here("rendered_reports"))
        }
        if (external_forecasts_full %>%
          filter(
            forecast_date >= round_date(Sys.Date() - 3, "week", 6) - 4 * 7,
            target == "wk inc rsv prop ed visits"
          ) %>% distinct(forecast_date) %>% nrow() == 0) {
          return()
        }
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "rendered_reports",
            sprintf("%s_rsv_nssp_scoring_individual.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "rsv",
            target = "nssp",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc rsv prop ed visits") %>% select(-target),
            archive = nssp_archive_data,
            scores = external_scores_nssp_full,
            averaging_method = "individual"
          )
        )
        rmarkdown::render(
          ongoing_score_report_rmd,
          output_file = here::here(
            "rendered_reports",
            sprintf("%s_rsv_nssp_scoring_common.html", as.Date(Sys.Date()))
          ),
          params = list(
            disease = "rsv",
            target = "nssp",
            external_forecasts = external_forecasts_full %>% filter(target == "wk inc rsv prop ed visits") %>% select(-target),
            archive = nssp_archive_data,
            scores = external_scores_nssp_full,
            averaging_method = "common"
          )
        )
      }
    ),
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
