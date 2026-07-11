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
g_excluded_geos <- c("as", "gu", "mh")
g_time_value_adjust <- 3
g_fetch_args <- epidatr::fetch_args_list(return_empty = FALSE, timeout_seconds = 400)
g_disease <- "flu"
g_s3_prefix <- "exploration"
g_external_object_name <- glue::glue("2024/2024-2025_{g_disease}_hosp_forecasts.parquet")
# needed for windowed_seasonal
g_very_latent_locations <- list(list(
  c("source"),
  c("flusurv", "ILI+")
))
# Date to cut the truth data off at, so we don't have too much of the past for
# plotting.
g_truth_data_date <- "2023-09-01"

# The pipeline's notion of "today", read once here so the whole run can be pinned
# for reproducible oracle captures (REFACTOR.md gotcha: forecast dates otherwise
# move with the calendar). Unset -> Sys.Date(), i.e. current production behavior.
# Both modes derive their forecast-date vectors from this, and the latest as-of
# slice uses it, so pinning it turns prod-"latest" into a reproducible as-of.
g_forecast_reference_date <- function() {
  raw <- Sys.getenv("FORECAST_REFERENCE_DATE", "")
  if (nzchar(raw)) as.Date(raw) else Sys.Date()
}

# Mode dispatch on the targets project name. scripts/run.R and the oracle pass
# TAR_RUN_PROJECT; direct tar_* invocations (e.g. make prune-flu-evaluation) set
# TAR_PROJECT. TAR_RUN_PROJECT wins so a stale TAR_PROJECT in the shell can't
# flip the mode.
g_evaluation_mode <-
  Sys.getenv("TAR_RUN_PROJECT", Sys.getenv("TAR_PROJECT", "flu_hosp_prod")) == "flu_hosp_evaluation"

if (!g_evaluation_mode) {
  # Production: a single forecast for the current week. The generation date is
  # the as_of for the forecast. On our typical schedule it's today (a
  # Wednesday); for a delayed forecast it can be a Thursday. Used both for
  # stamping the data and for choosing the as_of when creating the forecast.
  g_forecast_generation_dates <- g_forecast_reference_date()
  # Usually the forecast_date equals the generation date, but it can be
  # overridden. It should be a Wednesday.
  g_forecast_dates <- round_date(g_forecast_generation_dates, "weeks", week_start = 3)
  # The forecast is actually for the Wednesday beforehand on these days.
  if (g_forecast_generation_dates %in% as.Date(c("2025-12-29"))) {
    g_forecast_dates <- as.Date("2025-12-24")
  }
} else {
  # Evaluation: replay production over historical forecast dates. Skips the
  # weekly report notebook (each week's report is preserved as an ASOF snapshot)
  # and instead builds the evaluation scoring notebook; submission CSVs are
  # written only for the final date.
  #
  # End of the replay window; pin FORECAST_REFERENCE_DATE for a reproducible
  # oracle capture (default Sys.Date()). Must be >= 2025-12-31 so the trailing
  # seq.Date()s stay non-empty and 1:1 aligned with g_forecast_dates.
  g_reference_date <- g_forecast_reference_date()
  g_forecast_generation_dates <- c(
    as.Date(c("2024-11-21", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
    seq.Date(as.Date("2025-01-08"), as.Date("2025-12-17"), by = 7L),
    as.Date(c("2025-12-29")),
    seq.Date(as.Date("2025-12-31"), g_reference_date, by = 7L)
  )
  # Every Wednesday since mid-Nov 2024.
  g_forecast_dates <- seq.Date(as.Date("2024-11-20"), g_reference_date, by = 7L)
  # Optional: keep only the last N dates for a fast partial evaluation
  # (REFACTOR.md oracle). Inert when unset. Both date vectors are 1:1 aligned,
  # so slice both by the same indices.
  g_evaluation_n_dates <- as.integer(Sys.getenv("EVALUATION_N_DATES", "0"))
  if (!is.na(g_evaluation_n_dates) && g_evaluation_n_dates > 0) {
    keep <- tail(seq_along(g_forecast_dates), g_evaluation_n_dates)
    g_forecast_dates <- g_forecast_dates[keep]
    g_forecast_generation_dates <- g_forecast_generation_dates[keep]
  }
}

# The forecaster set + per-signal config (forecaster fn, version_policy, exogenous,
# primary_source) live in flu_build_prod2_grid() (R/flu_assemble.R), which the
# forecast loop consumes. The ensemble stage only needs the list of ids.
g_forecaster_params_grid <- tibble(id = unique(flu_build_prod2_grid()$id))


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
    name = nhsn_archive_data,
    command = {
      get_nhsn_data_archive("flu")
    },
    cue = tar_cue("always")
  ),
  tar_target(
    name = nhsn_latest_data,
    command = {
      nhsn_archive_data %>%
        epix_as_of(min(g_forecast_reference_date(), nhsn_archive_data$versions_end)) %>%
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
  tar_target(
    name = nssp_latest_data,
    command = {
      nssp_archive_data %>%
        epix_as_of(min(g_forecast_reference_date(), nssp_archive_data$versions_end))
    }
  )
)


# ================================ FORECAST TARGETS ================================
# tar_map over the signal grid (REFACTOR.md Exp 4, option A): one forecast target per
# (forecaster x outcome_signal x date). flu_assemble builds honest per-signal input
# (no nhsn/nssp column/source spoof) and the flu2_* adapter runs it. Kept in tar_map
# (not a plain loop) so targets still provides per-cell caching, crew parallelism,
# and code-invalidation. tar_combine splits the branches back into
# forecast_{nhsn,nssp}_full by outcome_signal.
forecast_targets <- tar_map(
  values = tidyr::expand_grid(
    flu_build_prod2_grid(),
    tibble(
      forecast_date_int = g_forecast_dates,
      forecast_generation_date_int = g_forecast_generation_dates,
      forecast_date_chr = as.character(g_forecast_dates)
    )
  ),
  names = c("id", "outcome_signal", "forecast_date_chr"),
  tar_target(
    name = forecast,
    command = {
      # Seed from the semantic cell key, not the target name. `targets` otherwise
      # derives each target's seed from its name, so renaming a target silently
      # moves the forecast for the stochastic forecasters (linear, cdc_baseline,
      # linear_no_population_scale). Seeded here rather than inside the
      # forecasters because only the harness knows (signal, date, ahead), and so
      # the set of stochastic forecasters doesn't have to be tracked by hand.
      set.seed(targets::tar_seed_create(
        paste(id, outcome_signal, forecast_date_chr, aheads, sep = "/")
      ))
      forecaster_fn <- get(forecaster)
      flu_assemble(
        archives = list(
          nhsn = nhsn_archive_data,
          nssp = nssp_archive_data,
          joined_latest_extra_data = joined_latest_extra_data,
          flu_data_substitutions = flu_data_substitutions
        ),
        outcome_signal = outcome_signal,
        exogenous = exogenous,
        version_policy = version_policy,
        generation_date = forecast_generation_date_int,
        forecast_date = forecast_date_int,
        insufficient_data_geos = g_insufficient_data_geos
      ) %>%
        forecaster_fn(ahead = aheads, extra_sources = exogenous, primary_source = primary_source) %>%
        mutate(
          forecaster = id,
          outcome_signal = outcome_signal,
          geo_value = as.factor(geo_value)
        )
    },
    pattern = map(aheads)
  )
)

combined_forecast_targets <- list(
  tar_combine(
    name = forecast_nhsn_full,
    forecast_targets[["forecast"]],
    command = dplyr::bind_rows(!!!.x) %>%
      dplyr::filter(outcome_signal == "nhsn") %>%
      dplyr::select(-outcome_signal)
  ),
  tar_combine(
    name = forecast_nssp_full,
    forecast_targets[["forecast"]],
    command = dplyr::bind_rows(!!!.x) %>%
      dplyr::filter(outcome_signal == "nssp") %>%
      dplyr::select(-outcome_signal)
  )
)


# ================================ ENSEMBLE TARGETS ================================
ensemble_targets <- tar_map(
  values = tibble(
    forecast_date_int = g_forecast_dates,
    forecast_generation_date_int = g_forecast_generation_dates,
    forecast_date_chr = as.character(g_forecast_dates)
  ),
  names = "forecast_date_chr",
  tar_target(
    name = forecast_filtered,
    command = list(
      nhsn = forecast_nhsn_full %>%
        filter(forecast_date == as.Date(forecast_date_int)) %>%
        filter(forecaster %nin% c("linear_no_population_scale")),
      nssp = forecast_nssp_full %>%
        filter(forecast_date == as.Date(forecast_date_int)) %>%
        filter(forecaster %nin% c("linear"))
    )
  ),
  tar_target(
    name = geo_weights,
    command = flu_geo_weights(flu_geo_exclusions, flu_nssp_geo_exclusions, forecast_date_int)
  ),
  tar_target(
    name = geo_exclusions,
    command = exclude_geos(geo_weights$nhsn)
  ),
  tar_target(
    name = ensemble_clim_lin,
    command = flu_ensemble_clim_lin(forecast_filtered, aheads, geo_weights, geo_exclusions)
  ),
  tar_target(
    name = ens_ar_only,
    command = flu_ens_ar_only(forecast_filtered)
  ),
  tar_target(
    name = ensemble_mixture,
    command = flu_ensemble_mixture(forecast_filtered, ensemble_clim_lin, geo_weights)
  ),
  tar_target(
    name = forecasts_and_ensembles,
    command = list(
      nhsn = bind_rows(forecast_filtered$nhsn, ensemble_clim_lin$nhsn, ensemble_mixture$nhsn, ens_ar_only),
      nssp = bind_rows(forecast_filtered$nssp, ensemble_clim_lin$nssp, ensemble_mixture$nssp)
    )
  ),
  # Submission/validation write every production run, but in evaluation mode only
  # on the final forecast date.
  tar_target(
    name = make_submission_csv,
    command = if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
      forecast_reference_date <- get_forecast_reference_date(forecast_date_int)
      nhsn_submission <- ensemble_mixture$nhsn %>%
        format_flusight(disease = "flu")
      nssp_submission <- ensemble_mixture$nssp %>%
        format_flusight(disease = "flu") %>%
        mutate(
          target = flu_report_target("nssp"),
          value = value * flu_report_scale("nssp")
        )
      bind_rows(nhsn_submission, nssp_submission) %>%
        write_submission_file(
          forecast_reference_date,
          file.path(g_submission_directory, "model-output/CMU-TimeSeries")
        )
    } else {
      cli_alert_info("Not writing submission (cache dir or non-final evaluation date)")
    },
    cue = tar_cue("always")
  ),
  tar_target(
    name = make_climate_submission_csv,
    command = if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
      forecast_filtered$nhsn %>%
        filter(forecaster %in% c("climate_base", "climate_geo_agged")) %>%
        group_by(geo_value, target_end_date, quantile) %>%
        summarize(forecast_date = as.Date(forecast_date_int), value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        ungroup() %>%
        filter(!(geo_value %in% g_excluded_geos)) %>%
        format_flusight(disease = "flu") %>%
        filter(location %nin% c("60", "66", "78")) %>%
        write_submission_file(
          get_forecast_reference_date(forecast_date_int),
          file.path(g_submission_directory, "model-output/CMU-climate_baseline"),
          file_name = "CMU-climate_baseline"
        )
    } else {
      cli_alert_info("Not writing climate submission (cache dir or non-final evaluation date)")
    },
    cue = tar_cue("always")
  ),
  tar_target(
    name = validate_result,
    command = {
      make_submission_csv
      if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
        validate_submission(
          g_submission_directory,
          file_path = sprintf("CMU-TimeSeries/%s-CMU-TimeSeries.csv", get_forecast_reference_date(forecast_date_int))
        )
      } else {
        "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
    }
  ),
  tar_target(
    name = validate_climate_result,
    command = {
      make_climate_submission_csv
      if (g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))) {
        validate_submission(
          g_submission_directory,
          file_path = sprintf(
            "CMU-climate_baseline/%s-CMU-climate_baseline.csv",
            get_forecast_reference_date(forecast_date_int)
          )
        )
      } else {
        "not validating when there is no hub (set SUBMISSION_DIRECTORY)"
      }
    }
  ),
  tar_target(
    name = truth_data,
    command = flu_truth_data(nhsn_archive_data, nhsn_latest_data, nssp_latest_data, forecast_generation_date_int)
  ),
  # The weekly report notebook is production-only: in evaluation mode each week's
  # report is preserved as an ASOF snapshot, and the scoring notebook takes over.
  tar_target(
    notebook,
    command = if (!g_evaluation_mode) {
      if (!dir.exists(here::here("reports"))) dir.create(here::here("reports"))
      rmarkdown::render(
        forecast_report_rmd,
        output_file = here::here(
          "reports",
          sprintf("%s_flu_prod_on_%s.html", as.Date(forecast_date_int), as.Date(Sys.Date()))
        ),
        params = list(
          disease = "flu",
          forecast_nhsn = forecasts_and_ensembles$nhsn %>% ungroup() %>% filter(forecaster %in% c("cdc_baseline", "climate_linear", "ensemble_mix", "windowed_seasonal", "windowed_seasonal_extra_sources")),
          forecast_nssp = forecasts_and_ensembles$nssp %>% ungroup() %>% filter(forecaster %in% c("cdc_baseline", "climate_linear", "ensemble_mix", "windowed_seasonal", "windowed_seasonal_extra_sources")),
          forecast_date = as.Date(forecast_date_int),
          truth_data_nhsn = truth_data$nhsn,
          truth_data_nssp = truth_data$nssp
        )
      )
    }
  )
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
      score_forecasts(nhsn_latest_data, local_forecasts_and_ensembles_nhsn, flu_report_target("nhsn"))
    }
  ),
  tar_target(
    name = local_scores_nssp,
    command = {
      nssp_latest_data %>%
        rename(value = nssp) %>%
        mutate(time_value = ceiling_date(time_value, unit = "week") - 1) %>%
        score_forecasts(local_forecasts_and_ensembles_nssp %>% mutate(value = value * flu_report_scale("nssp")), flu_report_target("nssp"))
    }
  ),
  tar_combine(
    name = joined_forecasts_and_ensembles_nhsn,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = filter_shared_geo_dates(
      purrr::map(list(!!!.x), "nhsn") %>% dplyr::bind_rows(),
      external_forecasts_full %>% filter(target == flu_report_target("nhsn")) %>% select(-target),
      min_locations = 52,
      min_dates = 40
    )
  ),
  tar_combine(
    name = joined_forecasts_and_ensembles_nssp,
    ensemble_targets[["forecasts_and_ensembles"]],
    command = filter_shared_geo_dates(
      purrr::map(list(!!!.x), "nssp") %>% dplyr::bind_rows(),
      external_forecasts_full %>% filter(target == flu_report_target("nssp")) %>% select(-target) %>% mutate(value = value / flu_report_scale("nssp")),
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
  combined_forecast_targets,
  ensemble_targets,
  external_forecast_targets,
  combined_targets,
  joined_targets,
  score_notebook
)
