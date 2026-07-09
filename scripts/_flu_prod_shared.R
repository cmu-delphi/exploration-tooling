# Shared config and target factory for the flu production + evaluation pipelines.
#
# Sourced by scripts/flu_hosp_prod.R (production, as_of = today) and
# scripts/flu_hosp_evaluation.R (historical replay). Each entry script sources
# R/load_all.R, sources this file, sets g_evaluation_mode and the forecast-date
# globals, then calls build_flu_prod_pipeline(). Keeping the target DAG here (not
# duplicated per entry) is the point of the split.


# ================================ GLOBALS =================================
# Variables prefixed with 'g_' are globals needed by the targets pipeline (they
# need to persist during the actual targets run, since the commands are frozen
# as expressions). The two mode-specific globals — g_evaluation_mode and the
# forecast-date vectors — are set by the entry scripts, not here.

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
# NOTE: g_evaluation_mode, g_forecast_dates, and g_forecast_generation_dates are
# set by the entry script (flu_hosp_prod.R or flu_hosp_evaluation.R) before it
# calls build_flu_prod_pipeline().

# The forecaster set + per-signal config (forecaster fn, version_policy, exogenous,
# primary_source) live in flu_build_prod2_grid() (R/flu_assemble.R), which the
# forecast loop consumes. The ensemble stage only needs the list of ids.
g_forecaster_params_grid <- tibble(id = unique(flu_build_prod2_grid()$id))


# Build the full flu pipeline target list. Reads the g_* globals (including the
# entry-set g_evaluation_mode and forecast-date vectors) from the global env, the
# same way the create_*/build_* factories do. Body intentionally left at file
# indentation to keep this split a behavior-preserving move (no reflow).
build_flu_prod_pipeline <- function() {
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
        epix_as_of(min(Sys.Date(), nhsn_archive_data$versions_end)) %>%
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
        epix_as_of(min(Sys.Date(), nssp_archive_data$versions_end))
    }
  )
)


# ================================ FORECAST TARGETS ================================
# tar_map over the signal grid (REFACTOR.md Exp 4, option A): one forecast target per
# (forecaster x outcome_signal x date). flu_assemble builds honest per-signal input
# (no nhsn/nssp column/source spoof) and the flu2_* adapter runs it. Kept in tar_map
# (not a plain loop) so targets still provides per-cell caching, crew parallelism,
# code-invalidation, and per-target seeds. tar_combine splits the branches back into
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
  tar_target(
    name = truth_data,
    command = flu_truth_data(nhsn_archive_data, nhsn_latest_data, nssp_latest_data, forecast_generation_date_int)
  ),
  # Mode-specific submission/validation/notebook tail (defined in R/flu_outputs.R),
  # spliced in with the g_evaluation_mode gating already resolved for this pipeline.
  flu_output_targets(g_evaluation_mode)
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
}
