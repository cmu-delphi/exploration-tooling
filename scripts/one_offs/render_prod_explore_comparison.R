#!/usr/bin/env Rscript
# Render prod-explore-comparison.Rmd from cached explore and prod stores.
# Reads joined_scores/joined_forecasts from {disease}_hosp_explore and
# scores_nhsn/local_forecasts_and_ensembles_nhsn from {disease}_hosp_prod
# without running either pipeline.
#
# Usage:
#   Rscript scripts/render_prod_explore_comparison.R
#   Rscript scripts/render_prod_explore_comparison.R flu   # flu variant

suppressPackageStartupMessages(source("R/load_all.R"))

disease <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(disease)) disease <- "covid"

explore_store <- paste0(disease, "_hosp_explore")
prod_store <- paste0(disease, "_hosp_prod")
reports_dir <- "reports"

# Explore families to include (best-in-class from each).
explore_families_to_include <- c(
  "scaled_pop_exogenous", "revision_aware", "revision_aware_nssp",
  "revision_aware_beds_no_season", "revision_aware_beds_seasonal"
)

prod_forecaster_ids <- c(
  "windowed_seasonal", "windowed_seasonal_extra_sources",
  "ensemble_mix", "ens_ar_only", "climate_linear", "CMU-TimeSeries"
)

# ---- Load explore scores and forecasts -----------------------------------
# Multiple slugs may exist from different explore runs; pick the one with the
# earliest start year to get the widest date coverage and avoid duplicates.
explore_meta <- targets::tar_meta(store = explore_store, fields = "name") %>%
  filter(stringr::str_starts(name, "joined_scores_"))
if (nrow(explore_meta) == 0) {
  cli::cli_abort("No joined_scores_* targets found in {explore_store}. Run make explore-{disease} first.")
}
best_slug <- explore_meta %>%
  mutate(slug = stringr::str_remove(name, "^joined_scores_")) %>%
  slice_min(slug, n = 1) %>%
  pull(slug)

explore_scores <- targets::tar_read_raw(paste0("joined_scores_", best_slug), store = explore_store)
explore_forecasts <- targets::tar_read_raw(paste0("joined_forecasts_", best_slug), store = explore_store) %>%
  rename(value = prediction)

outside_forecasters <- targets::tar_read(outside_forecaster_subset, store = explore_store)

# ---- Load prod scores and forecasts --------------------------------------
read_prod <- function(target_name) {
  tryCatch(
    targets::tar_read_raw(target_name, store = prod_store),
    error = function(e) {
      cli::cli_warn("Could not read {target_name} from {prod_store}: {conditionMessage(e)}")
      tibble::tibble()
    }
  )
}

prod_scores <- read_prod("scores_nhsn") %>%
  { if (nrow(.) > 0) rename(., ae = ae_median, coverage_50 = interval_coverage_50, coverage_90 = interval_coverage_90) else . } %>%
  select(-any_of("target")) %>%
  filter(forecaster %in% prod_forecaster_ids)

prod_forecasts <- read_prod("local_forecasts_and_ensembles_nhsn") %>%
  select(-any_of(c("ahead", "source"))) %>%
  filter(forecaster %in% prod_forecaster_ids)

truth_data <- read_prod("nhsn_latest_data") %>%
  rename(target_end_date = time_value, true_value = value)

# ---- Select best-in-class explore forecasters, relabeled by family -------
g_aheads <- 0:4 * 7
g_dummy_mode <- FALSE
g_very_latent_locations <- list(list(c("source"), c("flusurv", "ILI+")))
forecaster_params <- if (disease == "covid") {
  get_covid_forecaster_params()
} else {
  get_flu_forecaster_params()
}
id_to_family <- purrr::imap(forecaster_params, \(x, idx) {
  x %>% select(id) %>% mutate(family = idx)
}) %>%
  dplyr::bind_rows() %>%
  filter(family %in% explore_families_to_include)

# Pick the lowest mean-WIS ID per family, then rename it to the family name.
best_in_class <- explore_scores %>%
  summarize(mean_wis = mean(wis, na.rm = TRUE), .by = forecaster) %>%
  inner_join(id_to_family, by = join_by(forecaster == id)) %>%
  slice_min(mean_wis, by = family, with_ties = FALSE) %>%
  select(forecaster, family)

# ---- Combine and relabel -------------------------------------------------
relabel <- function(df, id_col = "forecaster") {
  df %>%
    left_join(best_in_class, by = setNames("forecaster", id_col)) %>%
    mutate({{ id_col }} := if_else(!is.na(family), family, .data[[id_col]])) %>%
    select(-family)
}

explore_scores_subset <- explore_scores %>%
  filter(forecaster %in% best_in_class$forecaster) %>%
  relabel()

explore_forecasts_subset <- explore_forecasts %>%
  filter(forecaster %in% best_in_class$forecaster) %>%
  relabel()

combined_scores <- dplyr::bind_rows(
  explore_scores_subset,
  explore_scores %>% filter(forecaster %in% outside_forecasters),
  prod_scores
) %>%
  mutate(season_slug = season_of_date(forecast_date))

combined_forecasts <- dplyr::bind_rows(
  explore_forecasts_subset,
  explore_forecasts %>% filter(forecaster %in% outside_forecasters),
  prod_forecasts
) %>%
  mutate(season_slug = season_of_date(forecast_date))

# For flu, restrict to forecast dates where the baseline actually submitted.
# Flu external forecasters only cover the active season, so scoring Delphi
# models over the full calendar year inflates their relative WIS vs baseline.
# COVID forecasters run year-round so no restriction is needed there.
if (disease == "flu") {
  base_forecaster_name <- "FluSight-baseline"
  active_season_dates <- combined_scores %>%
    filter(forecaster == base_forecaster_name) %>%
    pull(forecast_date) %>%
    unique()
  cli::cli_inform(
    "Flu: restricting to {length(active_season_dates)} active-season forecast dates \\
    (where {base_forecaster_name} submitted)."
  )
  combined_scores <- combined_scores %>% filter(forecast_date %in% active_season_dates)
  combined_forecasts <- combined_forecasts %>% filter(forecast_date %in% active_season_dates)
}

# ---- As-of data snapshots ------------------------------------------------
# Pull the NHSN data as it appeared on each forecast date so fan plots can
# show the vintage the forecaster actually saw alongside its predictions.
asof_archive <- tryCatch(
  targets::tar_read(nhsn_prod_archive, store = prod_store),
  error = function(e) {
    cli::cli_warn("Could not read nhsn_prod_archive: {conditionMessage(e)}")
    NULL
  }
)

asof_data <- if (!is.null(asof_archive)) {
  fc_dates_all <- sort(unique(combined_forecasts$forecast_date))
  arch_versions <- asof_archive$DT$version
  valid_dates <- fc_dates_all[
    fc_dates_all >= min(arch_versions) & fc_dates_all <= max(arch_versions)
  ]
  cli::cli_inform("Pulling {length(valid_dates)} as-of snapshots from archive...")
  purrr::map(valid_dates, function(dd) {
    epix_as_of(asof_archive, version = dd) %>%
      as_tibble() %>%
      mutate(forecast_date = dd) %>%
      select(geo_value, time_value, value, forecast_date)
  }) %>% dplyr::bind_rows()
} else {
  tibble::tibble()
}

# ---- Render --------------------------------------------------------------
for (slug in sort(unique(combined_scores$season_slug))) {
  out_file <- here::here(reports_dir, paste0(disease, "-prod-explore-comparison-", slug, ".html"))
  cli::cli_inform("Rendering {slug} -> {out_file}")
  rmarkdown::render(
    "reports/writeups/prod-explore-comparison.Rmd",
    params = list(
      scores = combined_scores %>% filter(season_slug == slug) %>% select(-season_slug),
      forecasts = combined_forecasts %>% filter(season_slug == slug) %>% select(-season_slug),
      truth_data = truth_data,
      asof_data = asof_data,
      disease = disease
    ),
    output_file = out_file
  )
}
cli::cli_inform("Done.")
