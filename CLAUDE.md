# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Delphi (CMU) epidemic forecasting monorepo for COVID, flu, and RSV hospitalization forecasts submitted to the CDC forecast hubs as "CMU-TimeSeries". It is organized as several [targets](https://docs.ropensci.org/targets/) pipeline projects (declared in `_targets.yaml`) sharing one R codebase, plus a report site deployed to Netlify (https://delphi-forecasting-reports.netlify.app/).

Note: this is a Jujutsu (`.jj/`) colocated repo — use `jj` commands, not git.

## Common commands

```sh
make install              # renv::restore() R dependencies (R 4.4.1)
make test                 # testthat::test_dir('tests/testthat')
make prod-flu             # run flu production pipeline (TAR_RUN_PROJECT=flu_hosp_prod)
make prod-covid           # covid production pipeline
make prod-rsv             # rsv production pipeline
make explore-flu          # flu exploration sweep (~3h)
make explore-covid        # covid exploration sweep (~3h)
make eval-flu             # flu historical replay + scoring (own project/store: flu_hosp_evaluation); EVALUATION_N_DATES=<n> limits to last n dates. Covid/rsv still use BACKTEST_MODE=TRUE on the prod store (prod-covid-backtest, prod-rsv-backtest)
make pull / make push     # sync aux_data, targets stores, and forecasts with S3 (forecasting-team-data bucket)
make update-site && make netlify   # rebuild report index and deploy
make submit-flu           # commit forecast to ../FluSight-forecast-hub and open PR (also: submit-covid, submit-rsv, *-dry)
make get-flu-prod-errors  # show errors from the last pipeline run
```

Run a single test file: `Rscript -e "testthat::test_file('tests/testthat/test-forecaster-utils.R')"`.

REPL workflow (preferred for debugging):

```r
suppressPackageStartupMessages(source("R/load_all.R"))  # sources all of R/ — there is no package build step
Sys.setenv(TAR_PROJECT = "covid_hosp_prod")
tar_make()
# Debug one target: put browser() in its function, then
tar_make(target_name, callr_function = NULL, use_crew = FALSE)
get_targets_errors("covid_hosp_prod", top_n = 10)
forecaster_lookup("surprised.tarantula")  # map code name -> parameter settings
```

Key env vars (`.Renviron`): `TAR_PROJECT` (REPL default project), `TAR_RUN_PROJECT` (used by `scripts/run.R` because the shell overwrites `TAR_PROJECT`), `BACKTEST_MODE` (covid/rsv only; flu evaluation mode dispatches on the project name), `DUMMY_MODE` (replace all forecasters with a dummy for pipeline testing), `EPIDATR_USE_CACHE`, `FLU/COVID/RSV_SUBMISSION_DIRECTORY`, `AUX_DATA_PATH`.

## Architecture

Each project in `_targets.yaml` maps a pipeline script to a store directory of the same name: `covid_hosp_explore`, `flu_hosp_explore`, `covid_hosp_prod`, `flu_hosp_prod`, `rsv_hosp_prod`. Explore projects sweep many forecaster/parameter combinations to find good settings; prod projects generate the weekly submission and reports. Store directories (targets caches) are synced to/from S3 rather than recomputed.

- `scripts/<project>.R` — pipeline definitions. Globals are prefixed `g_` and must be top-level (targets freezes commands as expressions, so function arguments can't carry them). `g_forecast_dates` are the nominal (Wednesday) forecast dates; `g_forecast_generation_dates` are when forecasts actually ran (differ on holiday/outage delays) and serve as the data `as_of`.
- `g_forecaster_parameter_combinations` — human-readable tibble of forecasters × parameter settings; `g_forecaster_params_grid` is the same data reshaped for targets' dynamic branching. Each heading in the combinations tibble gets its own report notebook in `reports/`.
- `R/` — all shared code, sourced wholesale by `R/load_all.R` (imports in `R/imports.R`). Subdirs: `R/forecasters/` (forecaster functions), `R/targets/` (target factory/config code per disease), `R/new_epipredict_steps/`. Built on the Delphi stack: epiprocess/epipredict/epidatr, with `epi_df`/`epi_archive` data structures.
- `scripts/build_nhsn_archive.R`, `build_nssp_archive.R` — fast polling scripts that build versioned data archives; pipelines depend on these archives rather than fetching data themselves. Run every 5 min via systemd timers (see `deploy/systemd/README.md`; `scripts/run_prod_if_fresh.R` gates the Wednesday prod run on data freshness via `check_data_freshness()`).
- `scripts/*_geo_exclusions.csv` — per-date/geo forecaster ensemble weights, edited by hand to tune weekly submissions; `*_data_substitutions.csv` — manual data corrections.
- `scripts/reports/` — Rmd/qmd report sources rendered into `reports/` (the Netlify site).
- `aux_data/` — non-public input data, synced from S3.

Forecaster functions follow the signature `function(epi_data, outcome, ahead = 1, ...)` with `extra_sources` (exogenous columns) and `filter_source` (select source from a joined multi-source archive; `""` means use augmented data from all sources). Extra `...` args flow to `default_args_list` (epipredict training/prediction control). To add one, copy `R/forecasters/forecaster_scaled_pop.R`, register it in `g_forecaster_parameter_combinations`, and iterate with most other forecasters commented out and few forecast dates. See README.md "Adding a new forecaster".

## Conventions

- Testing focuses on utility functions; forecaster quality is assessed by inspecting results/reports, not unit tests.
- Formatting via `air` (`air.toml`); lint config in `.lintr`.
- `_local/` is user-level git-ignored scratch (rg skips it by default).
