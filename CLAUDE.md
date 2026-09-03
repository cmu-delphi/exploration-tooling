# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Delphi (CMU) epidemic forecasting monorepo for COVID, flu, and RSV hospitalization forecasts submitted to the CDC forecast hubs as "CMU-TimeSeries". It is organized as several [targets](https://docs.ropensci.org/targets/) pipeline projects (declared in `_targets.yaml`) sharing one R codebase, plus a report site deployed to Netlify (https://delphi-forecasting-reports.netlify.app/).

## Common commands

```sh
make install              # renv::restore() R dependencies (R 4.4.1)
make test                 # testthat::test_dir('tests/testthat')
make prod-flu             # run flu production pipeline (TAR_RUN_PROJECT=flu_hosp_prod)
make prod-covid           # covid production pipeline
make prod-rsv             # STUB: scripts/rsv_hosp_prod.R does not exist yet; recipe fails if run
make explore-flu          # flu exploration sweep (~3h)
make explore-covid        # covid exploration sweep (~3h)
make eval-flu             # flu historical replay + scoring (own project/store: flu_hosp_evaluation); EVALUATION_N_DATES=<n> limits to last n forecast dates. Also: eval-covid. BACKTEST_MODE survives only for the rsv stub (prod-rsv-backtest)
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

Key env vars: `TAR_PROJECT` (targets project selection; set via `Sys.setenv` in a REPL — never in `.Renviron`, which overrides the shell env on every Rscript start), `TAR_RUN_PROJECT` (how make recipes/`scripts/run.R` select the project, immune to `.Renviron`), `BACKTEST_MODE` (rsv stub only; flu/covid evaluation mode dispatches on the project name), `FORECAST_REFERENCE_DATE` (pins the pipeline's "today" for reproducible replays/captures), `DUMMY_MODE` (replace all forecasters with a dummy for pipeline testing), `EPIDATR_USE_CACHE`, `FLU/COVID/RSV_SUBMISSION_DIRECTORY`, `AUX_DATA_PATH`.

## Architecture

Each project in `_targets.yaml` maps a pipeline script to a store directory of the same name: `covid_hosp_explore`, `flu_hosp_explore`, `covid_hosp_prod`, `flu_hosp_prod`, `rsv_hosp_prod` (a stub — see below), plus `flu_hosp_evaluation` / `covid_hosp_evaluation` (same scripts as the prod projects, separate stores, for historical replays). Explore projects sweep many forecaster/parameter combinations to find good settings; prod projects generate the weekly submission and reports. Store directories (targets caches) are synced to/from S3 rather than recomputed.

- `scripts/<project>.R` — pipeline definitions. Globals are prefixed `g_` and must be top-level (targets freezes commands as expressions, so function arguments can't carry them). `g_forecast_dates` are the nominal (Wednesday) forecast dates; `g_forecast_generation_dates` are when forecasts actually ran (differ on holiday/outage delays) and serve as the data `as_of`.
- `g_forecaster_parameter_combinations` — human-readable tibble of forecasters × parameter settings; `g_forecaster_params_grid` is the same data reshaped for targets' dynamic branching. Each heading in the combinations tibble gets its own report notebook in `reports/`.
- `R/` — all shared code, sourced wholesale by `R/load_all.R` (imports in `R/imports.R`). Subdirs: `R/forecasters/` (forecaster functions), `R/targets/` (target factory/config code per disease), `R/new_epipredict_steps/`. Built on the Delphi stack: epiprocess/epipredict/epidatr, with `epi_df`/`epi_archive` data structures.
- `scripts/build_nhsn_archive.R`, `build_nssp_archive.R` — fast polling scripts that build versioned data archives; pipelines depend on these archives rather than fetching data themselves. Run every 5 min via systemd timers (see `deploy/systemd/README.md`; `scripts/run_prod_if_fresh.R` gates the Wednesday prod run on data freshness via `check_data_freshness()`).
- `scripts/*_geo_exclusions.csv` — per-date/geo forecaster ensemble weights, edited by hand to tune weekly submissions; `*_data_substitutions.csv` — manual data corrections.
- `scripts/reports/` — Rmd/qmd report sources rendered into `reports/` (the Netlify site).
- `aux_data/` — non-public input data, synced from S3.

Forecaster functions follow the signature `function(epi_data, outcome, ahead = 1, ...)` with `extra_sources` (exogenous columns) and `filter_source` (select source from a joined multi-source archive; `""` means use augmented data from all sources). Extra `...` args flow to `default_args_list` (epipredict training/prediction control). To add one, copy `R/forecasters/forecaster_scaled_pop.R`, register it in `g_forecaster_parameter_combinations`, and iterate with most other forecasters commented out and few forecast dates. See README.md "Adding a new forecaster".

## Shared forecaster & ensemble architecture

Principle: one declarative spec per forecaster (core function + parameter grid
+ spec metadata) consumed identically by exploration sweeps, backtesting, and
prod, so explore results transfer to prod verbatim and the prod copy of "the
same" forecaster can't silently drift from what exploration evaluated. Flu
prod, covid prod, and flu/covid explore are all on this stack. Three layers:

1. **Canonical archives**: version-independent munging (geo renames,
   Wednesday↔Saturday shift, season info, source stamping, folding in static
   historical extras) is applied once at archive construction
   (`nhsn_prod_archive`, `nssp_target_archive`), never per forecaster × date.
   Rule: normalize `time_value` at archive build, denormalize once
   post-forecast; never fake `version = time_value` for sources with real
   revisions.
2. **Shared snapshot**: `make_forecast_snapshot()` (`R/looping.R`) — archive +
   forecast/generation dates + as-of policy (`"asof"` real-time vs
   `"cheating"` finalized-with-cutoff) + substitutions → `epi_df` with correct
   `as_of`/`other_keys` metadata, with a parquet snapshot cache.
3. **Shared runner + grid**: `run_forecaster()`
   (`R/targets/forecaster_runner.R`) owns cross-cutting conventions (ahead
   scaling, source filtering, extra-data join, target-date shift, geo
   exclusions, id stamping, quantile sorting). Conventions are declared as
   spec columns with defaults in `FORECASTER_SPEC_DEFAULTS` (`R/utils.R`),
   split from modeling params by the shared `make_forecaster_grid()`.

The prod ensemble layer mirrors this: `build_prod_ensemble_targets()`
(`R/targets/prod_shared.R`) builds both diseases' ensemble targets from a
declarative per-disease `g_ensemble_specs` (in `scripts/*_hosp_prod.R`;
per-disease asymmetries are spec fields or factory arguments, never forked
code), executed by `run_ensemble()` (`R/targets/ensemble_runner.R`): component
presence asserted loudly, method dispatch (`climate_linear`/`mean`/`weighted`),
geo-exclusion filtering, id stamping, output validation. The hand-edited
`scripts/*_geo_exclusions.csv` weights files are schema-validated inside
`parse_prod_weights()` (`R/utils.R`; retired-but-inert forecaster ids are
whitelisted via `LEGACY_PROD_WEIGHT_FORECASTER_IDS`).

Contracts guard the boundaries: `make_forecast_snapshot()` asserts version
faithfulness (no as-of row observed after the generation date), and
`validate_forecast_output()` at the end of `run_forecaster()`/`run_ensemble()`
asserts output shape (keys present, no NAs, non-negative, monotone quantiles).
Forecasters whose quantiles can cross (the `scaled_pop_seasonal` family) opt
into the `sort_quantiles` spec column; monotone-by-construction forecasters
stay unsorted so a crossing surfaces as an error.

Fan-out stays deliberately different — "share the cell, keep two fan-out
strategies": prod is `tar_map` per (forecaster, date) for caching/crew/seeds;
explore batches dates inside one target per forecaster via the slide cache.

Rsv prod is a **stub and not a priority**: every rsv reference (Makefile
recipes, the `_targets.yaml` entry, `RSV_SUBMISSION_DIRECTORY`) points at a
not-yet-written `scripts/rsv_hosp_prod.R` and will fail if run; write it
directly on the shared stack whenever it is picked up.

History lives in `notes/`: the annotated commit log
(`notes/2026-07-22-ds-refactor2-annotated-commit-log.md`) is the source of
truth for what was done and how it was verified; `notes/refactor-ideas.md`
holds all open threads and future refactor designs; the remaining dated
files are experiment records. CLAUDE.md describes only the current state.

## Refactoring practice

A behavior-preserving step succeeds when its golden diff
is empty — replay with `make eval-flu` (separate `flu_hosp_evaluation` store,
so replays can't invalidate the weekly prod cache; `EVALUATION_N_DATES=<n>`
limits scope) and compare via oracle captures (`scripts/oracle/capture.R`).
Never mix a refactor with a bug fix; the golden faithfully reproduces current
bugs. For pure code moves, diff `targets::tar_manifest()` between revisions —
byte-identical commands mean no cache invalidation. `targets` metaprogramming
gotchas: `tar_map(values=)` substituting each grid row as a literal is the
safe form (commands that dereference a grid *global* at run time make the
whole grid a dependency of every branch); `tar_target`'s default
`tidy_eval = TRUE` splices a bare `!!!params` at build time (set
`tidy_eval = FALSE` to defer); `rlang::syms` on the trainer column is
load-bearing.

## Conventions

- Testing focuses on utility functions; forecaster quality is assessed by inspecting results/reports, not unit tests.
- Formatting via `air` (`air.toml`); lint config in `.lintr`.
- `_local/` is user-level git-ignored scratch (rg skips it by default).

## Run conventions
Most runs of explore targets happen on a remote machine, and not locally
