# R/targets/

`targets` pipeline factory functions and runners. Nothing here is called directly by forecasters — it's the infrastructure that wires them into the pipeline.

## Files

- **forecaster_runner.R** — `run_forecaster`: cross-cutting conventions (ahead scaling, source filtering, extra-data join, target-date shift, geo exclusions, id stamping, output validation). Called per (forecaster, date) branch.
- **ensemble_runner.R** — `run_ensemble`: component presence assertion, method dispatch (`climate_linear` / `mean` / `weighted`), geo exclusion, id stamping, output validation.
- **prod_shared.R** — `build_prod_ensemble_targets`: declarative ensemble target factory for flu and covid prod pipelines.
- **flu_data_targets.R**, **covid_data_targets.R** — Disease-specific archive and data target factories (`nhsn_prod_archive`, `nssp_target_archive`, etc.).
- **flu_external_targets.R**, **covid_external_targets.R** — Targets that fetch external (hub) forecasts for comparison.
- **flu_forecaster_config.R**, **covid_forecaster_config.R** — Per-disease `g_forecaster_parameter_combinations` and `g_forecaster_params_grid` definitions.
- **score_targets.R** — Evaluation target factories (WIS scoring, coverage).
- **shared_utils.R** — Utilities shared across disease-specific target files.

## Key invariants

- `tar_map(values = grid_row)` substitutes each row as a literal — never dereference a grid global at branch run time.
- `tidy_eval = FALSE` defers `!!!params` splicing; `rlang::syms` on the trainer column is load-bearing.
- Prod and explore fan-out strategies deliberately differ: prod is `tar_map` per (forecaster, date); explore batches dates inside one target per forecaster via the slide cache.
