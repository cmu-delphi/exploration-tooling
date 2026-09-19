# R/

Shared R codebase for all pipeline projects. Sourced wholesale via `load_all.R` (recursive, no package build step).

## Subdirectories

- **forecasters/** — Forecaster implementations and modeling utilities. See `forecasters/CLAUDE.md`.
- **targets/** — `targets` pipeline factories and runners. See `targets/CLAUDE.md`.
- **new_epipredict_steps/** — Custom `epipredict` recipe steps (`step_training_window`).
- **data/** — Data acquisition and preparation (NHSN, NSSP, geo, time). See `data/CLAUDE.md`.
- **eval/** — Scoring and visualisation. See `eval/CLAUDE.md`.

## Top-level files

- **load_all.R** — Entry point: sources `imports.R` then all `.R` files recursively.
- **imports.R** — `library()` calls for all shared dependencies.
- **looping.R** — Sliding forecast loop: `slide_forecaster`, `epix_slide_simple`, `make_forecast_snapshot`, `make_forecast_archive_snapshot`.
- **forecaster_config.R** — Forecaster grid machinery: `forecaster_lookup`, `add_id`, `get_single_id`, `make_forecaster_grid`, `FORECASTER_SPEC_DEFAULTS`, `data_substitutions`.
- **ensemble_weights.R** — Prod weight CSV parsing and validation: `parse_prod_weights`, `validate_prod_weights_*`, `exclude_geos`, `LEGACY_PROD_WEIGHT_FORECASTER_IDS`.
- **submission.R** — Output writing: `write_submission_file`, `get_forecast_reference_date`, `update_site`.
- **utils.R** — General utilities: `%nin%`, `sort_by_quantile`, `get_targets_errors`, `retry_fn`, `validate_epi_data`, `get_unique`, `filter_shared_geo_dates`, `get_file_hash`.
