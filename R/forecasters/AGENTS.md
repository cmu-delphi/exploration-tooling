# R/forecasters/

Forecaster implementations and shared modeling utilities.
All forecasters follow the signature `function(epi_data, outcome, ahead = 1, ...)`.

## Files

- **forecaster_scaled_pop.R** — `scaled_pop`: ARX with population scaling.
- **forecaster_scaled_pop_seasonal.R** — `scaled_pop_seasonal`: adds seasonal whitening, climate baseline, PCA, window methods.
- **forecaster_smoothed_scaled.R** — `smoothed_scaled`: smoothed + scaled variant.
- **forecaster_revision_aware.R** — `scaled_pop_seasonal_revision`: revision-aware forecaster (takes `epi_archive`); `flag_revision_outlier_versions`, `compute_finalization_lag_weeks`.
- **revision_predictors.R** — Revision design matrix: `archive_to_revision_predictors`, `revision_predictor_design`, `roll_asof_value` (data.table rolling joins), `replicate_whitening_params`.
- **forecaster_baseline_linear.R** — `cdc_baseline_linear`: CDC-style linear baseline.
- **forecaster_climatological.R** — `climatological`: seasonal median baseline.
- **forecaster_flusion.R** — `flusion`: Flusion ensemble replication.
- **forecaster_flatline.R** — `flatline_fc`: naive flatline.
- **forecaster_no_recent_outcome.R** — Variant without recent outcome signal.
- **forecaster_dummy.R** — `dummy_forecaster`: pipeline smoke-test stub.
- **climatological_model.R** — `climate_median`, `compute_pca`: seasonal baseline computations shared by several forecasters.
- **data_transforms.R** — Feature extraction (`get_trainable_names`), rolling stats, whitening/coloring (`calculate_whitening_params`, `data_whitening`, `data_coloring`), polynomial features.
- **data_validation.R** — `validate_forecast_output` and related shape/key checks.
- **epipredict_utilities.R** — Helpers that wrap epipredict internals.
- **formatters.R** — Output formatting to hub submission format.
- **ensemble_average.R**, **ensemble_linear_climate.R** — Ensemble combination methods.
- **default_epipredict_args.R** — `default_args_list`, `default_flatline_args`: ARX / flatline parameter constructors.

## Inter-file dependencies

`forecaster_revision_aware.R` calls `archive_to_revision_predictors` from `revision_predictors.R` and `calculate_whitening_params` / `data_whitening` / `data_coloring` from `data_transforms.R`.
`scaled_pop_seasonal` and friends call `climate_median` / `compute_pca` from `climatological_model.R`.
