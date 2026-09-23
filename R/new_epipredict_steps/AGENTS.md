# R/new_epipredict_steps/

Custom extensions to the `epipredict` recipe/frosting system.
All steps follow the standard `recipes` S3 pattern: constructor → `_new` → `prep.*` → `bake.*` → `print.*`.
All layers follow the epipredict frosting pattern: constructor → `_new` → `slather.*` → `print.*`.

## Files

- **step_training_window.R** — `step_epi_training_window`: limits training rows to the N most recent observations per key, with optional seasonal windowing.
- **step_epi_whitening.R** — `step_epi_whitening` + `layer_epi_coloring`: per-(source, geo_value) whitening (center, scale, optional nonlinear transform). Params learned in `prep`, applied in `bake`. The paired `layer_epi_coloring` reverses the transform on predictions by retrieving learned params via `workflows::extract_recipe`.
- **step_epi_rolling_stats.R** — `step_epi_rolling_stats`: adds `slide_{col}_m{w}` (rolling mean) and `slide_{col}_sd{w}` (rolling SD) columns. Delegates to `rolling_mean` / `rolling_sd` in `R/forecasters/data_transforms.R`.
- **step_epi_poly_coefs.R** — `step_epi_poly_coefs`: fits a degree-N polynomial to multiple trailing windows of one column and adds coefficient columns named `{window_name}_c{k}`. All windows computed in one `epi_slide` call. Delegates to `get_poly_coefs` in `R/forecasters/data_transforms.R`.

## Key relationships

- `step_epi_whitening` and `layer_epi_coloring` are always used as a pair.
- `step_epi_rolling_stats` and `step_epi_poly_coefs` must appear in the recipe **before** `arx_preprocess` so the new columns exist when `step_epi_lag` runs.
- `step_epi_whitening` must appear before `step_population_scaling` (whitening in original-count space, then scale to rates).
- `layer_epi_coloring` must appear in frosting **after** `layer_predict` / `layer_quantile_distn` / `layer_point_from_distn`.
- `calculate_whitening_params`, `data_whitening`, `data_coloring`, `rolling_mean`, `rolling_sd`, `get_poly_coefs`, `replicate_whitening_params` remain in `R/forecasters/data_transforms.R` and `revision_predictors.R` — the steps delegate to them.
