# Design scout: validate_forecast_snapshot() and epipredict attribute reads

Read-only investigation (agent, 2026-07-19) for the roadmap item "a
validate_model_frame()-style check at the snapshot input boundary, replacing
the raw attributes()<- metadata handling." Verified against the exact pinned
package SHAs in renv.lock (epiprocess 5d35361e, epipredict 5f5c470e).

## 1. What make_forecast_snapshot() actually does (R/looping.R:123-196)

Not a blanket attributes()<- replacement — exactly two fields inside the
`metadata` attribute; class, geo_type, time_type, decay_to_tibble untouched:

```r
attributes(snapshot)$metadata$other_keys <- other_keys                          # line 193
attributes(snapshot)$metadata$as_of <- min(forecast_date, archive$versions_end)  # line 194
```

Two different kinds of operation:

- **other_keys restore (193)** is defensive: the comment says
  data_substitutions() "drops the epi_df metadata." Empirically FALSE on the
  pinned epiprocess dev version — inner_join/anti_join/bind_rows/filter on an
  epi_df all preserve class and metadata via epiprocess's
  dplyr_reconstruct.epi_df / dplyr_row_slice.epi_df, and running
  data_substitutions() directly kept full metadata including other_keys. Stale
  defensive code — cheap insurance, not wrong.
- **as_of override (194)** is a deliberate POLICY, not metadata repair:
  epix_as_of() already stamps as_of = min(generation_date, versions_end)
  (epiprocess R/methods-epi_archive.R:131); this overwrites it with the
  NOMINAL forecast_date (Wednesday), which differs from generation_date on
  delayed runs — and it flows into epipredict's model fitting (below).

## 2. Does epipredict read these attributes? YES — as_of and other_keys.

**as_of is read at training time and drives model behavior**:

- epipredict::get_forecast_date() (R/utils-latency.R:36-56), called from
  prep.step_adjust_latency() (R/step_adjust_latency.R:277-283):
  `forecast_date <- attributes(new_data)$metadata$as_of` where new_data is the
  TRAINING data — the snapshot. This repo defaults adjust_latency =
  "extend_lags" (R/default_epipredict_args.R:12) and no forecaster passes
  fixed_latency, so this branch is live. The value seeds get_latency_table(),
  determining how far lags extend — the snapshot's as_of changes the trained
  data shape.
- bake.epi_recipe() (epipredict R/epi_recipe.R:562-580): at prediction time
  pulls `attr(new_data, "metadata")` and re-attaches via
  as_epi_df(new_data, as_of = meta$as_of, other_keys = meta$other_keys).
- get_forecast_date_in_layer() (utils-latency.R:156-177), used by
  layer_add_forecast_date/layer_add_target_date (arx_postprocess,
  R/forecasters/epipredict_utilities.R:81-82, passes neither explicitly):
  falls back to metadata$as_of only if the prepped recipe's forecast_date is
  NULL — secondary/edge-case read, but exists.

**other_keys is read** in bake.epi_recipe (epi_recipe.R:566-580),
epi_workflow (R/epi_workflow.R:102-103), and key_colnames() throughout.

**This repo also reads them directly**: data_validation.R:97
(`attr(epi_data, "metadata")$as_of + ahead`), data_transforms.R:23,112-120,
forecaster_smoothed_scaled.R:90 (same restore-after-transform pattern).

geo_type/time_type are read downstream (layer_add_forecast_date.R:101-105,
layer_add_target_date.R:111-115 use time_type from the fitted template) but
make_forecast_snapshot() never touches them.

## 3. Is swapping attributes()<- for as_epi_df() behavior-equivalent?

Mostly, with nuances:

- as_epi_df.tbl_df (epiprocess R/epi_df.R:236-303) re-guesses
  geo_type/time_type and re-validates ukey uniqueness (check_ukey_unique) —
  a superset of current behavior, not a change to those fields.
- The "as_of resets to Sys.Date()" risk only applies when as_of is OMITTED;
  passing it explicitly avoids that.
- REAL risk/feature: as_epi_df() cli_aborts on duplicate
  (geo_value, other_keys, time_value) rows that raw mutation silently passes
  through (surfacing later as confusing epi_recipe failures). Newly rejecting
  malformed snapshots is a behavior change — introduce it as an explicit,
  named invariant check, not an incidental constructor-swap side effect
  (never mix refactor and bug fix).
- Simpler alternative: keep the two attr assignments, add an explicit
  validator before them, skip as_epi_df()'s heavier re-derivation.

## 4. Recommended validate_forecast_snapshot() design

Run at the end of make_forecast_snapshot(), before returning:

1. inherits(snapshot, "epi_df") and is.list(attr(snapshot, "metadata")).
2. metadata$as_of scalar Date/POSIXt matching class(snapshot$time_value)
   (mirrors epipredict's own check, utils-latency.R:64-74);
   as_of >= max(time_value); no acausal as_of vs generation date.
3. metadata$other_keys character, all present as columns;
   check_ukey_unique(snapshot, c("geo_value", other_keys, "time_value")) —
   reuse epiprocess's exported check_ukey_unique (used inside as_epi_df,
   epi_df.R:296) so behavior tracks epiprocess.
4. Keep the existing time_value > generation_date leak check (lines 181-187)
   in the same boundary function (version-faithfulness contract).
5. Do NOT re-derive geo_type/time_type — never modified here; unnecessary
   surface area.

Epipredict exports no validate_model_frame() to reuse (grepped NAMESPACE and
R/). Closest analogues: epi_check_training_set()'s ad hoc other_keys check
(epipredict R/epi_check_training_set.R:5-23) and get_forecast_date()'s type
check — mirror their error messages, build on check_ukey_unique.
