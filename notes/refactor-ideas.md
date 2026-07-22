# Open threads and future refactor ideas

The single home for everything not yet implemented. Consolidated 2026-07-22
from CLAUDE.md's "Open threads" and the three 2026-07-19 design/cleanup
scouts (`e2-explore-ensemble-design`, `validate-snapshot-design`,
`clarity-simplification-scout` — originals in git history); items the
2026-07-22 `clean:` commits already executed have been removed.

Rough priority order:

1. **E2 — sweep the shipped prod ensembles in explore** (highest-value;
   deferred by decision, full design below).
2. **Snapshot input validator** (`validate_forecast_snapshot()`, design
   below).
3. **Clarity/simplification sweep** (verified inventory below; the first
   deletions landed 2026-07-22).
4. **Data-layer / archive-stack consolidation** (scouted 2026-07-22; items
   1–3 are refactors, item 4 is a data-migration project).
5. Smaller threads (last section): per-forecaster `output_scale`, per-source
   version policies, BLAS pinning, the rsv prod stub.

Cache-invalidation notes follow the repo discipline: "no command-text
change" means `tar_manifest()` diffs empty; function-*body* changes still
invalidate targets whose commands reference the function symbol (targets
hashes global functions), so those need a golden replay even when the
manifest is clean.

---

## E2: make the shipped prod ensembles sweepable in explore

Status: design only, deferred (decided 2026-07-19). The highest-value open
item. Read-only design scout so it can be picked up later.

### The gap

Prod ships `ensemble_mix` (climate_linear + weighted-AR blend, built by
`run_ensemble()` from `g_ensemble_specs` in `scripts/{flu,covid}_hosp_prod.R`,
post E0/E1). Exploration never evaluates that code path. Explore only sweeps a
monolithic `climate_linear_ensembled` (`R/forecasters/forecaster_climatological.R`)
that re-implements the blend internally. So the submitted ensemble is never
compared against alternatives with evidence, and the two implementations can
drift. `run_ensemble` has zero references outside `R/targets/ensemble_runner.R`
and `R/targets/prod_shared.R`.

Two structural mismatches to design around:

1. **Explore has no atomic prod-component forecasters.** Prod's spec composes
   named component ids (`climate_base`, `climate_geo_agged`, `linear`,
   `linear_no_population_scale`, `windowed_seasonal`,
   `windowed_seasonal_extra_sources`) that exist as prod grid rows. Explore has
   none of them standalone (only `climate_linear_ensembled` hash variants and a
   differently-tuned `scaled_pop_seasonal` family). So E2 step one is "give
   explore the atomic components to sweep the ensemble *from*."
2. **Fan-out shape.** Prod is `tar_map` per (ensemble, signal, date); explore is
   one target per forecaster that internally loops `map(forecast_dates)` and
   `bind_rows()` (`create_forecast_targets()`, `R/targets/shared_utils.R`). The
   explore ensemble command must loop dates internally and call `run_ensemble()`
   once per date — required for correctness, see the latency bug below.

### Correctness landmine

`ensemble_climate_linear()` computes `last_data <- min(forecasts$target_end_date)`
and `forecast_date <- min(forecasts$forecast_date)` over the *whole* frame it is
given (`R/forecasters/ensemble_linear_climate.R:24-26`). Called on a multi-date
frame it collapses every date's latency to the earliest date's — silently wrong.
`weighted`/`mean` methods key on `forecast_date`/`ahead` explicitly and are safe,
but to stay faithful the explore ensemble command should call `run_ensemble()`
per date regardless of method. (Mirrors explore's existing per-date loop, so no
new idiom.)

### Proposed structure

**Atomic components family.** Add `prod_ensemble_components` to
`get_{flu,covid}_forecaster_params()`, copied verbatim from the prod grid
rows. Two traps:

- **Do NOT run it through `add_id()`** — every other family calls `add_id()`
  which overwrites `id` with a param hash; `run_ensemble()` matches components
  by literal name. Special-case it or bind it in after the `map()`.
- **`output_scale = "count"`, not `"per100k"`** — the flu config sets
  `output_scale <- "per100k"` unconditionally for every family, but these
  components are `pop_scaling = FALSE` like prod (`FORECASTER_SPEC_DEFAULTS`
  is `"count"`). Wrong value = scores off by ~pop/1e5.

Adding rows to `g_forecaster_params_grid` (evaluated once at graph-build, not in
a frozen command) is purely additive: existing `tar_map` branches' literal
values are untouched, so existing `forecast_*`/`score_*` don't recompute. Adding
the family to `g_forecaster_parameter_combinations` auto-generates its per-family
notebook with no notebook-loop code change.

**Ensemble sweep grid + targets.** New file
`R/targets/explore_ensemble_targets.R` with
`build_explore_ensemble_targets(...)`, called from the explore scripts after
`create_forecast_targets()`. Key decisions:

- Grid should be **one row per full `g_ensemble_specs`-shaped variant** (weights
  source × climate caps × component set), each row internally the 3-stage
  prod shape (climate_linear → ar_only → weighted-mix-with-`extra_forecasts`),
  NOT a flat one-row-per-method tibble. Reason: prod's `ensemble_mix` feeds
  `ensemble_clim_lin`'s *output* into `ensemble_weighted()` as `extra_forecasts`
  — the mix is climate_linear + weighted-AR, so a faithful sweep must reproduce
  that dependency order.
- `component_forecasts_for_ensemble`: one plain `tar_target` binding the six
  literal-id `forecast_<id>` targets by symbol; shared input to every sweep
  branch. Filter to one `forecast_date` inside the branch before `run_ensemble()`.
- `ensemble_forecast` sweep `tar_map(values = grid, names = id)`; `ensemble_score`
  duplicates (does NOT edit) the ~15-line scoring body from
  `create_forecast_targets()` so the existing `score` target's frozen command is
  untouched; `tar_combine` into `ensemble_forecasts`/`ensemble_scores`.

**Weights.**

- Prod's per-date hand-edited CSVs (`parse_prod_weights`) are forward-only and
  tolerate stale/typo'd historical blocks (E3 finding) — wrong as a sweep
  default. Honest default: **uniform weights** (`weight = 1` per component × geo,
  same `(forecast_date, forecaster, geo_value, weight)` shape run_ensemble wants),
  built from explore's existing `state_geo_values` target — NOT via
  `parse_prod_weights()` (it does a live GitHub fetch per call).
- Offer prod CSV weights as one swept config (`weights_id = "prod_csv"`) to
  answer "does hand-tuning actually help historically vs uniform" — the core
  question E2 exists for. Under that variant also wire the `exclude_geos()`
  equivalent so it's a faithful prod replay.

**Report wiring.** Extend `joined_forecasts`/`joined_scores`
(`R/targets/shared_utils.R`) to `bind_rows(ensemble_forecasts/scores)`. This
changes their command text so they + `overall_notebook` + per-family notebooks
recompute — cheap `bind_rows`/render, not the 3h sweep. `forecaster_lookup()`
unaffected (reads current grid).

### Scope boundaries

- **nhsn/hhs signal only.** Explore has no nssp-as-target archive/snapshot/scoring
  path (prod-only). Wiring nssp-as-target into explore is a materially bigger
  lift — named follow-up, do not half-implement.
- **`climate_linear_ensembled` retirement is out of scope.** Once E2 lands it and
  the new `prod_climate_linear*` sweep rows sit side-by-side in `overall_notebook`
  — exactly the comparison needed to later judge retirement safe.

### Invalidation summary (verify via tar_manifest diff, E0/E1 methodology)

- Expected changed (cheap): `forecaster_params_grid`, `joined_forecasts`,
  `joined_scores`, `overall_notebook`, per-family `notebook_*`.
- Expected byte-identical: every pre-existing `forecast_<id>`/`score_<id>`,
  `delphi_forecasts`, `delphi_scores`, all data/external targets.
- New: 6 component `forecast_*`/`score_*`, `component_forecasts_for_ensemble`,
  the sweep branches, `ensemble_forecasts`, `ensemble_scores`.

### Risks / open questions (ranked)

1. Should the 6 atomic components also feed the main `delphi_forecasts` ranking?
   Recommend no (keep them ensemble-inputs-only; visible via own family notebook
   + overall_notebook) — avoids touching `delphi_forecasts` command text.
2. `ensemble_mix` `extra_forecasts` chaining ⇒ grid must be full-spec-per-row
   (folded into the structure decision above).
3. nssp-as-target sweep out of scope (follow-up).
4. `climate_linear_ensembled` retirement out of scope (follow-up).
5. `assert_components_present()` fires if a sweep row names a component absent for
   a date — verify the 6 components share explore's exact `forecast_dates`/`aheads`
   and that `windowed_seasonal_extra_sources`'s `excluded_geos = c("mo","wy")`
   interacts correctly with `state_geo_values` filtering.
6. Spot-check `output_scale = "count"` magnitudes against `hhs_evaluation_data`
   on one date before trusting the full sweep.

### Files to touch when implemented

`R/targets/ensemble_runner.R`, `R/targets/prod_shared.R`,
`R/targets/shared_utils.R`, `R/targets/{flu,covid}_forecaster_config.R`,
`scripts/{flu,covid}_hosp_explore.R`, `R/utils.R` (parse_prod_weights, add_id,
make_forecaster_grid, FORECASTER_SPEC_DEFAULTS); source of truth for component
params + spec shape is `scripts/{flu,covid}_hosp_prod.R`.

---

## validate_forecast_snapshot(): explicit snapshot input validator

Status: design only. Read-only investigation (2026-07-19) for the roadmap
item "a validate_model_frame()-style check at the snapshot input boundary,
replacing the raw attributes()<- metadata handling." Verified against the
exact pinned package SHAs in renv.lock (epiprocess 5d35361e, epipredict
5f5c470e).

### 1. What make_forecast_snapshot() actually does (R/looping.R:123-196)

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

### 2. Does epipredict read these attributes? YES — as_of and other_keys.

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

### 3. Is swapping attributes()<- for as_epi_df() behavior-equivalent?

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

### 4. Recommended validate_forecast_snapshot() design

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

---

## Clarity/simplification sweep (remaining items)

From the 2026-07-19 scout; every item was verified by rg / reading the code /
running R before inclusion. The scout's H2 (dead `g_rsv_*` closures), H3
(dead-function purge across utils/scoring/aux-data/formatters), and H5
(broken `check_nssp_socrata_github_diff` chain, moved to
`scripts/one_offs/compare_nssp_sources.R`) landed 2026-07-22 (`ace2dad`,
`ea75903`) and are removed here. Line numbers predate those commits — re-rg
before acting.

### High

#### H1. Delete the three unused explore forecaster families (~630 lines)

- `R/forecasters/forecaster_flusion.R` (219 lines), `forecaster_smoothed_scaled.R`
  (251), `forecaster_no_recent_outcome.R` (163).
- None is registered in any live grid: rg for `"flusion"`, `"smoothed_scaled"`,
  `"no_recent_outcome"` over `R/targets/*config*.R` and `scripts/*_prod.R`
  hits only *commented-out* `no_recent_outcome` blocks
  (`flu_forecaster_config.R:136,156,183`). The `flusion` hits in live flu files
  are the `flusion_data_archive` *data* target, not the forecaster.
- Their tests are already half-dead: `tests/testthat/test-forecasters-basics.R:8-11`
  comments flusion out ("TODO: flusion is broken?") and no_recent_outcome
  ("cannot be run without aux_data"); lines 32,45 `skip()` the remaining
  no_recent_outcome/smoothed_scaled cases as broken. smoothed_scaled is
  otherwise test-only (`test-forecasters-data.R:12`, one one_off step-through).
- Why: 630 lines of self-labeled copypasta that every "shared prologue" grep
  matches, two of them known-broken, all maintained for nothing. Deleting them
  also shrinks H4 (the prologue extraction) to the four live forecasters.
- Change: delete the three files, their test rows/blocks, and the commented
  config blocks (recoverable from history). Keep `flusion_data_archive`.
- Risk: behavior-preserving; no live target references them, so no cache
  impact. `make test` confirms.

#### H4. Extract the still-duplicated forecaster prologue/epilogue

The runner hoisted the *outer* conventions; the inner ~50-line copypasta
survives in all forecaster bodies. Byte-identical (or near, with named deltas)
across `forecaster_scaled_pop.R`, `forecaster_scaled_pop_seasonal.R` (and the
H1 trio until deleted):

- `arg_match` trio (scaled_pop.R:64-66, seasonal.R:60-62, …)
- `validate_epi_data` + `unlist(extra_sources)` (scaled_pop.R:68-69, …)
- insufficient-data empty-tibble early return (scaled_pop.R:76-87, …)
- fake-`source` `adding_source` block (scaled_pop.R:89-94; seasonal.R:93-98
  differs only in using `primary_source` instead of literal `"nhsn"`)
- `default_args_list` assembly + `sanitize_args_predictors_trainer`
  (scaled_pop.R:95-102, …)
- whitening block (scaled_pop.R:108-116; seasonal passes
  `setdiff(predictors, "hhs_region")`)
- coloring epilogue incl. `pmax(0, value)` + `adding_source` unwind + `gc()`
  (scaled_pop.R:159-173; seasonal.R:255-272 gates pmax on `clip_lower`)

Change: `forecaster_prologue()` returning
`(epi_data, args_list, predictors, trainer, learned_params, adding_source)`
and `finalize_forecast(pred, ..., clip_lower = TRUE)`; deltas become
arguments. Do H1 first so only the four live forecasters need porting.

Risk: needs-golden-replay. Command text is unchanged, but the forecaster
symbols (`scaled_pop` etc.) are spliced into commands via `rlang::syms`, so
targets hashes their bodies — every forecast target recomputes. Replay
`make eval-flu` / `eval-covid` against oracle captures.

### Medium

#### M1. `filter_shared_geo_dates` has a permanently-dead else-branch

- `R/utils.R:947`: `local_forecasts %>% distinct(forecast_date) %>% length() == 1`
  — `length()` of a one-column tibble is its column count, i.e. always 1
  (verified in R: two distinct dates still give `length() == 1`). The comment
  says "the length is one if we're forecasting this week", but the else branch
  (joint local+external viability join, :952-960) has never run.
- Only callers: `scripts/flu_hosp_prod.R:510,520`
  (`joined_forecasts_and_ensembles_*`).
- Change: decide which behavior is intended. Deleting the dead branch (keep
  current behavior) is the behavior-preserving simplification; "fixing" to
  `nrow()` would change flu report inputs and must be its own adjudicated
  change, never mixed in. Also fix the `trucated_forecasters` typo while there.
- Risk: branch deletion is behavior-preserving but changes
  `filter_shared_geo_dates`'s body → invalidates the two report-side targets;
  cheap, golden optional. **Needs a behavior adjudication from the user before
  touching.**

#### M2. Quantile-sorting idiom exists in three places, one with a stale error hint

- `sort_by_quantile()` (`R/utils.R:631`), a manual pre-sort in
  `evaluate_predictions` (`R/scoring.R:4-9`), and another manual sort in
  `format_scoring_utils` (`formatters.R:92-95`).
- The scoring pre-sorts silently mask crossings — the opposite of the
  `validate_forecast_output` philosophy — but can't be removed outright:
  *external* forecasts (`covid/flu_external_targets.R`, `score_forecasts`)
  never pass through the validator and may legitimately cross. Local explore
  forecasts reaching `evaluate_predictions` are already validated monotone, so
  for them the sort is dead defensive code.
- `evaluate_predictions`'s abort hint (`scoring.R:50`: "if wis is missing,
  then likely quantile monotonicity was violated") is stale — it sorts five
  lines earlier, so monotonicity can no longer be the cause.
- Change: replace both manual sorts with `sort_by_quantile()` calls plus a
  one-line "external forecasts are unvalidated" comment; fix the hint.
- Risk: behavior-preserving (same sort); body change invalidates score
  targets → cheap golden (`EVALUATION_N_DATES=1`) or score-frame diff.

#### M3. `scripts/build_nhsn_archive.R` / `build_nssp_archive.R` share ~90 duplicated lines

- `sync_s3_to_local_cache` is identical modulo default `prefix`/
  `local_cache_path` values (nhsn:88-162 vs nssp:83-157), plus a verbatim
  header/preamble (1-36) and `run_time` block.
- Copy-paste bug: `build_nssp_archive.R:216` logs "Checking for updates to
  NHSN data..." inside the NSSP updater.
- Same concept, two names: nhsn `get_last_raw_update_at()` vs nssp
  `get_s3_updated_at()`.
- Change: hoist `sync_s3_to_local_cache` (and the shared preamble) into `R/`
  sourced by both; fix the log string; unify the updated-at helper name.
- Risk: behavior-preserving; these are systemd polling scripts outside the
  targets stores, so no cache concern — verify by running each script once.
- More detail in "Data-layer / archive-stack consolidation" item 2 below
  (`update_data_raw` near-twin; `get_version_timestamp` regexes differ).

#### M4. Dead trainer/date globals in the explore scripts

- `linreg` and `randforest_grf` (`scripts/flu_hosp_explore.R:11,13`,
  `covid_hosp_explore.R:11,13`): no grid row anywhere references `"linreg"` or
  `"randforest_grf"` (rg over `R/targets/`, `scripts/` finds zero hits, even
  commented); every live trainer string is `"quantreg"`/`"g_quantreg"`.
- `g_forecast_generation_dates` (`flu_hosp_explore.R:21`,
  `covid_hosp_explore.R:21`): consumed only by the *prod* scripts; explore
  snapshots use generation == forecast date.
- Minor twin: explore names the trainer object `quantreg`, prod names the same
  object `g_quantreg` — one name would help grepability.
- Change: delete the dead globals (and the commented `g_hhs_signal` line in
  flu explore).
- Risk: behavior-preserving; not referenced by any command, so no cache impact.

#### M5. `hhs_region` target is byte-identical between the two data-target files

- `R/targets/flu_data_targets.R:284-298` vs `covid_data_targets.R:297-311`:
  byte-for-byte identical `tar_target` (two csv URLs + join). The rest of the
  two files legitimately diverge; this is the one cleanly liftable block.
- Change: shared `hhs_region_target()` constructor in
  `R/targets/shared_utils.R` emitting the same expression.
- Risk: behavior-preserving; if the emitted command deparses identically the
  manifest diff is empty (verify with `tar_manifest()` per the pure-code-move
  rule).

#### M6. Submission-gate condition repeated four times in `build_prod_ensemble_targets`

- `g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))`
  appears verbatim in `make_submission_csv`, `make_climate_submission_csv`,
  `validate_result`, `validate_climate_result`
  (`R/targets/prod_shared.R:223,247,273,287`).
- Change: a named helper, e.g. `is_live_submission_date(forecast_date_int)`,
  so the gate reads as one concept and can't drift between the four copies.
- Risk: behavior-preserving; changes command text of exactly those four
  targets — all `tar_cue("always")` or cheap, so invalidation is free.

### Low

#### L1. `g_baseline_forecaster` name violates the documented `g_` convention

- `R/targets/prod_shared.R:5` — a *function in `R/`*, but the prefix means
  "global defined in the calling script" (both explore scripts and
  prod_shared.R:2-3 say so). Rename to `baseline_forecaster`.
- Risk: needs-golden-replay or manifest check — the symbol appears in prod
  grid rows (`scripts/*_hosp_prod.R` `forecaster = "g_baseline_forecaster"`),
  so renaming changes command text and invalidates the cdc_baseline forecast
  targets.

#### L2. Stale comment: ensemble spec already exists

- `R/targets/prod_shared.R:146-148`: "adjudicate when the ensemble layer gets
  its spec" — the ensemble layer got its spec in E1. Reword to "adjudicate
  separately (see nhsn-derived geo_exclusions asymmetry)". Comment-only and it
  sits *above* the `tar_target()` call (outside the frozen command), so no
  invalidation — sanity-check with `tar_manifest()`.

#### L3. `slide_forecaster` is legacy explore machinery, now test-only

- `R/looping.R:31-87`: callers are only `tests/testthat/test-forecasters-data.R`
  and `scripts/one_offs/forecaster_profiling.R` — the live explore path is
  `create_forecast_targets()` → `run_forecaster` + `make_forecast_snapshot`.
  It still carries the `forecaster_args_names` "hack around targets" that the
  param_names cleanup removed everywhere else, and its roxygen documents a
  nonexistent `exampleSpec.R` and a wrong first param name (`archive`).
- Change: at minimum fix the docs and drop `forecaster_args_names` (pass a
  named list, as the grids now do); better, move it next to the tests as a
  test helper so nobody mistakes it for pipeline code.
- Risk: behavior-preserving; test-only surface, `make test` verifies.

#### L4. `forecaster_lookup` cruft

- `R/utils.R:32-36`: `out %>% unlist()` result discarded; returns `NULL`
  invisibly on no match; roxygen documents `forecaster_grid` but the arg is
  `forecaster_params_grid`; line 20's `forecaster_params_grid %||% g_...` runs
  in a branch where the arg is known NULL. Five minutes of tidying for the
  REPL tool CLAUDE.md tells newcomers to use first.

#### L5. Repeated magic values that deserve names

- Territory FIPS `c("60", "66", "78")` (AS/GU/VI) filtered in
  `R/targets/prod_shared.R:255` and `score_forecasts`
  (`R/targets/score_targets.R:84`, as `c("US","60","66","78")`)
  → a named `EXCLUDED_TERRITORY_FIPS` constant.
- The `state_pop.csv` GitHub URL appears twice in `R/utils.R` (:294 inside
  `parse_prod_weights`, :346 in `get_population_data`); `parse_prod_weights`
  should call `get_population_data()` (or both should share a constant) —
  currently a weights-csv parse does its own network fetch.
- `compute_pca`'s sentinel default `filter_time = "2320-07-01"`
  (`R/forecasters/data_transforms.R:387`, "year 2320 = never") → `NULL` +
  guard, or at least a comment.
- Risk: behavior-preserving; `parse_prod_weights`/`compute_pca` body changes
  invalidate their consumers → cheap golden.

#### L6. Small broken/stale odds and ends

- `get_default_truth_data` (`R/plotting.R:8`): param misspelled `exclued_geos`
  while the body filters on `exclude_geos`, and `forecast_date` (:16) is not a
  parameter — the `is.null(truth_data)` branch of `plot_forecasts` errors.
  All four report callers pass `truth_data`, so it's an unreachable-until-hit
  trap. Fix the signature or delete the default branch.
- `format_storage` (`formatters.R:15`) accepts `true_forecast_date` and
  `target_end_date` and uses neither; every caller dutifully passes them.
  Drop the params.
- Commented-out `new_data_notebook` target
  (`R/targets/shared_utils.R`, end of `create_joined_targets`) with a TODO
  about a missing function — delete or file the fix.
- `get_nwss_coarse_data` (`R/aux_data_utils.R:289-294`): commented-out dynamic
  S3 fetch + hardcoded `nwss_20241028.csv` snapshot. Delete the dead block and
  state that the frozen snapshot is intentional (or fix the fetch).
- `drop_non_seasons` (`aux_data_utils.R:281`): the commented `season != "2021/22"`
  filter is an *intentionally kept* non-filter per its inline note — prefix it
  "INTENTIONALLY kept:" so a future cleanup doesn't reintroduce it.

### Suggested sequencing

H1 (pure deletion, `make test`) — then M4/L2/L4/L6 (comment/doc/global
tidies) — then M2/M5/M6/L5 (small helpers, manifest-diff or 1-date golden
each) — then H4 (the one real refactor, full golden replay both diseases) —
M1 last, since it needs a behavior adjudication from the user before
touching.

---

## Data-layer / archive-stack consolidation

Scouted 2026-07-22 (review-bot pass; structural claims spot-verified by rg).
The archive split is deeper than targets-vs-fetch: there are two disjoint
archive stacks for the same signals, plus flu/covid copy-paste inside each.

### The actual structure

1. **Explore path**: `R/targets/{flu,covid}_data_targets.R` — fetches via
   classic covidcast (`pub_covidcast`) and `get_health_data()`
   (healthdata.gov), munges once, merges everything into
   `joined_archive_data`.
2. **Prod path**: cron scripts `scripts/build_{nhsn,nssp}_archive.R` →
   Socrata → parquet → S3 → v5 "cast" API → `get_nhsn_data_archive()` /
   `up_to_date_nssp_state_archive()` (`R/aux_data_utils.R`) →
   `nhsn_prod_archive` / `nssp_target_archive` targets inline in the prod
   scripts.

So NHSN and NSSP are fetched from *different upstream services* in explore
vs prod, with independent munging code. They cross exactly once: flu prod
pulls explore's `joined_archive_data` for ILI+/flusurv augmentation rows.

**Caution — the layering is deliberate, keep it**: "munge in targets"
(explore) vs "munge in fetch function + target" (prod) isn't accidental
drift everywhere. Prod deliberately hoisted munging into
archive-construction targets (phase 0) so per-date targets are pure as-of
slices. Consolidation should share the munging functions and the target
factories, not collapse targets into fetchers.

### Ranked opportunities

1. **Flu vs covid duplication** (cheap, safe, self-flagged).
   `flu_data_targets.R` literally has `# TODO: Share code with covid?` at
   every shared target, and the prod scripts' archive targets are
   hand-parallel copies (`nhsn_prod_archive` flu:242 / covid:241,
   `nssp_target_archive` flu:303 / covid:284). This is exactly the pattern
   `build_prod_ensemble_targets()` already solved for the ensemble layer — a
   `build_prod_archive_targets(disease_spec)` factory in
   `R/targets/prod_shared.R` would be the same move, with flu's augmentation
   fold (and its source stamping / covid's deliberate lack of it) as spec
   fields. Same factory treatment applies to the explore data targets.
   Behavior-preserving, verifiable with `tar_manifest()` diff + empty
   golden.
2. **`build_nhsn_archive.R` vs `build_nssp_archive.R`** (cheap) — the same
   item as M3 above: shared `sync_s3_to_local_cache`/preamble plumbing
   hoisted into `R/`. Additional detail from this pass: `update_data_raw`
   is also a near-twin, and `get_version_timestamp` exists in both but with
   *different* filename regexes (nhsn:57 vs nssp:55) — parameterize, don't
   blindly unify. NSSP notably lacks the archive-assembly half NHSN has, so
   sharing the plumbing would also make adding it trivial.
3. **Dead code sweep** (trivial; extends L6): stale redefinitions of
   `get_health_data` (`scripts/one_offs/making_hhs_weekly_ex.R:8`, shadows
   `R/aux_data_utils.R:331`) and `generate_flusurv_adjusted`
   (`scripts/one_offs/build_flusion_data.R:133`); the commented-out NWSS S3
   logic with a hardcoded stale key is already L6.
4. **Explore↔prod path unification** (big, NOT a refactor). Migrating
   explore's NHSN/NSSP off `pub_covidcast` onto the v5 cast archives would
   change data provenance — different revisions, different latency
   semantics — so the golden diff won't be empty *by design*. It's a real
   project with a validation question ("do the two sources even agree?" —
   what `scripts/one_offs/compare_nssp_sources.R` was probing). It also
   interacts with E2 (sweeping prod ensembles in explore benefits from
   explore seeing prod's actual data). Treat 1–3 as consolidation and this
   as a separate data-migration decision.

## Smaller open threads

- **Per-forecaster `output_scale`**: per-disease today and NOT a live scoring
  bug (verified by magnitude 2026-07-19). If ever done, must be explicit
  per-family declarations — deriving it from `pop_scaling` would corrupt flu
  scores by ~pop/1e5. Low priority.
- **Per-source version policies** for explore-style multi-source snapshots.
- **Prod parallelism / BLAS**: measured 2026-07-19
  (`notes/2026-07-19-blas-timing.md`): pinning BLAS to 1 thread saves ~7%
  CPU-seconds consistently but wall time is unchanged at evaluation scale
  (16 targets / 12 workers). Not the clear win hypothesized; re-measure at
  prod scale before restructuring anything.
- **Rsv prod script**: a stub, not a priority; write `scripts/rsv_hosp_prod.R`
  directly on the shared stack whenever picked up.
