# Clarity/simplification scout (2026-07-19)

Post-consolidation sweep for the NEXT round of cleanups: dead code,
duplication that survived the forecaster/ensemble unification, naming, and
onboarding friction. Every item was verified by rg over `R/`, `scripts/`
(incl. `one_offs/` and `reports/`), `tests/`, `Makefile` — or by reading the
code / running R — before inclusion. Known open threads (E2 explore-ensemble,
validate_forecast_snapshot, per-forecaster output_scale, per-source version
policies, BLAS pinning) are deliberately absent.

Cache-invalidation notes follow the repo discipline: "no command-text change"
means `tar_manifest()` diffs empty; function-*body* changes still invalidate
targets whose commands reference the function symbol (targets hashes global
functions), so those need a golden replay even when the manifest is clean.

## High

### H1. Delete the three unused explore forecaster families (~630 lines)

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

### H2. Delete `R/rsv_prod_forecasters.R` (last remnant of the old prod-closure style)

- All six `g_rsv_*` closures have zero references anywhere in `R/`, `scripts/`,
  `tests/`, `Makefile` (rg per function; `scripts/rsv_hosp_prod.R` does not
  exist). This is exactly the hand-written per-disease closure style the
  spec/runner refactor deleted for flu/covid.
- The `g_` prefix also lies: the documented convention is "globals defined in
  the calling script", and these live in `R/`.
- Change: delete the file. CLAUDE.md already says rsv should be written
  directly on the shared stack when picked up.
- Risk: behavior-preserving, no cache impact.

### H3. Dead-function purge across `R/utils.R`, `R/scoring.R`, `R/forecasters/formatters.R`, `R/aux_data_utils.R`

Zero callers anywhere (per-function rg over R/, scripts/, tests/, Makefile,
.github/); several are also internally broken:

- `format_covidhub` (`formatters.R:34-54`) — dead AND broken: body references
  undefined `forecasts`, `ahead`, `quantiles`, `source`, `signal`,
  `epipredict_forecast`. Cannot ever have worked as written. Delete.
- `read_external_predictions_data` (`scoring.R:74`) — dead.
- `get_old_nhsn_data_archive` (`aux_data_utils.R:713`) — superseded by
  `get_nhsn_data_archive` (:681, live in both prod scripts). Delete.
- `get_exclusions` (`utils.R:176`) — reads a `scripts/geo_exclusions.json` that
  doesn't exist (the live mechanism is the `*_geo_exclusions.csv` files). Delete.
- `compare_s3_etag` (`utils.R:1023`) — self-described "a test to verify that I
  understand how S3 ETags are computed". Delete.
- `get_tibble_hash` + its only consumer-of-record `get_file_hash` usage
  (`utils.R:981-1013`), `get_cast_api_updated_at` (:861),
  `get_cast_api_latest_update_date` (:1118),
  `get_covidcast_signal_last_update` (:795) — dead metadata/hash helpers.
- Manual-ops S3 helpers with no wiring: `get_bucket_df_delphi` (:727),
  `delete_extra_s3_files` (:562), `delete_duplicates_from_s3_by_etag` (:741)
  (whose only callee `delete_files_from_s3` then loses its last caller),
  `find_unused_score_files` (:597). Judgment call: delete, or move to
  `scripts/one_offs/` under an explicit "manual S3 maintenance, never called
  by pipelines" banner. Do NOT touch `get_targets_errors` /
  `forecaster_lookup` — REPL tools documented in CLAUDE.md.
- Risk: behavior-preserving, no cache impact (none appear in any command).
  `make test` + a grep pass is sufficient verification.

### H4. Extract the still-duplicated forecaster prologue/epilogue

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

### H5. `check_nssp_socrata_github_diff` / `get_nssp_upstream` cannot run

- `R/aux_data_utils.R:793-795` calls `get_nssp_upstream("github")` /
  `("socrata")`, passing the *source* string positionally into the first
  parameter `disease = c("covid","influenza")`; `rlang::arg_match(disease)`
  (:770) aborts on it. Only consumer is the Makefile target
  `check-nssp-socrata-github-diff` (Makefile:266-267), so the whole chain is
  a broken appendix.
- Change: fix the call to `get_nssp_upstream(disease = ..., source = ...)` if
  the check is wanted, else delete both functions and the Makefile target.
- Risk: behavior-preserving either way (currently errors on invocation); no
  cache impact.

## Medium

### M1. `filter_shared_geo_dates` has a permanently-dead else-branch

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
  cheap, golden optional.

### M2. Quantile-sorting idiom exists in three places, one with a stale error hint

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

### M3. `scripts/build_nhsn_archive.R` / `build_nssp_archive.R` share ~90 duplicated lines

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

### M4. Dead trainer/date globals in the explore scripts

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

### M5. `hhs_region` target is byte-identical between the two data-target files

- `R/targets/flu_data_targets.R:284-298` vs `covid_data_targets.R:297-311`:
  byte-for-byte identical `tar_target` (two csv URLs + join). The rest of the
  two files legitimately diverge; this is the one cleanly liftable block.
- Change: shared `hhs_region_target()` constructor in
  `R/targets/shared_utils.R` emitting the same expression.
- Risk: behavior-preserving; if the emitted command deparses identically the
  manifest diff is empty (verify with `tar_manifest()` per the pure-code-move
  rule).

### M6. Submission-gate condition repeated four times in `build_prod_ensemble_targets`

- `g_submission_directory != "cache" && (!g_evaluation_mode || as.Date(forecast_date_int) == max(g_forecast_dates))`
  appears verbatim in `make_submission_csv`, `make_climate_submission_csv`,
  `validate_result`, `validate_climate_result`
  (`R/targets/prod_shared.R:223,247,273,287`).
- Change: a named helper, e.g. `is_live_submission_date(forecast_date_int)`,
  so the gate reads as one concept and can't drift between the four copies.
- Risk: behavior-preserving; changes command text of exactly those four
  targets — all `tar_cue("always")` or cheap, so invalidation is free.

## Low

### L1. `g_baseline_forecaster` name violates the documented `g_` convention

- `R/targets/prod_shared.R:5` — a *function in `R/`*, but the prefix means
  "global defined in the calling script" (both explore scripts and
  prod_shared.R:2-3 say so). Rename to `baseline_forecaster`.
- Risk: needs-golden-replay or manifest check — the symbol appears in prod
  grid rows (`scripts/*_hosp_prod.R` `forecaster = "g_baseline_forecaster"`),
  so renaming changes command text and invalidates the cdc_baseline forecast
  targets.

### L2. Stale comment: ensemble spec already exists

- `R/targets/prod_shared.R:146-148`: "adjudicate when the ensemble layer gets
  its spec" — the ensemble layer got its spec in E1
  (notes/2026-07-19-ensemble-layer-log.md). Reword to "adjudicate separately
  (see nhsn-derived geo_exclusions asymmetry)". Comment-only and it sits
  *above* the `tar_target()` call (outside the frozen command), so no
  invalidation — sanity-check with `tar_manifest()`.

### L3. `slide_forecaster` is legacy explore machinery, now test-only

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

### L4. `forecaster_lookup` cruft

- `R/utils.R:32-36`: `out %>% unlist()` result discarded; returns `NULL`
  invisibly on no match; roxygen documents `forecaster_grid` but the arg is
  `forecaster_params_grid`; line 20's `forecaster_params_grid %||% g_...` runs
  in a branch where the arg is known NULL. Five minutes of tidying for the
  REPL tool CLAUDE.md tells newcomers to use first.

### L5. Repeated magic values that deserve names

- Territory FIPS `c("60", "66", "78")` (AS/GU/VI) filtered in
  `R/targets/prod_shared.R:255` and `R/scoring.R`-adjacent
  `score_forecasts` (`R/targets/score_targets.R:84`, as `c("US","60","66","78")`)
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

### L6. Small broken/stale odds and ends

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

## Suggested sequencing

H1 → H2 → H3 → H5 (pure deletions, one commit each, `make test` between) —
then M4/L2/L4/L6 (comment/doc/global tidies) — then M2/M5/M6/L5 (small
helpers, manifest-diff or 1-date golden each) — then H4 (the one real
refactor, full golden replay both diseases) — M1 last, since it needs a
behavior adjudication from the user before touching.
