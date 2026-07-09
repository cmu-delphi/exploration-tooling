# Flu pipeline refactor: plan & experiments

Working notes for refactoring `scripts/flu_hosp_prod.R`. Companion to
`py-probe/ORCHESTRATION.md` (which explores a Python rewrite — **on hold**). This
doc is about restructuring the *existing R* pipeline in place, in small,
behavior-preserving steps.

> **Naming note (later rename).** What the experiments below call the *backfill*
> pipeline (`flu_hosp_backfill.R`, `g_backtest_mode`, `BACKTEST_N_DATES`) is now the
> *evaluation* pipeline (`flu_hosp_evaluation.R`, `g_evaluation_mode`,
> `EVALUATION_N_DATES`). There was never a second pipeline — it's the one production
> DAG (`build_flu_prod_pipeline()`) run in two modes: **latest** (forecast the current
> week, submit) and **evaluation** (replay a season of past dates, score). "Backfill"
> named the mechanism (past dates); the purpose is evaluation. Rename was flu-only:
> covid/rsv keep their independent `g_backtest_mode`. Historical prose below still
> says "backfill/backtest" — read it as "evaluation".

## The bet

`targets` is good at one thing: content-addressed invalidation of an irregular
**code** DAG ("I edited this node → recompute it + descendants"). In this repo
that value lives entirely in the **archive layer** (`R/targets/flu_data_targets.R`):
~7 heterogeneous, network-bound sources merged into `joined_archive_data`. Keep
that in `targets`.

Everything below the archive is a **regular grid** — `forecast_date × forecaster
× ahead`, plus a hidden `nhsn/nssp` dimension. `targets` can only express a grid
by *generating* it (`tar_map` / `tar_combine` / `!!!.x` / `rlang::syms`), which is
the metaprogramming that makes `flu_hosp_prod.R` hard to read. The grid wants a
partition key + a parquet cache, not a DAG scheduler.

### The seams

- **ASOF (data subset).** `as_of(archive, d)` = latest version ≤ d per
  (geo, time_value). Currently re-derived inline in `full_data` / `forecast_nssp`
  / `forecast_nhsn` (`flu_hosp_prod.R:176–255`) and partly hand-cached by
  `epix_slide_simple` (`R/looping.R:90`). Lift to one function. This is the
  correctness boundary (version faithfulness) and the highest-value seam.
- **forecaster → ensemble.** A shallow 2–3 stage intra-partition dependency
  (`ensemble_mixture` consumes `ensemble_clim_lin`). Expressible as straight-line
  code within a partition; does not need a scheduler.

## Findings that shape the plan

1. **Reuse already failed, silently.** `flu_hosp_explore.R` uses the shared
   factory `create_forecast_targets()` (`R/targets/shared_utils.R:51`);
   `flu_hosp_prod.R` hand-rolls its own `tar_map` (`:164`). They share only the
   *data* layer. So merging prod+backtest into one file did **not** buy forecast
   reuse — it produced two diverged copies. Share the *functions*, not the wiring.
2. **Three run shapes, not two:** explore (research bake-off, many forecasters),
   prod-latest (1 date), prod `BACKTEST_MODE` (historical replay, ~80 dates,
   `:50`). The split should let all three share pure forecaster/ensemble functions.
3. **Ensembles aren't functions yet** — they're inline `tar_target` command blocks
   (`:313–483`). Extracting them is a prerequisite for both contract-unification
   and prod/backfill sharing.
4. **Prod is NOT fully deterministic** (corrected — see Exp 4 audit). The seasonal
   (`quantile_reg`) and climate forecasters are deterministic, but `cdc_baseline`
   (simulation-based quantiles) and `linear` / `linear_no_population_scale` (residual
   sampling) are **stochastic**. The golden oracle passes on re-runs only because
   `targets` assigns each target a deterministic seed derived from its name. ⇒ any
   loop/harness that replaces the grid MUST reseed per `(forecaster × date × ahead)`
   cell, or those three forecasters diff against the golden. (`grf_quantiles` remains
   explore-only.)
5. **Two-level cache exists:** `targets` caches forecast *outputs*;
   `epix_slide_simple` caches as_of *slices* (`R/looping.R:99`, keyed on
   hash(whole archive) × date). Any replacement must preserve both levels.
6. **Parallelism worry is likely BLAS oversubscription** — crew spawns
   `detectCores()-4` workers (`shared_utils.R:213`) while BLAS may also be
   multi-threaded. Fix: pin BLAS to 1 thread/worker, parallelize at task grain,
   measure. Not fundamental.

## Forecaster contract (target state)

- Harness owns **when** (version): one `as_of(archive, d)` → version-correct slice
  with all sources. `version_policy ∈ {as_of, latest}` becomes forecaster
  metadata, replacing the `grepl("latest", id)` branch.
- Forecaster owns **what** (columns): it selects the sources/columns it needs from
  the slice, replacing the harness-side `extra_data` / nssp-spoof branching.
- `nhsn`/`nssp` and per-signal params (e.g. nssp `climate_linear` uses
  `max_climate_*`, `:322`) become **grid rows** `(fn, signal, params)`, not
  separate functions. Rule: different *params/signal* → same fn, new row;
  different *logic* → new fn.

## Method: behavior-preserving refactors + a golden oracle

None of these experiments change the model. Success = **golden diff is empty**.
Because prod is deterministic, diffs should be near-bit-exact (float-reorder
tolerance only). Each experiment is one `jj` commit; green diff → keep, else
`jj abandon` and shrink the step. **Never mix a behavior-preserving refactor with
a bug-fix in one commit** — the golden faithfully reproduces current bugs (e.g.
the `forecast_nhsn` slice uses `forecast_date_int` while siblings use
`forecast_generation_date_int`, `:251` vs `:254`; fixing that is a *separate*,
explicitly-differing experiment).

### Oracle (Exp 0)

`scripts/oracle/capture.R` — build a project, `tar_read` the archives (frozen
inputs) + forecast/ensemble frames (golden outputs), write each to
`cache/oracle/<project>/<label>/<target>.parquet` (nanoparquet). epi_archives are
dumped via `$DT`.

`scripts/oracle/compare.R` — diff two labels target-by-target: exact on key
columns, relative tolerance on value columns. Empty diff = behavior preserved.

Golden scope: **prod-latest** (BACKTEST_MODE=FALSE, 1 date) and a **partial
backtest** (BACKTEST_MODE=TRUE + `EVALUATION_N_DATES=<small>`), never the full
80-date run.

## Experiment sequence

- **Exp 0 — oracle.** capture + compare scripts; capture baseline goldens
  (prod-latest and partial-backtest). No production logic touched.
- **Exp 1 — extract inline commands → functions.** Moved the five complex
  ensemble-map command blocks (`geo_weights`, `ensemble_clim_lin`, `ens_ar_only`,
  `ensemble_mixture`, `truth_data`) verbatim into `flu_*` functions in
  `R/flu_ensembles.R`; the targets now call them with their deps as args. Trivial
  blocks (`forecast_filtered`, `geo_exclusions`, `forecasts_and_ensembles`) left
  inline. Submission targets left as-is — already function-based (`format_flusight`)
  and entangled with the `g_evaluation_mode`/`g_submission_directory` gating, which
  belongs to per-pipeline constant propagation, not this pass. Verified
  behavior-preserving: full re-execution (686 targets, 0 skipped) diffs ALL MATCH
  (max rel diff 0, all 12 targets) against the pre-refactor `baseline-bt3`.
- **Exp 2 — split prod/backfill.** DONE. Because this ran *before* Exp 1 (the
  ensembles are still inline), a copy-paste split would have duplicated the whole
  target DAG — the exact "duplicate the boilerplate" cost the merged pipeline was
  avoiding. So instead: extract the target-list construction into
  `build_flu_prod_pipeline()` in `scripts/_flu_prod_shared.R` (a factory reading
  the `g_*` globals, same pattern as `create_flu_data_targets()`), plus mode-
  independent globals. Two thin entry scripts — `flu_hosp_prod.R` (`g_evaluation_mode
  <- FALSE`, `as_of = today`) and `flu_hosp_evaluation.R` (`TRUE`, historical dates
  + `EVALUATION_N_DATES` hook) — set the mode-specific globals and call the factory.
  `g_evaluation_mode` branching stays *inside* the factory (behavior-preserving);
  per-pipeline constant propagation of those branches is a later step. New
  `flu_hosp_evaluation` project in `_targets.yaml`. Verified behavior-preserving:
  oracle diff ALL MATCH (max rel diff 0, all 12 targets) for both
  `flu_hosp_evaluation`@N=3 vs `baseline-bt3` and prod-latest vs `baseline-latest`.
- **Exp 3 — `as_of` extraction + `version_policy`.** DONE. `flu_slice_archive`
  (R/flu_data_prep.R) is the single version-faithful slice: `as_of` →
  `epix_as_of(min(gen_date, versions_end))`, `latest` →
  `epix_as_of(versions_end) |> filter(time_value < cutoff)`. An explicit
  `version_policy` column on the forecaster grid replaces the three
  `grepl("latest", id)` branches in `full_data` / `forecast_nssp` /
  `forecast_nhsn` (and the `data_substitutions` gate). The `forecast_nhsn`
  latest-cutoff asymmetry (forecast date, not generation date) is preserved via
  the `latest_cutoff` arg — flagged, not fixed (a fix is a separate experiment).
  tar_map substitutes `version_policy` to a string literal per branch (verified).
  Verified behavior-preserving: forecast targets re-executed (362 completed) and
  diff ALL MATCH (max rel diff 0, all 12 targets) vs `baseline-bt3`.
- **Exp 4 — column-select to forecaster; nhsn/nssp → grid rows.** One forecaster
  at a time; `(climate_linear, nssp, params)` row must reproduce hand-coded nssp
  `ensemble_clim_lin`. Settles the same-fn-vs-new-fn audit row by row.

- **Per-pipeline constant propagation.** DONE. The mode-specific ensemble-map
  tail (`make_submission_csv`, `make_climate_submission_csv`, `validate_result`,
  `validate_climate_result`, `notebook`) moved to `R/flu_outputs.R`: bodies
  extracted as `flu_write_submission` / `flu_write_climate_submission` /
  `flu_validate_submission` / `flu_validate_climate_submission` /
  `flu_render_forecast_notebook`, and `flu_output_targets(evaluation_mode)` emits
  the tail with `g_evaluation_mode` folded out — production gates on
  `dir != "cache"` and renders the notebook; backfill gates on
  `dir != "cache" && final date` and drops the (dead) notebook. The factory
  splices `flu_output_targets(g_evaluation_mode)` into the ensemble tar_map.
  Manifest confirms: prod has `notebook` (1), backfill has none (0), submission
  targets present in both. CAVEAT: in cache mode these targets no-op, so the
  output oracle does not exercise the gates; gate correctness is by boolean
  constant fold. Captured-target diff vs `baseline-bt3` ALL MATCH.

## Exp 5 — the bet: drop the grid `tar_map` for a loop (PROPOSED, not committed)

This is the payoff step the earlier experiments cleared the runway for. **Not yet
decided** — gated on the caching question below.

**The move.** Replace `forecast_targets` (the grid `tar_map`) and
`build_combined_forecast_targets` (the `tar_combine`) with one target that loops
over the grid internally and returns `forecast_nhsn_full` / `forecast_nssp_full`
directly. The grid stops being `tar_map(values = expand_grid(...))` and becomes a
tibble (`g_forecaster_params_grid` × dates × aheads) iterated with `pmap`. The loop
body is the current `forecast_nssp`/`forecast_nhsn` command blocks (`flu_slice_archive`
→ `forecaster_fn` → tag). All `tar_map`/`tar_combine`/`!!!.x`/`rlang::syms`
metaprogramming — the unreadable part — is deleted. Archive layer stays in `targets`.
Ensemble/score `tar_map`s are a separate, smaller follow-on (they already just filter
`forecast_*_full` by date).

**Two things `targets` gave the grid for free (must be carried, or lost):**

1. **Per-cell caching + invalidation.** One target ⇒ any change reruns the whole
   grid. Replace with a parquet cache inside the loop, keyed on
   `(forecaster_id, date, ahead, signal, hash(inputs))` — same pattern as
   `epix_slide_simple` (`looping.R:99`).
2. **Parallelism.** `tar_map` cells fan out over crew. Replace with
   `furrr::future_pmap` inside the target + BLAS pinned to 1 thread/worker (finding 6).
   Believed recoverable.

**Phasing (each oracle-checked):**

- **A** — `run_forecast_grid(grid, archives, dates, aheads)` returning the two full
  frames, added *alongside* the existing `tar_map`. Diff vs current
  `forecast_nhsn_full`/`forecast_nssp_full`. Empty diff → loop reproduces the grid.
- **B** — keyed parquet cache in the loop.
- **C** — `future_pmap` + BLAS pin; measure vs crew.
- **D** — delete `forecast_targets` + `build_combined_forecast_targets`.

**DECISION GATE — cache invalidation on *code*, not just inputs.** `targets` does
content-addressed invalidation: it statically parses each command, finds the global
functions it calls, and hashes their bodies — *transitively*. Edit
`g_flu_windowed_seasonal`, or a helper it calls (`scaled_pop_seasonal`), and dependent
targets recompute. A homegrown input-hash cache does **not** see this: after editing a
forecaster it serves stale forecasts. This is the risk that isn't obviously
recoverable, and it won't show in an oracle run (a clean rebuild passes; only
*incremental* reruns go stale). Options, none free:

- **(a) Hash the forecaster closure** into the key (`rlang::hash` of body+formals, or
  `deparse(body(fn))`). Catches edits to the *top-level* forecaster. Misses transitive
  helper edits — you'd manually clear the cache after touching `scaled_pop_seasonal` &
  co. Cheap; partial.
- **(b) Reimplement transitive code-dep hashing** (walk the call graph like `targets`
  does via codetools). Full fidelity; this is rebuilding the thing we're removing. Trap.
- **(c) Live without it.** Accept manual cache-clear on code edits. Viable *only* if the
  edit-forecaster-code loop is rare.

**The asymmetry that may decide it:** prod/backfill run *frozen* code varying dates ⇒
input-hash caching is sufficient there. Explore edits forecasters and helpers
constantly ⇒ transitive code-invalidation is exactly its value. So the honest options
are (a)+(c) for prod while **keeping `targets` for explore** — but that reintroduces the
prod/explore divergence finding 1 warns about. Resolving this tension is the
prerequisite for committing to Exp 5.

## Exp 4 — assemble + kill the nhsn/nssp spoof (SPIKE VALIDATED, not committed)

Spiked in `R/flu_assemble.R` + `R/flu_forecast_input.R`. `flu_assemble(outcome_signal,
exogenous)` builds one modeling frame with **honest** source labels and exogenous
column names, replacing the two-target (`forecast_nhsn`/`forecast_nssp`) split and the
`rename(nssp = value)` / `source = "nhsn"` spoof. The forecaster takes `primary_source`
(threaded through `scaled_pop_seasonal` → `run_workflow_and_format` →
`get_oversized_test_data`, default `"nhsn"` so other callers are untouched) instead of
hardcoding `"nhsn"` as "the primary series".

Validated: `windowed_seasonal_extra_sources` reproduces the spoof path **bit-exactly**
(max abs value diff 0) on *both* signals, including the nssp run with fully honest
labels (`source="nssp"`, exogenous column `nhsn`, `primary_source="nssp"`). Remaining to
make real: put `(outcome_signal, exogenous, primary_source, scale, target_name)` on the
grid rows; collapse the two forecast targets into one loop.

**Audit DONE (all 8 grid rows).** With equal RNG state, every forecaster reproduces the
spoof path bit-exactly (maxdiff 0) on both signals under `flu_assemble` — seasonal with
its `exogenous`, the other five with empty `exogenous`. The audit surfaced the
nondeterminism now recorded in finding 4: `cdc_baseline` / `linear` /
`linear_no_population_scale` are stochastic, so the harness loop must reseed per cell
(the only non-obvious prerequisite for integration; assembly itself is clean).

**INTEGRATED into `flu_hosp_prod` + `flu_hosp_evaluation` (spoof removed, spike retired).**
`flu_build_prod2_grid()` (R/flu_assemble.R) is the 16-row signal grid (8 forecasters x
{nhsn, nssp}) carrying `outcome_signal / exogenous / primary_source`;
`flu_run_forecast_grid` (R/flu_forecast_loop.R) assembles per row via `flu_assemble`,
calls the `flu2_*` adapters, and splits by `outcome_signal` into `forecast_nhsn_full` /
`forecast_nssp_full`. This replaced the `forecast_nhsn`/`forecast_nssp` tar_map +
`build_combined_forecast_targets` in `build_flu_prod_pipeline()`; downstream
(ensembles/scores/submission) is unchanged and consumes the same `forecast_*_full`
names. Removed: `scripts/flu_hosp_prod2.R` (+ its `_targets.yaml` project),
`R/flu_forecast_input.R` (dead spoof helpers; `flu_build_full_data` / `flu_load_archives`
moved to `R/flu_assemble.R`), and the old `g_forecaster_params_grid` tibble (slimmed to
`tibble(id = unique(flu_build_prod2_grid()$id))`, the only part the ensemble stage uses).
Manifests build: flu_hosp_prod 395, flu_hosp_evaluation 1414, covid_hosp_prod 395
(unaffected -- the `primary_source` arg added to `scaled_pop_seasonal` defaults to
"nhsn"). Console diff vs the spoof path: all 16 combos maxdiff 0 (matched per-ahead seed).

**`scale` + `target_name` on the grid (DONE).** Each grid row carries the reporting
transform (`scale`: model-units -> submission-units, e.g. nssp 0.01; `target_name`: the
CDC target string). Accessors `flu_report_scale(signal)` / `flu_report_target(signal)`
read them, and the submission (`flu_outputs.R`), local scoring, and external-comparison
sites in `_flu_prod_shared.R` now pull from them instead of hardcoded `100` / target
strings. Deliberately NOT applied right after the forecaster: `forecast_*_full` must stay
in model units so the notebook plot and scoring align with truth (`flu_truth_data`), and
this reporting scale is separate from any internal pop/quantile normalization a
forecaster does. Pure constant-extraction (`value/100 == value*0.01`), behavior-
preserving; not moved into the plotting path. The scattered target-string *filters* in
the ongoing-score notebooks were left as literals (report-gen / plotting side).

**Golden must be re-baselined** (decision: accept). A real `tar_make` diff will show the
3 stochastic forecasters (`cdc_baseline`, `linear`, `linear_no_population_scale`)
differing -- the loop uses a global seed 42, `targets` used a per-target seed; the 5
deterministic rows match. Not yet run against live data. Remaining, deferred: `scale` /
`target_name` post-processing on the grid row (the `/100` + target names, still in the
ensemble/submission stage); per-cell seeding (finding 4) if bit-exact stochastic parity
is ever wanted.

## Exp 6 — consolidate data alignment (normalize-in / denormalize-out) (DEFERRED)

Surfaced while building `flu_assemble`; **punted to its own refactor.** The Wed-centering
(`time_value ± 3`) and the nssp `/100` scale are applied in scattered places and, for
some sources, *twice*.

- **The shift is necessary** (weekly signals on different native reference days must share
  one axis so joins align, `ahead`-in-days is defined, and lags mean the same distance;
  output must return to the CDC reference date). So it's a real normalize-in /
  denormalize-out requirement — can't be deleted, only consolidated.
- **It's inconsistent, not required-to-be-scattered.** Most sources are centered at
  archive construction (`flu_data_targets.R:275`, using `g_time_value_adjust`), but nhsn
  is centered *per-forecast* (`_flu_prod_shared.R:180`, hardcoded `3`). Smoking gun:
  `aux_data_utils.R:691-695` has a comment "center the time_value on Wednesday …" next to
  a `mutate` that only touches `version` — the intended nhsn centering was never
  implemented at build time, so the harness patches it downstream.
- **Output denormalize is copy-pasted per forecaster:** `target_end_date + 3` in every
  prod forecaster, all three diseases (`flu_prod_forecasters.R:43,62`, covid, rsv), plus
  `+ g_time_value_adjust` again in `shared_utils.R:98-99` / score / external targets. nssp
  scale is the analogous round-trip: `/100` in (`flu_outputs.R:22`) ↔ `*100` out
  (`score_targets.R:77,:351`).

Target shape: normalize once at construction (implement the `:693` comment for nhsn —
`time_value` only), denormalize once post-forecast in the harness (single `+3` + `/100`
step), forecasters never see a `3` or `100`. **Version constraint:** center `time_value`
only; leave `version` as the true report date. Do NOT copy the `version = time_value`
pattern (`flu_data_targets.R:275`) — that's for synthetic sources without real revision
history; applying it to nhsn corrupts as-of behavior. Behavior-preserving in aggregate
(a round-trip) so the golden oracle guards it, but **diff final forecast frames, not
archives** — intermediate representations change. Touches all three diseases; keep it
separate from Exp 4 so the oracle stays clean.

## Status & remaining (session ledger)

**Done and integrated** (flu_hosp_prod + flu_hosp_evaluation; manifests build 395 / 1414;
covid unaffected):
- Exp 0-3 (oracle, ensemble extraction, prod/backfill split, `flu_slice_archive` +
  `version_policy`) — all verified empty-diff previously.
- Exp 4 — spoof removed. `flu_assemble` builds honest per-signal input; `flu2_*`
  adapters + `flu_build_prod2_grid` (16-row signal grid: `outcome_signal / exogenous /
  primary_source / scale / target_name`); `primary_source` threaded through
  `scaled_pop_seasonal`. `scale`/`target_name` read at the reporting boundary. Forecaster
  is now a plain function callable in the REPL (`flu_load_archives` + `flu_assemble`).
  Console-validated bit-exact vs the spoof path (16/16, matched seed).
- Forecast grid collapsed from `tar_map`+`tar_combine` to one `forecast_full` loop
  target (`flu_run_forecast_grid`). **This is the piece with a cost** (below).

**The loop's cost, and that it's separable.** The Exp-4 contract win did NOT require the
loop; the loop is an independent choice that gave up (a) per-cell output caching, (b)
crew parallelism, (c) targets' transitive code-invalidation, and (d) reproducible
per-target seeds (hence the stalled golden rebaseline). All of Exp 4's value lives in
`flu_assemble`/grid/adapters, which work equally under a `tar_map`.

**Chosen: option A (DONE).** Forecast generation reverted from the single `forecast_full`
loop to a `tar_map` over `flu_build_prod2_grid()` x dates — one `forecast_<id>_<signal>_
<date>` target per cell (`pattern = map(aheads)`), each calling `flu_assemble` + the
`flu2_*` adapter with honest labels, then `tar_combine` split into `forecast_{nhsn,nssp}_
full` by an `outcome_signal` tag. This restores per-cell caching / crew parallelism /
code-invalidation / per-target seeds — targets does it, no cache to hand-build. All Exp-4
wins kept (assemble, honest contract, grid, scale/target_name, REPL dev loop). The loop
(`flu_run_forecast_grid` / `R/flu_forecast_loop.R`) is deleted but lives in git history if
the loop route (option B, below) is ever wanted. Manifests: flu_hosp_prod 410,
flu_hosp_evaluation 2789 (per-cell granularity back), covid 395. Verified the `tar_map`
substitutes the string fn-name (`get("flu2_...")`) and the `exogenous` list-column
correctly (`"nhsn"` / `character(0)`); command inspection clean. Not yet run via `tar_make`.

Golden: the `tar_map` gives *new* target names (`forecast_<id>_<signal>_<date>` vs the old
`forecast_nssp_<id>_<date>`), so the 3 stochastic forecasters get different per-target
seeds -> not bit-exact vs the old golden. Deterministic forecasters + everything derived
from them stay exact. Fix is a one-command re-capture (capture.R with a new label), NOT
the seed-injection dance; equivalence is already console-proven. (If literal old-golden
bit-exactness were required, keep the original two-family `forecast_nhsn`/`forecast_nssp`
target names instead of the signal-expanded grid — declined as not worth it.)

Not pursued: **B** (keep loop + `targets:::hash_imports`-keyed cache) — recorded above if
the loop is ever revived; **C** (loop + `furrr`, no cache).

## Gotchas

- Float reordering (ensemble means, `bind_rows` order) → set tolerance up front.
- `cue = tar_cue("always")` targets (`:396,421`) never cache; capture the frame
  *before* the CSV write so a `cache` submission dir doesn't yield empty goldens.
- Golden preserves current bugs — fixes are separate experiments (see above).
