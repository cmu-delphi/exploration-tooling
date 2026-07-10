# Flu pipeline refactor: plan & experiments

Working notes for restructuring the flu forecasting pipelines in place, in small,
behavior-preserving steps. Companion to `py-probe/ORCHESTRATION.md` (Python rewrite —
**on hold**).

> **Naming.** There was never a separate "backfill" pipeline. There is one production DAG
> (`build_flu_prod_pipeline()`, `scripts/_flu_prod_shared.R`) run in two modes: **latest**
> (forecast the current week, submit) and **evaluation** (replay a season, score). Old
> names (`flu_hosp_backfill.R`, `g_backtest_mode`, `BACKTEST_N_DATES`) survive in covid/rsv
> and in old oracle capture dirs. Read "backfill/backtest" as "evaluation".

## The bet

`targets` is good at one thing: content-addressed invalidation of an irregular **code** DAG
("I edited this node → recompute it + descendants"). In this repo that value lives in the
**archive layer** (`R/targets/flu_data_targets.R`): ~7 heterogeneous, network-bound sources
merged into `joined_archive_data`. Keep that in `targets`.

Everything below the archive is a **regular grid** — `forecast_date × forecaster × ahead ×
signal`. `targets` can only express a grid by *generating* it (`tar_map`/`tar_combine`/
`!!!.x`/`rlang::syms`), which is the metaprogramming that made the pipeline hard to read.

We tried removing `targets` from the grid (a plain loop) and **reverted** — see
"Loop vs tar_map" below. The grid stayed in `tar_map`; the win came from fixing the
*forecaster contract*, which was always independent of the scheduler.

## Durable findings

1. **Reuse failed silently.** `flu_hosp_explore.R` uses `create_forecast_targets()`
   (`R/targets/shared_utils.R:52`); prod hand-rolls its own `tar_map`. They share only the
   *data* layer. Merging files did not buy forecast reuse. **Share the functions, not the
   wiring.**
2. **Three run shapes:** explore (research bake-off, many forecasters), prod-latest
   (1 date), evaluation (historical replay, ~80 dates).
3. **Prod is NOT fully deterministic.** `quantile_reg`-based seasonal and climate
   forecasters are deterministic; `cdc_baseline` (simulated quantiles) and `linear` /
   `linear_no_population_scale` (residual sampling) are **stochastic**. They reproduce only
   because `targets` derives each target's seed from its *name* — so renaming a target
   changes the forecast. Any harness replacing the grid must reseed per
   `(forecaster × signal × date × ahead)`. (`grf_quantiles` is explore-only.)
   **Fixed for flu prod in Exp 7** — the seed is now derived from that semantic key, not
   the target name. Still true for covid/rsv.
4. **Two-level cache:** `targets` caches forecast *outputs*; `epix_slide_simple` caches
   as-of *slices* (`R/looping.R:99`, keyed on hash(archive) × date). Preserve both.
5. **Parallelism worry is likely BLAS oversubscription** — crew spawns `detectCores()-4`
   workers (`shared_utils.R:213`) while BLAS may also be multi-threaded. Pin BLAS to
   1 thread/worker, parallelize at task grain, measure. Not fundamental.
6. **The recurring asymmetry.** Prod/evaluation run *frozen code over varying dates* (input
   hashing suffices; invalidation blast radius irrelevant, 8 rows). Explore *edits
   forecasters and params constantly* (transitive code-invalidation and per-row
   invalidation are its whole value). This same split killed the loop (Exp 5) **and** the
   `config$` object (PR #169). Probably structural. Check any unification proposal against
   it first.

## Forecaster contract (target state)

- Harness owns **when** (version): one slice per `(archive, date, policy)`.
  `version_policy` is forecaster metadata, not a `grepl("latest", id)` branch.
- Forecaster owns **what** (columns): it selects the sources/columns it needs.
- `nhsn`/`nssp` and per-signal params are **grid rows** `(fn, signal, params)`, not separate
  functions. Rule: different *params/signal* → same fn, new row; different *logic* → new fn.

## Method: behavior-preserving refactors + a golden oracle

Success = **golden diff is empty**. Each experiment is one commit; green diff → keep, else
abandon and shrink the step. **Never mix a behavior-preserving refactor with a bug fix** —
the golden faithfully reproduces current bugs.

- `scripts/oracle/capture.R` — `tar_make` a project, dump archives (frozen inputs) +
  forecast/ensemble/score frames (golden outputs) to
  `cache/oracle/<project>/<label>-<rev>/<target>.parquet`. The `<rev>` suffix stamps
  source provenance into the folder name: a jj `change_id` (`[k-z]`, stable across the
  amend/squash that finalize a change) when jj is on `PATH`, else the git short hash
  (`[0-9a-f]`, plus `-dirty` for uncommitted work). `compare.R` resolves a bare `<label>`
  back to the folder, so callers never type the rev. capture.R looks up `jj` then `jj-musl`
  (a static jj binary in `~/.local/bin`; rocker/tidyverse has no system jj, so it's kept
  under a distinct name to avoid shadowing the host jj on the shared PATH).
- `scripts/oracle/compare.R` — diff two labels: exact on key columns, relative tolerance on
  value columns. Usage: `compare.R <project>:<label> <project>:<label> [tol]`.
- `make oracle-capture project=<p> label=<l> [n=<N>]` / `make oracle-compare project=<p> a=<l>
  b=<l>` wrap both, pinning `FORECAST_REFERENCE_DATE` (`ORACLE_REFERENCE_DATE` in the Makefile)
  so captures are reproducible week-to-week.

Golden scope: prod-latest (1 date) and a partial evaluation run (`EVALUATION_N_DATES=3`),
never the full ~80-date run.

## Done

Verified empty-diff (or explained-diff) at each step; see "Golden status".

| Step | What |
|---|---|
| **Exp 0** | Oracle capture + compare; baseline goldens. |
| **Exp 1** | Extracted the five inline ensemble command blocks into `flu_*` functions (`R/flu_ensembles.R`). |
| **Exp 2** | Split prod/evaluation. Target DAG extracted to `build_flu_prod_pipeline()` in `scripts/_flu_prod_shared.R`; two thin entry scripts set mode-specific globals. Avoided duplicating the DAG. |
| **Exp 3** | `flu_slice_archive()` (`R/flu_assemble.R:29`) is the single version-faithful slice. An explicit `version_policy` grid column replaced three `grepl("latest", id)` branches. |
| **Const-prop** | Mode-specific submission/validation/notebook tail moved to `R/flu_outputs.R`; `flu_output_targets(evaluation_mode)` emits it with `g_evaluation_mode` folded out. |
| **Exp 4** | **Killed the nhsn/nssp spoof.** `flu_assemble()` builds one modeling frame with honest source labels and exogenous column names, replacing the `forecast_nhsn`/`forecast_nssp` split and the `rename(nssp = value)` / `source = "nhsn"` lie. `flu_build_prod2_grid()` is the 16-row signal grid (8 forecasters × {nhsn, nssp}) carrying `outcome_signal / exogenous / primary_source / version_policy / scale / target_name`. `primary_source` threaded through `scaled_pop_seasonal` (defaults `"nhsn"`, so covid/rsv are untouched). |
| **scale/target_name** | Reporting transform (`scale`: model→submission units; `target_name`: CDC target string) lives on grid rows, read via `flu_report_scale()` / `flu_report_target()` at the submission/scoring/external boundary only. Deliberately **not** applied right after the forecaster — `forecast_*_full` must stay in model units so notebook plots and scoring align with truth. |
| **Exp 7** | **Semantic seeding.** `set.seed(tar_seed_create(paste(id, outcome_signal, forecast_date_chr, aheads, sep = "/")))` at the top of the forecast command (`_flu_prod_shared.R:144`). The **only deliberately behavior-changing** step so far: it moves the 3 stochastic forecasters and must not be diffed for an empty golden. See below. |

**Loop vs tar_map (resolved).** Exp 5 replaced the grid `tar_map` with one target looping
internally. That gave up per-cell caching, crew parallelism, `targets`' transitive
code-invalidation, and reproducible per-target seeds — while Exp 4's actual win
(`flu_assemble` + grid + adapters) never depended on the loop. **Reverted to `tar_map`**
over `flu_build_prod2_grid()` × dates: one `forecast_<id>_<signal>_<date>` per cell
(`pattern = map(aheads)`), then `tar_combine` split by `outcome_signal` into
`forecast_{nhsn,nssp}_full`. `R/flu_forecast_loop.R` is deleted but lives in git history.
Not pursued: keeping the loop with a `targets:::hash_imports`-keyed cache, or with `furrr`.

**Exp 7 — semantic seeding (behavior-changing, by design).** Rejected the first proposal
(an env var read inside each stochastic forecaster, set only by `capture.R`, seeding 42) on
three grounds: it validates a code path prod never runs, so prod stays name-seeded and the
brittleness is hidden rather than fixed; it doesn't address the actual defect, which is not
run-to-run variation (there is none) but that the seed derives from *a string refactors
change*; and one constant seed gives every `(date, ahead, signal)` cell the *same* RNG
stream, replacing `targets`' independent per-cell streams with perfectly correlated Monte
Carlo error across horizons — a modeling change smuggled in as a test fix.

Instead: seed unconditionally in the harness from the semantic cell key, reusing
`targets::tar_seed_create` (exported; string → valid integer seed) so `targets`' own seed
derivation is preserved but fed a stable key. `targets` sets its name-derived seed *before*
evaluating the command, so a `set.seed` inside the command cleanly overrides it. Seeded in
the harness, not the forecasters, because (a) a forecaster can't see its date/signal/ahead,
and (b) it removes the need to track *which* forecasters are stochastic — a set that isn't
stable, since swapping `quantile_reg()` for `rand_forest()` in `g_flu_windowed_seasonal`
would silently make a deterministic forecaster stochastic. Seeding all 8 costs nothing.

Consequence: **this is the one step whose golden diff must NOT be empty.** Expected shape —
nonzero for exactly `linear`, `cdc_baseline`, `linear_no_population_scale`; bit-zero for the
other 5 on both signals. That's the self-check that seeding didn't leak into the
deterministic path. Re-capture the baseline after landing.

Manifests: `flu_hosp_prod` 410, `flu_hosp_evaluation` 2789, `covid_hosp_prod` 395.

## Golden status (2026-07-10)

**New canonical baseline: `cache/oracle/flu_hosp_prod/baseline-pinned-9fb2cb8`** (label
`baseline-pinned`), captured on
current code (post-Exp 7) at the pinned `FORECAST_REFERENCE_DATE=2026-06-24` via
`make oracle-capture project=flu_hosp_prod label=baseline-pinned`. All future checks compare
to it: `make oracle-compare project=flu_hosp_prod a=baseline-pinned b=<label>`. Rebaselining
here is justified because Exp 1–4 + the tar_map revert were already verified bit-exact vs main
for the 5 deterministic forecasters (below), and Exp 7's seeding is a deliberate, understood
change; so current code is trusted parity + a known reseed. **Bonus:** because seeding is now
stable across renames, the new baseline makes *all 8* forecasters diff-checkable across future
refactors, not just the 5 deterministic ones.

Historical reference (pre-pinning): **`cache/oracle/flu_hosp_prod/baseline-bt3-e8d582e`** — the
pre-refactor baseline from oracle commit `e8d582e`, *before* Exp 2 created the
`flu_hosp_backfill` project; and `cache/oracle/flu_hosp_evaluation/postA-bt3`
(`EVALUATION_N_DATES=3`), which is **no longer comparable** (captured on a Sys.Date()-relative
window that has since rolled — re-baseline evaluation at the pinned date if needed). Verified a
real rebuild: all 240 grid cells (8 × 2 × 3 dates × 5 aheads) written during the run, per
`tar_meta(store=)`.

**Result: Exp 1–4 + the tar_map revert are bit-exact vs main for all 5 deterministic
forecasters, on both signals.** All rows join on
`(geo_value, forecast_date, target_end_date, quantile, forecaster, ahead)`. All 4 archives +
`joined_latest_extra_data` are bit-exact, so the diff is attributable to code, not network
drift. Exactly two differences, both expected:

1. **3 stochastic forecasters** — `linear` maxabs 110.034521, `cdc_baseline` 94.937045,
   `linear_no_population_scale` 2.383865 (nhsn). Target-rename seed change; see finding 3.
   The other 5 are `0.000000` on both signals.
2. **`source` on nssp: `nhsn` → `nssp`** — Exp 4's de-spoof landing. A *key* column changing,
   not a value. The old golden was reproducing the lie.

The `flu_hosp_backfill/*` captures are intermediates (that project didn't exist on main).
Diffing against `as-of-bt3` (Exp 3) yields *digit-identical* numbers to diffing against
`baseline-bt3` — independent evidence that Exp 1–3 really were empty-diff.

## Open TODOs

### Correctness questions (investigate; each needs its own explicitly-differing experiment)

1. ~~**latest-cutoff asymmetry.**~~ **Downgraded 2026-07-10: benign, non-shipped.** The
   exogenous nssp slice cuts at `forecast_date` while every sibling cuts at `generation_date`
   (`flu_exogenous_column`, `R/flu_assemble.R:106`). Only reachable via
   `version_policy == "latest"` — the `seasonal_nssp_latest` "cheating" forecaster, which
   peeks at the newest data revision purely to measure the gain from doing so and is **never
   ensembled, submitted, or shown in the notebook** (ensembles/submission keep only
   `climate_*`/`linear`/`windowed_seasonal*`). Investigated: `generation_date ==
   forecast_date` on 80/84 evaluation dates — they differ only on holiday-delayed
   off-Wednesday runs (gen always *later*, +1d ×3, +5d ×1), and on those 4 dates the slice
   differs by exactly one week (the forecast-reference-week nssp, 52 geos). A Sat(nhsn)/Wed(nssp)
   reference-day mismatch means neither cutoff aligns both signals across all delayed dates, so
   this is a symptom of the scattered alignment **Exp 6** consolidates, not an independent bug.
   Not worth a standalone experiment; subsumed by Exp 6.
2. ~~**3 of 8 forecasters are stochastic** (finding 3).~~ **Done for flu prod (Exp 7).**
   Remaining: port to `covid_hosp_prod.R` / rsv, whose harness has a different shape
   (separate nhsn/nssp targets, `get_partially_applied_forecaster`, `aheads` passed as a
   vector rather than mapped) so the semantic key differs. No rsv oracle exists.
3. ~~**Exogenous column asymmetry.**~~ **Downgraded 2026-07-10: benign code smell, no
   data difference.** An exogenous `nssp` column is the RAW slice (no time transform); an
   exogenous `nhsn` column is time-shifted (`time_value - 3`), and primary `nssp` is
   `floor_date(week_start=7) + 3`. Three spellings, but they all resolve to the same
   Wednesday grid on the actual data: raw nssp is stored 100% on Wednesday (prod and
   evaluation stores, 24,360 rows), raw nhsn 100% on Saturday. Verified empirically:
   `floor+3` is a no-op on Wednesday input and reproduces `-3` on every nhsn Saturday
   (`all(floor+3 == raw-3)` TRUE); the exogenous-nssp column and the primary-nssp column
   join on `(geo_value, time_value)` with **all 6069 rows matched, values byte-identical**.
   So the production `windowed_seasonal` join (nhsn primary + nssp exogenous) aligns
   correctly; switching the exogenous path to `floor+3` changes nothing. Unlike #1 (the
   *cutoff* asymmetry) this never bites — no differencing experiment needed.

   **Proposed fix (Exp 6, low-risk):** collapse to one named helper, e.g.
   `align_epiweek_wednesday(d) <- floor_date(d, "week", week_start = 7) + 3`, and call it at
   every alignment site. It is verified-equivalent to all three current spellings, so it is
   a pure readability change with no numeric effect. The real smell it removes: the
   exogenous-nssp branch (`R/flu_assemble.R:114`) applies *no* transform and silently relies
   on the raw archive being Wednesday-stamped — a signal that changed its reporting day, or a
   different exogenous signal, would misalign the join to all-NA with no error. The magic
   `-3` (assumes Saturday input) is likewise fragile. The `floor+3` idiom is already
   copy-pasted ~9× (`R/flu_assemble.R:92`, `R/utils.R:185`, `R/aux_data_utils.R:746,770`,
   `scripts/covid_hosp_prod.R:176,223,236`) plus two hardcoded `-3` sites
   (`R/flu_assemble.R:50`, `R/aux_data_utils.R:736`) — all candidates for the shared helper.
4. **`sort_by_quantile()` is applied inconsistently.** Explore applies it to flu forecaster
   output (`R/targets/shared_utils.R:68`, "TODO: Hack fix because whitening has edge
   cases"); `covid_hosp_prod.R:312` applies it; **flu prod does not** — it only sorts inside
   the ensembles (`R/flu_ensembles.R:30,41,52`) and in `forecaster_climatological.R:168`. So
   either flu prod ships individual forecasts with crossing quantiles, or the hack is
   unnecessary in the two places it appears. Resolve by asserting monotonicity at the
   forecaster output boundary and seeing what fires.

### Contracts to enforce

- **Version faithfulness.** No assembled row may carry `version > generation_date`. This is a
  **per-policy** invariant, not a global one — `version_policy = "latest"`
  (`seasonal_nssp_latest`) intentionally peeks, which is the entire reason
  `flu_slice_archive` exists. Assert for `as_of` rows only. Would have caught (1).
- **Frame shape at the assemble boundary.** `flu_assemble` states its contract by mutation:
  `attributes(x)$metadata$as_of <- ...` and `$other_keys <- "source"`, four times
  (`R/flu_assemble.R:57,60,94,95`). A `validate_model_frame()` at the end turns an
  assignment into a checked contract.
- **Forecaster output shape.** `(geo_value, forecast_date, target_end_date, quantile, value)`;
  quantiles monotone; no NAs; non-negative. Asserting this settles (4).
- **Units.** Partly done (`scale` / `target_name` on grid rows). The rest is Exp 6 below.
- ~~**Oracle provenance.**~~ **Done.** `capture.R` stamps `reference_date` **and** a source
  revision (`source_rev`) into `_manifest.csv`, and names the capture folder `<label>-<rev>`:
  a jj `change_id` when jj is available (stable across amend/squash; re-capturing the same
  change intentionally overwrites), else the git short hash + `-dirty`. `compare.R` resolves a
  bare `<label>` to that folder. Backfilled folders use best-guess **git** revs (REFACTOR.md's
  explicit mappings + folder mtimes vs commit order); low-confidence guesses:
  `refactored-latest`→`88b7d1f`, `refactored-bt3`→`b8e55b5`, `postA-bt3`→`c1ad910`.
  A static `jj-musl` binary lives in `~/.local/bin` so captures from rocker stamp change_ids;
  it is not declared anywhere, so a future Dockerfile/distrobox-assemble should install jj.

### Exp 6 — consolidate data alignment (normalize-in / denormalize-out)

Surfaced while building `flu_assemble`; punted to its own refactor. The Wed-centering
(`time_value ± 3`) and the nssp `/100` scale are applied in scattered places and, for some
sources, *twice*.

- **The shift is necessary** (weekly signals with different native reference days must share
  one axis so joins align and `ahead`-in-days is well defined; output must return to the CDC
  reference date). A real normalize-in / denormalize-out requirement — consolidate, not delete.
- **It's inconsistent, not required-to-be-scattered.** Most sources are centered at archive
  construction (`flu_data_targets.R:275`, via `g_time_value_adjust`), but nhsn is centered
  *per-forecast* (`_flu_prod_shared.R:180`, hardcoded `3`). Smoking gun:
  `aux_data_utils.R:691-695` has a comment "center the time_value on Wednesday …" next to a
  `mutate` that only touches `version` — the intended nhsn centering was never implemented at
  build time, so the harness patches it downstream.
- **Output denormalize is copy-pasted per forecaster:** `target_end_date + 3` in every prod
  forecaster, all three diseases (`flu_prod_forecasters.R:43,62`, covid, rsv), plus
  `+ g_time_value_adjust` again in `shared_utils.R:98-99` / score / external targets. nssp
  scale is the analogous round-trip: `/100` in (`flu_outputs.R:22`) ↔ `*100` out
  (`score_targets.R:77,:351`).

Target shape: normalize once at construction (implement the `:693` comment for nhsn —
`time_value` only), denormalize once post-forecast in the harness, forecasters never see a `3`
or `100`. **Version constraint:** center `time_value` only; leave `version` as the true report
date. Do NOT copy the `version = time_value` pattern (`flu_data_targets.R:275`) — that's for
synthetic sources without real revision history; applying it to nhsn corrupts as-of behavior.
Behavior-preserving in aggregate (a round-trip), but **diff final forecast frames, not
archives** — intermediate representations change. Touches all three diseases.

### `targets` metaprogramming: what is and isn't true

Investigated 2026-07-09 against `targets 1.11.4` / `tarchetypes 0.13.2`. Context: PR #169
tried an id → spec map and reverted it.

- **`tar_map` does NOT strip names from list-columns.** The docstring on
  `get_partially_applied_forecaster` (`R/targets/shared_utils.R:11`) asserts it does, which is
  why the grid carries a parallel `param_names` column (`R/utils.R:112`). Verified false: a
  `params` list-column substitutes with `names(params)` intact. (Plausibly true on an older
  tarchetypes.) ⇒ **`param_names` is deletable**, along with `set_names(params, param_names)`.
  Small, golden-checkable, touches flu + covid explore.
- **The real limitation is `tidy_eval`.** `tar_target`'s default `tidy_eval = TRUE` makes a
  bare `!!!params` in a command splice at *pipeline-build* time, failing with
  `object 'params' not found`. That is why the splice must hide inside a helper function.
  `tidy_eval = FALSE` leaves the `!!!` for `rlang::inject` to splice at *target-run* time.
  This — not "variable capture" — is the wall.
- **Why `config$` failed in #169, precisely: cache invalidation blast radius.** Called out in
  review at `covid_hosp_explore.R:279` ("any changes to any parameters of any forecasters will
  invalidate all previous runs") and conceded at `:31`. Root cause: the grid lived in a global
  object *dereferenced inside a command at run time*, so `targets` made the whole object a
  dependency of every branch. By contrast `tar_map(values = grid)` substitutes each row as a
  **literal into that branch's command** — verified: sibling branches get distinct command
  hashes.
- **⇒ The rule.** Spec-as-data is fine *provided every row is substituted by `tar_map` and no
  command ever dereferences a grid global.* Prod already obeys this (`forecaster` as a string
  + `get()` at `_flu_prod_shared.R:145`, `exogenous` as a list-column). The "best of" solution
  wanted in the #169 thread is available; #169 reached for the one form that breaks caching.
- Substituted values arrive as a `list(...)` **call**, not a literal object, so symbols inside
  resolve in the target's env at run time. `rlang::syms` on the `trainer` column is
  load-bearing, not decorative — don't "clean it up".
- Caveat: `keys_to_ignore = g_very_latent_locations` expands to a literal nested list in every
  row, so those commands stay ugly even once `param_names` is gone.

### Pipeline unification (explore ↔ prod/evaluation)

The data layer is already shared (`create_flu_data_targets()`, called by both). The gap is
forecast/score/notebook. Blockers, in order of realness:

- **`flu_assemble` can't build explore's frame.** Prod reads nhsn/nssp from separate
  `cue = tar_cue("always")` archives and takes everything else at *latest* version
  (`joined_latest_extra_data`), while explore slices `joined_archive_data` version-faithfully
  and gets its exogenous sources (`google_symptoms`, `nwss`, `va_flu_per_100k`) from it.
  ⇒ `version_policy` must become **per-source**, not a scalar on the forecaster row. This is
  the honest completion of Exp 3.
- **The grids differ in kind.** Prod rows *name frozen configurations* (params baked into the
  `flu2_*` adapters); explore rows *vary hyperparameters* (`params` + partial application).
  Unifying means the row carries `params` and the adapters become closures generated from a row.
- **Fan-out cannot and should not be shared.** Prod emits one target per
  `(forecaster × signal × date)`; explore's grid is a hyperparameter sweep an order of magnitude
  larger, which is why it batches dates *inside* one target via `epix_slide_simple` + its as-of
  cache (`R/looping.R:90`). Cross-producing explore's grid with dates gives tens of thousands of
  targets. **Share the cell** (`(grid_row, archives, date, ahead) → forecast frame`), keep two
  fan-out strategies.
- Cost to weigh: `create_forecast_targets()` is shared with `covid_hosp_explore.R:62`.
  Flu-specific logic pushed into it trades flu-explore/flu-prod divergence for
  flu-explore/covid-explore divergence.

Payoff if done: explore currently **cannot score ensembles**, and evaluation **cannot sweep
params**. Each becomes a one-line grid change once the cell is shared.

## Gotchas

- Float reordering (ensemble means, `bind_rows` order) → set tolerance up front.
- `cue = tar_cue("always")` targets never cache; capture the frame *before* the CSV write so a
  `cache` submission dir doesn't yield empty goldens.
- Golden preserves current bugs — fixes are separate experiments.
- `compare.R` uses relative diff; `rel = d / pmax(abs(a), 1e-12)` explodes to ~1e12 when the
  baseline value is 0. Read the absolute diff before panicking.
- **Miss a metric in `compare.R`'s `value_cols` and it becomes a sort key** — a tiny numeric
  change then scrambles row alignment and surfaces as a bogus "key columns differ".
  (`interval_coverage_50` / `_90` were missing; fixed.)
- **`EVALUATION_N_DATES=N` selects the tail of `seq.Date("2024-11-20", <ref>, by = 7L)`**
  (`flu_hosp_evaluation.R:23`). Historically `<ref>` was `Sys.Date()`, so the dates **moved
  with the calendar** and captures a week apart were not comparable. **Now pinnable:** set
  `FORECAST_REFERENCE_DATE` (an ISO date) to fix `<ref>` for both pipelines — evaluation's
  window becomes a pure function of `(ref, N)`, and prod-"latest" forecasts as-of `ref`
  (`epix_as_of(min(ref, versions_end))`, `_flu_prod_shared.R:104,119`). Unset → `Sys.Date()`
  (current behavior). `capture.R` stamps the effective `reference_date` into `_manifest.csv`
  and warns when unset. Constraint: `ref >= 2025-12-31` so evaluation's trailing hand-written
  `seq.Date()` segments stay non-empty and 1:1 aligned.
