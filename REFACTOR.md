# Flu pipeline refactor: plan & experiments

Working notes for refactoring `scripts/flu_hosp_prod.R`. Companion to
`py-probe/ORCHESTRATION.md` (which explores a Python rewrite — **on hold**). This
doc is about restructuring the *existing R* pipeline in place, in small,
behavior-preserving steps.

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
4. **Prod is deterministic.** The prod grid (`:62`) is all `quantile_reg`. The one
   nondeterministic trainer (`grf_quantiles`) is explore-only. ⇒ a golden-output
   diff is a clean, near-bit-exact pass/fail oracle.
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
backtest** (BACKTEST_MODE=TRUE + `BACKTEST_N_DATES=<small>`), never the full
80-date run.

## Experiment sequence

- **Exp 0 — oracle.** capture + compare scripts; capture baseline goldens
  (prod-latest and partial-backtest). No production logic touched.
- **Exp 1 — extract inline commands → functions.** Ensembles, truth, submission
  formatting (`:313–483`). Behavior-preserving; diff must be empty. Enables 2–4.
- **Exp 2 — split prod/backfill.** DONE. Because this ran *before* Exp 1 (the
  ensembles are still inline), a copy-paste split would have duplicated the whole
  target DAG — the exact "duplicate the boilerplate" cost the merged pipeline was
  avoiding. So instead: extract the target-list construction into
  `build_flu_prod_pipeline()` in `scripts/_flu_prod_shared.R` (a factory reading
  the `g_*` globals, same pattern as `create_flu_data_targets()`), plus mode-
  independent globals. Two thin entry scripts — `flu_hosp_prod.R` (`g_backtest_mode
  <- FALSE`, `as_of = today`) and `flu_hosp_backfill.R` (`TRUE`, historical dates
  + `BACKTEST_N_DATES` hook) — set the mode-specific globals and call the factory.
  `g_backtest_mode` branching stays *inside* the factory (behavior-preserving);
  per-pipeline constant propagation of those branches is a later step. New
  `flu_hosp_backfill` project in `_targets.yaml`. Verified behavior-preserving:
  oracle diff ALL MATCH (max rel diff 0, all 12 targets) for both
  `flu_hosp_backfill`@N=3 vs `baseline-bt3` and prod-latest vs `baseline-latest`.
- **Exp 3 — `as_of` extraction + `version_policy`.** One slice function; replace
  `grepl("latest", id)`. Diff.
- **Exp 4 — column-select to forecaster; nhsn/nssp → grid rows.** One forecaster
  at a time; `(climate_linear, nssp, params)` row must reproduce hand-coded nssp
  `ensemble_clim_lin`. Settles the same-fn-vs-new-fn audit row by row.

## Gotchas

- Float reordering (ensemble means, `bind_rows` order) → set tolerance up front.
- `cue = tar_cue("always")` targets (`:396,421`) never cache; capture the frame
  *before* the CSV write so a `cache` submission dir doesn't yield empty goldens.
- Golden preserves current bugs — fixes are separate experiments (see above).
