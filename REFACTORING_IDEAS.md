# Refactoring Ideas

Five independent reviews (Claude agents, 2026-07-12) each asked for one
non-cosmetic refactoring idea that would make the code structure easier to
understand and make it simpler to create a new forecaster and integrate it
into backtesting/exploration. **All five converged on the same refactoring**:
unify the duplicated explore/prod forecaster integration behind a single
spec/registry plus a shared runner.

## The consensus idea (5/5 reviews)

Today a forecaster is wired in through two incompatible mechanisms:

- **Explore**: string-named functions in parameter tibbles
  (`get_*_forecaster_params()` → `make_forecaster_grid` →
  `get_partially_applied_forecaster`), signature
  `fn(epi_data, outcome, ahead-in-days, ...)`
- **Prod**: hand-written per-disease closures (`R/flu_prod_forecasters.R`,
  `covid_prod_forecasters.R`, `rsv_prod_forecasters.R`) matched positionally
  to an `ids` vector via `rlang::syms`, signature
  `fn(epi_data, ahead-in-weeks, extra_data, ...)`

Each path re-implements the same cross-cutting conventions independently:
`ahead * 7` day/week conversion, the Wednesday→Saturday `target_end_date + 3`
shift, source filtering, geo exclusions, population rescaling,
`keys_to_ignore = g_very_latent_locations`, plus per-disease patches like the
`if (g_disease == "flu") sort_by_quantile()` hack in `create_forecast_targets`.

**Proposed fix**: one declarative forecaster spec per forecaster — core
function + parameter grid + metadata (ahead units, as-of policy, output scale,
source filter, required exogenous inputs) — consumed uniformly by both
`create_forecast_targets()` (explore) and the prod `tar_map`. Adding a
forecaster becomes: write the modeling core, add one spec entry, and it works
in exploration sweeps, backtesting, and all prod pipelines.

## Distinct emphases

- **Forecaster-body boilerplate** (reviews 1 & 2): beyond the wiring, every
  forecaster copies ~50 lines of self-labeled "copypasta"
  (`R/forecasters/forecaster_scaled_pop.R:75-103`): `validate_epi_data`, the
  insufficient-data empty-tibble early return, the fake `source`-column hack,
  `default_args_list` assembly, `sanitize_args_predictors_trainer`,
  whitening/coloring, `pmax(0, value)`. Hoist that into a `run_forecaster()`
  prologue/epilogue so a new forecaster is just its ~40-line modeling core.
- **Id-string dispatch** (review 5): prod targets also dispatch behavior by
  grepping id strings (`grepl("latest", id)` for as-of vs latest data), and
  scoring sniffs for a `population` column to decide whether to unscale —
  both should be spec metadata instead.
- **Drift as correctness hazard** (reviews 3, 4, 5): the strongest motivation
  isn't ergonomics but that the prod version of "the same" forecaster can
  silently diverge from what exploration actually evaluated; a shared spec
  makes that drift impossible by construction.

## Combined takeaway

One coherent refactor with three layers:

1. A shared runner owning validation and date/unit conventions.
2. A declarative spec registry replacing string dispatch and hand-rolled prod
   wrappers.
3. Both pipelines consuming the same registry, so explore results transfer to
   prod verbatim.

## Part 3 implementation sketch: shared explore/prod runner (2026-07-13)

Status: parts 1–2 (prod grid columns, explicit as-of policy, dropped id
dispatch) are done on the prod side. What remains is making explore and prod
call the same runner over the same-shaped data.

### The core observation

Both pipelines are already the same two-step loop, just factored differently:

- **Explore**: `epix_slide_simple(archive, fn, dates)` — per date: snapshot
  the archive as-of that date, call the partially-applied forecaster, bind.
  Branches over forecasters; dates live inside the slide (with a parquet
  snapshot cache).
- **Prod**: `tar_map` over forecaster × date — per branch: build `full_data`
  (an as-of snapshot with hacks), call `run_prod_forecaster`. Dates are
  targets because ensembles/reports/submissions hang off each date.

So the shared runner is `snapshot(archive, date, policy) |> forecast(spec)`,
and the two pipelines keep their different branching shapes around it.

### What actually blocks sharing today: data shape

Prod's `full_data` target does per-date, per-forecaster work that explore's
archive already has baked in, or does differently:

| concern | explore | prod |
|---|---|---|
| outcome column | `hhs` on multi-source archive | renamed to `value` |
| time alignment | Saturday-native, `+3` applied at scoring | `time_value - 3` at snapshot, `target_end_date + 3` after |
| geo names | archive-native | `usa -> us` per snapshot |
| season info | in archive | `add_season_info()` per snapshot |
| extra latest data | n/a (archive is fully versioned) | `joined_latest_extra_data` bound per snapshot |
| substitutions | none | `data_substitutions()` per snapshot |
| nssp-as-target | n/a | column-rename "spoofing" inside the target |

### Phase 0 — canonicalize the archive contract (prod side)

Define one input contract: an `epi_archive` with keys
`(geo_value, time_value, source)`, `other_keys = "source"`, a canonical
outcome column name, season columns present, geo names normalized,
Wednesday/Saturday convention fixed once. Move prod's `full_data` hacks
(`usa->us`, `time_value - 3`, `add_season_info`, `source = "nhsn"` stamping)
out of the per-date target into the archive-building targets — they're
version-independent transforms, so apply them once to `nhsn_archive_data`
instead of per forecaster × date. The nssp-as-target "spoofing" becomes a
second canonical archive target (`nssp_target_archive`) built once, killing
the in-target renames. `full_data` shrinks to: as-of slice + substitutions +
bind latest extras.

### Phase 1 — shared snapshot function

`make_forecast_snapshot(archive, forecast_date, generation_date, as_of_policy,
substitutions = NULL, extra_latest = NULL, cache_key = NULL)` → epi_df with
correct `as_of`/`other_keys` metadata. Explore's call is the degenerate case
(policy `"asof"`, generation == forecast date, no substitutions/extras) —
i.e. the body of `epix_slide_simple`, keeping its cache. Prod's `full_data` /
per-date nssp slices become calls to this. `epix_slide_simple` survives as
`map(dates, make_forecast_snapshot) |> map(fn) |> bind_rows` for explore.

### Phase 2 — one runner

Rename/extend `run_prod_forecaster` → `run_forecaster(snapshot, forecaster,
aheads, params, param_names, id, spec_cols...)`. It already owns ahead-unit
conversion, source filtering, extra-data join, target-date shift, geo
exclusions, id stamping. Add what explore's target body does around it:
the flu `sort_by_quantile()` whitening hack (make it a spec flag until the
epipredict fix), and settle the output schema — recommend hub convention
(Saturday `target_end_date`, column `value`, `forecaster` id) as the runner
output, with explore's scoring adapting (`prediction` rename, ahead calc)
instead of each pipeline shifting dates differently. That deletes the
scattered `+ g_time_value_adjust` in `create_forecast_targets` and
`ensemble_targets`.

### Phase 3 — unify the grid

`make_forecaster_grid()` is already shared. Fold the prod-only wrapper
columns (`as_of_policy`, `ahead_units`, `target_date_shift`,
`join_extra_data`, `filter_sources`, `excluded_geos`) into it with defaults
(`asof`, `days`, 3, FALSE, NULL, NULL) so explore rows get them for free and
prod's separate `left_join` metadata table disappears — each prod forecaster
tibble declares its own overrides inline. Standardize aheads on days at grid
build time (prod's `-1:3` weeks becomes `* 7` in the grid, not the runner),
retiring `ahead_units`.

### Phase 4 — pipelines consume it

Explore's `create_forecast_targets()` target body becomes
`map(dates, \(d) make_forecast_snapshot(...) |> run_forecaster(...))`; prod's
`forecast_nhsn`/`forecast_nssp` bodies become one snapshot target + one
`run_forecaster` call. Branching shapes stay as they are (explore:
per-forecaster; prod: forecaster × date) — no need to force them together.

### Order of work and verification

Do flu prod first (messiest), then covid, then rsv, then explore. Each phase
is independently landable. Verification: pin `FORECAST_REFERENCE_DATE`, run
`make prod-flu-backtest` before/after each phase and diff forecasts (oracle
captures via `scripts/oracle/capture.R`); explore side, diff `delphi_scores`
for a small forecaster subset with few dates. Note phase 0 rewrites archive
targets, which invalidates the S3-synced stores — budget one full recompute
per project and land it as its own commit.

### Decisions

- Canonical outcome name: `value` (decided 2026-07-13). Downstream artifacts
  (comparison/forecast/score notebooks, scoring targets) must be updated for
  the rename.
- `joined_latest_extra_data` demystified: it is the ILI+/flusurv historical
  seasons (the `select(hhs)` drops the nssp/gs/nwss columns; `filter(source
  != "nhsn")` leaves only those two sources), bound into `full_data` as
  augmented training rows for the `windowed_seasonal*` forecasters (which
  have `filter_sources = NULL`; linear/climate filter them back out). They
  are static finalized datasets with faux versions, so in phase 0 they fold
  into the canonical versioned archive — matching explore, and deleting the
  per-snapshot bind. Nit: as-of slicing excludes rows with `version >
  forecast_date`, unlike today's `versions_end` bind — check whether
  flusurv/ILI+ extend into the backtest window (difference is small and more
  honest if so).

### Phase 0 status (landed 2026-07-13, commit 00c5ba11 / stuvtpur)

Flu prod done: `nhsn_prod_archive` (season info, usa->us, Wednesday shift,
source stamp, geo drop, ILI+/flusurv extras folded in as `version =
time_value` rows) and `nssp_target_archive` (nssp-as-outcome spoofing built
once). `joined_latest_extra_data` deleted. `full_data`/`forecast_nssp` are
now as-of slice + substitutions + metadata stamping. Raw archives kept on
purpose for `nhsn_latest_data`/`nssp_latest_data`/`truth_data` (Saturday
convention, raw column names for scoring/truth) and for `forecast_nhsn`'s
exogenous nssp (needs the `nssp` column name).

Behavior-preservation verified empirically against the cached store
(2026-07-13):

- Extras are strictly historical: ILI+ time_values end 2024-07-24, flusurv
  ends 2020-04-22 — both before the earliest forecast date (2024-11-20). So
  the cheating path's `time_value < generation_date` filter and the
  substitutions join are no-ops on the extras; the fold-in reproduces the
  old unconditional `versions_end` bind exactly.
- `flu_data_substitutions.csv` dates span 2025-01-01..2026-02-11 — no
  (geo, time) collision with the extras.
- Note for future agents: `aux_data/` and the `flu_hosp_prod` store ARE
  materialized locally (`make pull`), so cached upstream targets can be
  inspected with `tar_read(..., store = "flu_hosp_prod")` without network.
  Only `nhsn_archive_data`/`nssp_archive_data` (cue = always) refetch on
  `tar_make`, and that fetch is cheap (same as the 5-min polling scripts).
  Use `distrobox enter rocker --` for R.

Backtest verification (2026-07-14): full `make prod-flu-backtest` ran clean
post-refactor (9051 targets, 0 errors, 30m). Compared against the
pre-refactor store (backed up at `_local/flu_hosp_prod_pre_phase0_backup`):
all 51 shared targets — every `full_data_*`, `forecast_nhsn_*`, and
`forecast_nssp_*` per-(forecaster, date) target from the last three weekly
prod runs — are identical (row-order/attribute-insensitive compare). The
only diffs were the `forecast_*_full` tar_combine aggregates, trivially
larger because the backtest covers all dates while the old store held three
weeks. Phase 0 is behavior-preserving; the backup can be deleted.

### Phase 1 status (landed 2026-07-14, commits ztvpnwqv + utpnlmrn)

`make_forecast_snapshot(archive, forecast_date, generation_date, as_of_policy
= "asof", substitutions = NULL, extra_latest = NULL, cache_key = NULL, before
= Inf)` in `R/looping.R` — the extracted body of `epix_slide_simple`, which
now is `map(dates, make_forecast_snapshot) |> map(fn) |> bind_rows` (parquet
slide cache preserved, byte-identical cache files at the same path). Flu
prod's `full_data` (asof+substitutions / cheating) and `forecast_nssp`'s nssp
slice consume it. `before` was added beyond the sketch signature to preserve
explore's `min_time_value` and cache-path key. The `as_of` stamp was unified
as `min(forecast_date, versions_end)` — reproduces old prod (`forecast_date`)
and old explore (epix_as_of natural) since `forecast_date <= versions_end`
for all real dates (verified against the cached store). `other_keys` is
captured from the fresh slice, not hardcoded to `"source"`.

Verified: unit tests pass (80 PASS / 0 FAIL); ~12 (forecast_date,
generation_date) pairs including substitution-active and holiday weeks match
the old inline logic and the actual cached `full_data_*` targets under both
policies, with matching metadata; explore old-vs-new identical for 3 dates,
cached and uncached, finite and Inf `before`. Full backtest skipped as
redundant with the store comparison.

Notes for later phases:

- `forecast_nhsn`'s exogenous nssp slice (raw `nssp_archive_data`) was left
  inline: its cheating branch cuts off at the nominal `forecast_date`, not
  `generation_date`, and stamps no `as_of`/`other_keys` metadata — a real
  divergence from the other slices on holiday weeks. Reconcile in phase 2
  when routing exogenous sources through the shared snapshot (decide whether
  the old cutoff was a bug).
- Covid prod (`scripts/covid_hosp_prod.R`) is still pre-phase-0 (uses
  `Sys.Date()`, in-target munging, no canonical archives); there is no rsv
  prod script yet. Each needs its own phase-0 canonicalization before
  adopting the shared snapshot/runner.

### Phase 2 status (landed 2026-07-14, commits ywsoqrvx + nmmkwvuv)

`run_prod_forecaster` → `run_forecaster(snapshot, forecaster, aheads, params,
param_names, id, ahead_units = "weeks", target_date_shift = 0L,
join_extra_data = FALSE, extra_data = NULL, filter_sources = NULL,
excluded_geos = NULL, sort_quantiles = FALSE)` in
`R/targets/forecaster_runner.R` (renamed file). Both pipelines call it: flu
prod `forecast_nhsn`/`forecast_nssp` with named args; explore inside
`create_forecast_targets`, mapped over dates via `make_forecast_snapshot`.
New grid spec columns in both explore scripts: `sort_quantiles` (flu TRUE,
covid FALSE — replaces the `g_disease == "flu"` grep) and `output_scale`
(flu "per100k", covid "count" — replaces the score-time population-column
sniff; empirically equivalent).

Deviations from the sketch, on purpose:

- The `g_time_value_adjust` shifts in `create_forecast_targets` are KEPT —
  they perform the real Wednesday→Saturday alignment for explore's
  scores/forecasts; deleting them changes verified outputs.
  `ensemble_targets` never referenced `g_time_value_adjust` (the plan's
  mention was speculative). Net: zero usages deleted.
- Runner output is intentionally NOT uniformly Saturday: prod's
  `target_date_shift` is per-forecaster (cdc_baseline/linear/climate use 0,
  seasonal/day-unit forecasters use 3), matching cached prod values. Explore
  uses the default 0 and shifts downstream as before.

Verified: unit tests at baseline (80 PASS); prod runner reproduces cached
`forecast_nhsn_*` exactly for deterministic forecasters (incl. extra-join +
geo-exclusion cases; stochastic ones match under same seed); explore
function-level maxd=0 on forecasts and scores for 2 flu + 1 covid forecasters
× 3 dates against cached explore stores. Backtest skipped as redundant.

Known gaps made explicit rather than fixed (outputs preserved):

- `output_scale` is per-disease, so like the old sniff it unscales ALL flu
  forecasters at scoring, including `pop_scaling = FALSE` ones — future fix
  is per-forecaster values.
- `forecast_nhsn`'s exogenous nssp slice stays inline with an explicit
  `cheating_cutoff = forecast_date` local + comment: judged a mild latent
  bug (cheating cutoff at nominal date vs `full_data`'s generation date;
  differs only on holiday weeks). Can't route through
  `make_forecast_snapshot` until the asof/cheating branches agree on which
  date governs.

### Phase 3 status (landed 2026-07-14, commits pppxoxus + oymnnwmr)

Spec columns unified in `make_forecaster_grid()` via a
`FORECASTER_SPEC_DEFAULTS` constant in `R/utils.R` (`as_of_policy = "asof"`,
`ahead_multiplier = 1L`, `target_date_shift = 0L`, `join_extra_data = FALSE`,
`filter_sources = NULL`, `excluded_geos = NULL`, `sort_quantiles = FALSE`,
`output_scale = "count"`). Prod's separate metadata `left_join` table is
deleted — each flu prod forecaster tibble declares overrides inline. Explore
per-script `mutate` stamping deleted: covid rides the defaults; flu declares
`sort_quantiles = TRUE` / `output_scale = "per100k"` in
`get_flu_forecaster_params()` after `add_id` (spec columns stay out of the
id hash; ids unchanged).

Deviation: the sketch's "standardize aheads on days, retire ahead_units" is
not achievable byte-identically — prod mixes week-native forecasters
(baseline/linear/climate compute `target_end_date = reference + ahead*7`)
with day-native ones, and the weekly `aheads` target `-1:3` is also consumed
by the ensembles (`seq(min,max)`, `/7`). Instead: per-forecaster
`ahead_multiplier` (1 or 7) applied at the call site
(`aheads * ahead_multiplier`), and `run_forecaster` dropped `ahead_units`
entirely — the runner is unit-agnostic, honoring the sketch's intent.

Verified: tests at baseline (80 PASS) after each commit; grid-information
equivalence old-vs-new for flu prod + flu/covid explore (rows, ids, params,
spec columns; all three manifests still build); old-vs-new runner
byte-identical incl. `target_end_date` on cached snapshots for shift-0,
shift-3, and cheating+extra-join forecasters; explore recompute maxd=0 vs
cache (crushing.papillon, 2 dates). Backtest skipped as redundant.

### Phase 4 status (landed 2026-07-14, commits mvmmzvzl + wxnolmzv)

Flu prod now has the target shape the sketch asked for: one snapshot target
per input feeding a single `run_forecaster` call. `nssp_forecast_data` (as-of
slice of `nssp_target_archive` via `make_forecast_snapshot`) was hoisted out
of `forecast_nssp`, mirroring `full_data`/`forecast_nhsn`; the only inline
prep left in the forecast targets is exogenous `extra_data` wrangling (the
nhsn-spoofed-as-nssp rename, and `forecast_nhsn`'s raw nssp slice with its
deliberate `cheating_cutoff = forecast_date` divergence — see phase 1/2
notes). Explore's side was already in this shape since phase 2
(`create_forecast_targets` maps `make_forecast_snapshot |> run_forecaster`
over dates). Branching shapes unchanged, per the sketch. A README section
documents the shared spec/runner wiring for forecaster authors.

Verified (2026-07-14): full `make prod-flu-backtest` post-refactor ran clean
(10,416 targets, 0 errors) and was compared against the pre-phase-4 store
backup (`_local/flu_hosp_prod_pre_phase4_backup`): all 7,764 shared
forecast-family targets (`full_data_*`, `forecast_nhsn_*`, `forecast_nssp_*`,
ensembles/aggregates) are hash-identical. The 688 only-new targets are
exactly the hoisted `nssp_forecast_data_*` (8 forecasters × 86 dates). The
317 differing targets are all downstream of the cue-always archive refetch,
not the refactor: `nhsn_latest_data` (same row count, 790 revised values —
routine NHSN history revisions), `truth_data_*`/scores (inherit those
revisions; forecasts unchanged), and `make_*submission_csv_*` (cue-always
no-op in backtest mode; stores a cli message id). The backup can be deleted.

Remaining from the plan's "order of work": covid prod (still pre-phase-0:
`Sys.Date()`, in-target munging, no canonical archives) and rsv prod (no
script yet) each need their own phase-0 canonicalization before adopting the
shared snapshot/runner.

### Carried over from ds/refactor (2026-07-15)

The earlier `ds/refactor` branch (abandoned) explored a different architecture
(`flu_assemble` + an `outcome_signal` grid dimension, honest per-signal input
with no nhsn/nssp spoof, an R-loop replacing `tar_map` — the last reverted on
that branch itself). Its `REFACTOR.md` retrospective is condensed here; the
generally useful changes were ported as commits on this branch:

- Semantic seeding (the one deliberate behavior change): flu prod targets now
  `set.seed(tar_seed_create(paste(id, signal, date, ahead, sep = "/")))` so
  renaming a target no longer silently moves the stochastic forecasters
  (linear, cdc_baseline, linear_no_population_scale). Self-check for any
  reseed: the 5 deterministic forecasters must stay bit-zero. TODO: port to
  covid (rsv has no prod script yet).
- `primary_source` arg on `scaled_pop_seasonal` (default `"nhsn"`,
  behavior-preserving): removes the hardcoded `source == "nhsn"` filters and
  threads to `run_workflow_and_format(source_value=)`. Enables a future
  de-spoof: stamp `source = "nssp"` honestly in `nssp_target_archive` and pass
  `primary_source = "nssp"` — a visible output change (the `source` column
  currently reproduces the lie), so do it as its own verified step.
- Flu evaluation is its own targets project/store (`flu_hosp_evaluation`,
  same script, dispatch on project name via TAR_RUN_PROJECT falling back to
  TAR_PROJECT): replays can't invalidate the weekly prod cache, retiring the
  `_local/*_backup` store-copy dance used in phases 0/4. `make eval-flu`
  replaces `make prod-flu-backtest`; `EVALUATION_N_DATES=<n>` replays only the
  last n dates (used by oracle captures). BACKTEST_MODE remains covid/rsv-only.
  NB: earlier phase notes saying `make prod-flu-backtest` now mean
  `make eval-flu`, and old replay targets still sit in the `flu_hosp_prod`
  store until it is pruned or re-pulled.
- Makefile: `*-log` recipe duplicates folded into tee'd recipes with global
  pipefail (deploy calls `tar_make` directly, so nothing depended on
  `prod-log`). Gotcha found while porting: a repo `.Renviron` overrides
  shell-exported `TAR_PROJECT` when Rscript starts (here it pins
  `flu_hosp_explore`) — recipes that must select a project reliably need
  `Sys.setenv(TAR_PROJECT=)` inside the R call or TAR_RUN_PROJECT (not in
  .Renviron); the flu prune recipes do this, the covid/rsv/explore prune
  recipes still have the latent footgun.

Durable findings from the retrospective still worth acting on:

- `sort_by_quantile()` is applied inconsistently: explore sorts all forecaster
  output (now the `sort_quantiles` spec column), covid prod sorts, flu prod
  sorts only inside ensembles and `forecaster_climatological`. Either flu prod
  ships crossing quantiles or the workaround is unnecessary — resolve by
  asserting monotonicity at the forecaster output boundary.
- Alignment consolidation (their "Exp 6"): phase 0 centered flu prod's
  archives, but covid prod still applies the Wednesday shift and `/100` scale
  in scattered places, and the output denormalize (`target_end_date + 3`) is
  copy-pasted per forecaster across diseases. Normalize once at archive
  construction (`time_value` only — never `version = time_value` for sources
  with real revisions), denormalize once post-forecast.
- Contracts to enforce: version faithfulness (no assembled row with
  `version > generation_date`, asserted for as-of rows only — cheating
  intentionally peeks); a `validate_model_frame()` at the snapshot boundary
  instead of raw `attributes()<-`; forecaster output shape (keys, monotone
  quantiles, no NAs, non-negative).
- `targets` metaprogramming truths (verified against targets 1.11.4): tar_map
  does NOT strip names from list-columns, so the parallel `param_names` grid
  column is deletable; the real wall is `tar_target`'s default
  `tidy_eval = TRUE` (a bare `!!!params` splices at build time — set
  `tidy_eval = FALSE` to defer); PR #169 broke caching because commands
  dereferenced a grid *global* at run time, making the whole grid a dependency
  of every branch — `tar_map(values=)` substituting each row as a literal is
  the safe form; `rlang::syms` on the trainer column is load-bearing.
- Explore/prod unification: the data layer and (since phase 2-3) the
  runner/grid are shared; fan-out stays different on purpose (prod: one target
  per cell for caching/crew/seeds; explore: dates batched inside a target via
  the slide cache). "Share the cell, keep two fan-out strategies." Remaining
  gap: per-source version policies for explore-style multi-source snapshots.
- Prod parallelism worry is likely BLAS oversubscription (crew spawns
  `detectCores()-4` workers while BLAS may also multithread) — pin BLAS to one
  thread per worker and measure before restructuring.
- Oracle practice: success for a behavior-preserving step is an empty golden
  diff; never mix a refactor with a bug fix (the golden faithfully reproduces
  current bugs). Cheap companion for pure code moves: diff
  `targets::tar_manifest()` between revisions — byte-identical commands mean
  no invalidation. Compare-script gotchas: relative diffs explode when the
  baseline is 0 (read the absolute diff); a metric missing from `value_cols`
  silently becomes a sort key and scrambles row alignment. Old capture labels
  under `cache/oracle/` predate this branch's target renames — re-baseline
  before relying on them.

### Open decisions

- (resolved in phase 2) Score-time population-column sniffing is now the
  `output_scale` spec column; per-forecaster values remain a future fix.
