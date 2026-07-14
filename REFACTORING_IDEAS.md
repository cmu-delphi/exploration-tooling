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

### Open decisions

- Score-time population-column sniffing (review 5) rides along in phase 2 as
  a spec column (`output_scale`).
