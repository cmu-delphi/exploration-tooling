# E2 design: make the shipped prod ensembles sweepable in explore (NOT YET IMPLEMENTED)

Status: design only, deferred. Not a current priority (decided 2026-07-19).
This is the record of a read-only design scout so we can pick it up later.

## The gap

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

## Correctness landmine

`ensemble_climate_linear()` computes `last_data <- min(forecasts$target_end_date)`
and `forecast_date <- min(forecasts$forecast_date)` over the *whole* frame it is
given (`R/forecasters/ensemble_linear_climate.R:24-26`). Called on a multi-date
frame it collapses every date's latency to the earliest date's — silently wrong.
`weighted`/`mean` methods key on `forecast_date`/`ahead` explicitly and are safe,
but to stay faithful the explore ensemble command should call `run_ensemble()`
per date regardless of method. (Mirrors explore's existing per-date loop, so no
new idiom.)

## Proposed structure

### Atomic components family
Add `prod_ensemble_components` to `get_{flu,covid}_forecaster_params()`, copied
verbatim from the prod grid rows. Two traps:
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

### Ensemble sweep grid + targets
New file `R/targets/explore_ensemble_targets.R` with
`build_explore_ensemble_targets(...)`, called from the explore scripts after
`create_forecast_targets()`. Key decisions:
- Grid should be **one row per full `g_ensemble_specs`-shaped variant** (weights
  source × climate caps × component set), each row internally the 3-stage
  prod shape (climate_linear → ar_only → weighted-mix-with-`extra_forecasts`),
  NOT a flat one-row-per-method tibble. Reason: prod's `ensemble_mix` feeds
  `ensemble_clim_lin`'s *output* into `ensemble_weighted()` as `extra_forecasts`
  — the mix is climate_linear + weighted-AR, so a faithful sweep must reproduce
  that dependency order (open question #2 from the scout, promoted to a
  requirement).
- `component_forecasts_for_ensemble`: one plain `tar_target` binding the six
  literal-id `forecast_<id>` targets by symbol; shared input to every sweep
  branch. Filter to one `forecast_date` inside the branch before `run_ensemble()`.
- `ensemble_forecast` sweep `tar_map(values = grid, names = id)`; `ensemble_score`
  duplicates (does NOT edit) the ~15-line scoring body from
  `create_forecast_targets()` so the existing `score` target's frozen command is
  untouched; `tar_combine` into `ensemble_forecasts`/`ensemble_scores`.

### Weights
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

### Report wiring
Extend `joined_forecasts`/`joined_scores` (`R/targets/shared_utils.R`) to
`bind_rows(ensemble_forecasts/scores)`. This changes their command text so they +
`overall_notebook` + per-family notebooks recompute — cheap `bind_rows`/render,
not the 3h sweep. `forecaster_lookup()` unaffected (reads current grid).

## Scope boundaries
- **nhsn/hhs signal only.** Explore has no nssp-as-target archive/snapshot/scoring
  path (prod-only). Wiring nssp-as-target into explore is a materially bigger
  lift — named follow-up, do not half-implement.
- **`climate_linear_ensembled` retirement is out of scope.** Once E2 lands it and
  the new `prod_climate_linear*` sweep rows sit side-by-side in `overall_notebook`
  — exactly the comparison needed to later judge retirement safe.

## Invalidation summary (verify via tar_manifest diff, E0/E1 methodology)
- Expected changed (cheap): `forecaster_params_grid`, `joined_forecasts`,
  `joined_scores`, `overall_notebook`, per-family `notebook_*`.
- Expected byte-identical: every pre-existing `forecast_<id>`/`score_<id>`,
  `delphi_forecasts`, `delphi_scores`, all data/external targets.
- New: 6 component `forecast_*`/`score_*`, `component_forecasts_for_ensemble`,
  the sweep branches, `ensemble_forecasts`, `ensemble_scores`.

## Risks / open questions (ranked)
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

## Files to touch when implemented
`R/targets/ensemble_runner.R`, `R/targets/prod_shared.R`,
`R/targets/shared_utils.R`, `R/targets/{flu,covid}_forecaster_config.R`,
`scripts/{flu,covid}_hosp_explore.R`, `R/utils.R` (parse_prod_weights, add_id,
make_forecaster_grid, FORECASTER_SPEC_DEFAULTS); source of truth for component
params + spec shape is `scripts/{flu,covid}_hosp_prod.R`.
