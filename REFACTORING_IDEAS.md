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
