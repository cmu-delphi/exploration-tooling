# Ensemble-layer consolidation log (started 2026-07-19)

Plan (scoped 2026-07-18 session): give the prod ensemble layer the same
treatment the forecasters got — E0 shared factory, E1 spec + runner with
contracts, E2 sweepable ensembles, E3 cleanup — followed by the covid honest
nssp de-spoof and per-forecaster `output_scale`. All steps behavior-preserving;
the known asymmetries stay as explicit config until adjudicated separately.

## E0 — shared ensemble target factory (`skvtwnxz`)

The flu/covid prod ensemble `tar_map`s (~230 lines each, ~95% identical
hand-copies) are hoisted into one `build_prod_ensemble_targets()` in
`R/targets/prod_shared.R`. Per-disease asymmetries became named arguments,
substituted into commands as literals via extra `tar_map` values columns (so no
run-time global carries them and no new dependency edges appear):

- `geo_exclusions_file` / `nssp_geo_exclusions_file`: weights csv target names
  (spliced as symbols via `rlang::syms`).
- `clim_lin_max_weights_nhsn/_nssp`: c(ahead, quantile) climate caps. Flu nhsn
  historically passed no caps = the `ensemble_climate_linear` defaults
  c(0.9, 1); flu nssp and both covid signals c(0.6, 0.6). Verified against the
  function defaults before unifying.
- `ar_drop_negative_aheads`: which signals drop negative aheads from the AR
  components before `ensemble_mixture` (flu: "nssp", covid: "nhsn").
- `climate_submission_excluded_geos`: flu-only c("as","gu","mh") filter in the
  climate submission (was `g_excluded_geos`, global deleted; covid gets
  `character(0)` — a no-op filter, the only textual change to covid's
  behavior-neutral path).

Other textual unifications (all value-preserving): flu's inline nhsn/nssp
clim-lin blocks -> covid's `make_clim_lin` local fn with caps as args; the two
one-line-comment neg-ahead filters -> a `make_ar(forecasts,
drop_negative_aheads)` local fn; submission target string -> `paste0("wk inc ",
disease, " prop ed visits")`; notebook filename sprintf gains a `%s` for
disease. The nhsn-derived `geo_exclusions` applied to both signals is kept
verbatim with a comment marking it as possibly-copy-paste (adjudicate in E1).

Verification:

- `tar_manifest()` pre/post for all four projects (prod + evaluation, flu +
  covid): identical target sets; commands changed only for
  `ensemble_clim_lin`, `ensemble_mixture`, `make_submission_csv`,
  `make_climate_submission_csv`, `notebook` (the rewritten ones). Everything
  else — all forecast targets, `forecast_filtered`, `geo_weights`,
  `geo_exclusions`, `ens_ar_only`, `forecasts_and_ensembles`, `validate_*`,
  `truth_data` — byte-identical, so no invalidation of the expensive caches.
- Spliced commands eyeballed in the manifest: caps, drop flags, disease
  literal, file symbols all correct per disease.
- `make test`: 93 pass, 0 fail (9 pre-existing skips).
- Flu golden: capture `e0-ensemble` vs `flu-wy-probe2-vlnyvwnp` (ref
  2026-01-07, EVALUATION_N_DATES=1) — ALL MATCH, max rel diff 0, including
  `local_forecasts_and_ensembles_*`.
- Covid golden: capture `e0-ensemble` vs `multihead9-ootrkxzt` (ref
  2026-02-18, EVALUATION_N_DATES=9) — ALL MATCH, max rel diff 0.

Both `e0-ensemble` captures double as the baselines for E1 (same pinned
reference dates and date counts).

## E1 — ensemble spec + runner with output contracts (`lpvvrkwy`)

Preceded by a small cleanup commit (`xvzyklmp`): deleted the dead
`make_ensemble_grid` in `R/utils.R` (no callers anywhere in `R/`, `scripts/`).

The three ensemble command bodies inside `build_prod_ensemble_targets()`
(`ensemble_clim_lin`, `ens_ar_only`, `ensemble_mixture`) are replaced by calls
to a new shared `run_ensemble()` (`R/targets/ensemble_runner.R`, the ensemble
analog of `run_forecaster()`), driven by a declarative per-disease
`g_ensemble_specs` (a named list of three rows -- `climate_linear`, `ar_only`,
`ensemble_mix` -- each with `id`, `method`, per-signal `components`, and
per-signal/method params: `climate_caps`, `apply_geo_exclusions`,
`drop_negative_aheads`, `sort_quantiles`) declared in `scripts/flu_hosp_prod.R`
/ `scripts/covid_hosp_prod.R` and passed into `build_prod_ensemble_targets()`
as a single `ensemble_spec` argument. `clim_lin_max_weights_nhsn/_nssp` and
`ar_drop_negative_aheads` (E0 factory arguments) migrated into the spec, since
they're per-ensemble parameters; `geo_exclusions_file`,
`nssp_geo_exclusions_file`, `disease`, and `climate_submission_excluded_geos`
stayed factory arguments.

`run_ensemble()` owns: asserting every declared component is present in the
input (`assert_components_present()` -- turns the silent
`mean(na.rm = TRUE)`-over-missing-component failure mode into a loud error),
method dispatch (`climate_linear` / `mean` / `weighted`), geo-exclusion
filtering, quantile sorting, `mutate(forecaster = id)`, and finally
`validate_forecast_output(id)`. Behavior-parity constraints from the task were
followed verbatim:

- `climate_linear` receives the FULL per-signal `forecast_filtered` frame, not
  pre-filtered to `components` -- `ensemble_climate_linear()` computes its
  `last_data`/`forecast_date` latency from the input before it filters
  internally via `grepl("climate|linear", forecaster)`; `components` is used
  only for the presence assert. Documented in `run_ensemble()`'s roxygen.
  Flu nhsn climate caps stayed `c(0.9, 1)` (the historical uncapped default);
  flu nssp and both covid signals `c(0.6, 0.6)`.
- `ensemble_mix`'s AR rows drop negative aheads only per the pre-existing
  per-disease/signal asymmetry (flu: nssp, covid: nhsn), and it does NOT sort
  quantiles at the end (`sort_quantiles = FALSE` in its spec row) -- kept
  exactly as before.
- The nhsn-derived `geo_exclusions` target is still applied to both signals'
  `climate_linear` only (pre-existing, possibly-copy-paste; left verbatim,
  comment kept).

**tar_map metaprogramming pitfall hit and fixed**: the ensemble spec is
disease-constant (not per forecast-date), so it's spliced into every branch's
frozen command as a single literal list-column (`ensemble_spec = list(.env$ensemble_spec)`),
same pattern as E0's per-disease literals. First attempt named the spec's
`ens_ar_only` row and its `geo_exclusions` boolean field to match the *target*
names `ens_ar_only` and `geo_exclusions` in the same `tar_map`. `tar_map`'s
per-branch symbol substitution renames any bare identifier on the RHS of a
`$` accessor that textually matches a sibling target name -- it doesn't
distinguish "symbol referencing a real target" from "field name of a spliced
literal that happens to share that word". So `spec$ens_ar_only` and
`spec$geo_exclusions` silently became `spec$ens_ar_only_2026.07.01` /
`spec$geo_exclusions_2026.07.01` in the frozen command, which don't exist on
the literal list, and `ens_ar_only` failed at runtime with `EXPR must be a
length 1 vector` (the switch on a NULL `method`). Fixed by renaming the
colliding spec keys/fields to non-colliding names: `ens_ar_only` -> `ar_only`
row key, `geo_exclusions` -> `apply_geo_exclusions` field (the ensemble's
stamped `id` stays `"ens_ar_only"`, so output is unaffected). General
takeaway for future spec-splicing: literal list keys/field names accessed via
`$` inside a `tar_map` command must avoid every sibling target name in that
same map, not just avoid being bare symbols.

Verification:

- `tar_manifest()` pre/post (baseline = E0's manifests) for all four projects:
  identical target sets; commands changed only for `ensemble_clim_lin`,
  `ens_ar_only`, `ensemble_mixture` (3 per prod project, 3 forecast dates each
  for the evaluation projects = 9). Everything else byte-identical.
- Spliced `ensemble_mixture` command inspected in both flu and covid
  manifests: `climate_caps`, `drop_negative_aheads`, and `id`/`method` fields
  all correct per disease.
- `make test`: 93 pass, 0 fail, 9 skips.
- Flu golden: capture `e1-ensemble` (ref 2026-01-07, EVALUATION_N_DATES=1) vs
  `e0-ensemble` -- ALL MATCH, max rel diff 0.
- Covid golden: capture `e1-ensemble` (ref 2026-02-18, EVALUATION_N_DATES=9)
  vs `e0-ensemble` -- ALL MATCH, max rel diff 0. No `validate_forecast_output`
  failures observed on either replay (in particular `ensemble_mixture`, which
  runs unsorted, produced no quantile-crossing errors on these dates).

## E3 — schema validation for the hand-edited prod weights csvs (`kqouxowm`)

`parse_prod_weights()` (`R/utils.R`) previously trusted the four hand-edited
`scripts/*_geo_exclusions.csv` files completely -- a typo'd forecaster id or
geo code just produced a weights row that silently never joined to anything
downstream. Added two validation helpers, called from inside
`parse_prod_weights()`:

- `validate_prod_weights_columns(raw, filename)` -- runs on the whole raw
  file (not just the requested date's rows): required columns present
  (`forecast_date`, `forecaster`, `geo_value`, `weight`); `forecast_date`
  parseable (readr guesses the whole column as character if any single entry
  doesn't match a date format, and `as.Date()` throws rather than returning
  `NA` on a totally unrecognized string, so this parses element-wise and
  reports the offending value(s)).
- `validate_prod_weights_values(weights, filename, forecaster_fn_names,
  all_geos)` -- runs only on `useful_prod_weights`, the post-date-filter
  subset that actually drives this call (only one date block is ever
  consumed per call; other historical blocks are inert and deliberately not
  checked, see finding below): `weight` numeric and finite; `weight >= 0`
  (the only well-defined range -- `ensemble_weighted()` and
  `ensemble_climate_linear()` renormalize weights as relative mass within a
  group, so there's no natural upper bound and negative weights would corrupt
  that renormalization; `exclude_geos()` also relies on `weight` bottoming
  out at exactly 0 for its exclusion test); `forecaster` in
  `forecaster_fn_names` (the grid's `g_forecaster_params_grid$id`, the actual
  argument already threaded through) plus the sentinel `"all"`; `geo_value`
  in the same state list `parse_prod_weights()` already builds, plus `"all"`.

**Finding -- the weights csvs reference forecaster ids that don't exist in
the current grid, by design.** `ensemble_climate_linear()` doesn't join
`other_weights` against a registered forecaster list; it matches by
`grepl("climate|linear", forecaster)` against whatever ids are actually
present in the forecast frame it's given. So all four csvs carry
`"linearlog"` and `"climate_quantile_extrapolated"` (retired forecasters,
kept at weight 0 as inert documentation across essentially every date block)
and `"climate_linear"` (predates the `climate_linear` ensemble target of the
same name -- never a base forecaster id, appears at a small nonzero weight
like 0.001 in several blocks, always inert). A strict
"forecaster must be in `g_forecaster_params_grid$id`" rule would reject all
four current files. Per the task's guidance, loosened rather than edited the
csvs: added `LEGACY_PROD_WEIGHT_FORECASTER_IDS <- c("linearlog",
"climate_quantile_extrapolated", "climate_linear")` (in `R/utils.R`,
documented inline) and whitelisted it alongside the active ids and `"all"`.

**Finding -- one real, pre-existing typo, currently harmless.**
`scripts/flu_nssp_geo_exclusions.csv`'s 2025-12-17 block has
`linear_no_population-scale` (hyphen) instead of the real id
`linear_no_population_scale` (underscore), at weight 2 (nonzero, i.e. not a
deliberately-zeroed placeholder). It's inert today only because that date
block is stale (superseded by later blocks for any current forecast date);
it would be caught loudly by this validation if `2025-12-17` were ever
re-requested (e.g. a historical replay). Left as-is per the task's
instructions (validation scope is intentionally the post-date-filter subset,
not the full historical file, precisely so a stale mistake like this doesn't
block current runs) -- worth a manual fix in the csv separately since it's a
one-off, not a documented convention like the legacy ids above.

Verification:

- `make test`: 105 pass, 0 fail, 9 skips (was 93/0/9; added 12 tests in
  `tests/testthat/test-prod-weights.R` covering both helpers directly --
  offline, no network dependency -- plus one end-to-end
  `parse_prod_weights()` pass/fail-free check per csv against the current
  disease's forecaster id list).
- All four current csvs parse cleanly via `parse_prod_weights()` with a
  plausible date (2026-01-07) and each disease's real
  `g_forecaster_params_grid$id` list.
- Flu golden: capture `e3-weights` (ref 2026-01-07, EVALUATION_N_DATES=1) vs
  `e1-ensemble` -- ALL MATCH, max rel diff 0. Only `geo_weights` (and its
  downstream ensemble targets) recomputed, as expected for a
  `parse_prod_weights()` function-body change; every other target
  byte-identical. Covid golden skipped (low-risk, slow 9-date replay per task
  scoping).

## Covid nssp de-spoof (`mkvkxsqw`)

Closed the last roadmap item from CLAUDE.md's "Shared forecaster architecture"
section: covid's `nhsn_prod_archive` and `nssp_target_archive` (in
`scripts/covid_hosp_prod.R`) previously stamped no `source` column at all, so
`scaled_pop_seasonal`'s missing-source fallback (`epi_data$source <- "nhsn"`,
`forecaster_scaled_pop_seasonal.R`) silently mislabeled every covid-nssp row
"nhsn" -- the same lie flu fixed on 2026-07-16 (`kqnkmnsk` + `qzukolwz`, built
on `vpnzyuwp`'s `primary_source` parameterization). Mirrored flu's design
exactly, since all three changes live in shared `R/` code already exercised by
flu with a source column present:

- `nhsn_prod_archive`: added `source = "nhsn"` to the build mutate, changed
  `as_epi_archive(compactify = TRUE)` -> `as_epi_archive(other_keys = "source",
  compactify = TRUE)`.
- `nssp_target_archive`: same two changes, `source = "nssp"`.
- `forecast_nssp`: added `primary_source = "nssp"` to the `run_forecaster()`
  call, so `scaled_pop_seasonal` filters/joins on the honestly-stamped `nssp`
  rows instead of relying on the fallback. `forecast_nhsn` needed no change --
  `primary_source` defaults to `"nhsn"`, matching `nhsn_prod_archive`'s new
  stamp.
- `forecast_nssp`'s `full_data_modified <- full_data %>% rename(nssp = value)
  %>% select(geo_value, time_value, nssp)` already dropped every non-listed
  column (including the new `source`), so the exogenous join still adds only
  `nssp` -- no change needed there, unlike flu's version which needed an
  explicit `filter(source == "nhsn") %>% select(-c(source, ...))` because flu's
  archive carries extra non-nhsn source rows (ILI+/flusurv) that covid's
  doesn't.
- `run_forecaster()`'s `primary_source` injection (formals-sniffing) and
  `validate_forecast_output()` needed no covid-specific change -- both are
  already disease-agnostic shared code.

No other consumer needed touching: `make_forecast_snapshot()` reads
`other_keys` off the archive's own metadata rather than hardcoding it;
`min_train_date` filtering, the exogenous joins, `truth_data`, and scoring key
on `geo_value`/`time_value`/`forecast_date`/`target_end_date`, never `source`.
`nhsn_latest_data`/`nssp_latest_data`/`truth_data` deliberately still read the
*raw* (unstamped) archives, same as flu.

Verification:

- `tar_manifest()` pre/post (baseline = E3's `e3_*.rds`) for all four
  projects: identical target sets everywhere. flu prod and flu evaluation
  (EVALUATION_N_DATES=3) are byte-identical, 0 commands changed -- flu
  untouched. Covid prod: 11 commands changed (`nhsn_prod_archive`,
  `nssp_target_archive`, one `forecast_nssp_*` per forecaster family = 9).
  Covid evaluation (EVALUATION_N_DATES=3, 3 forecast dates): 29 changed, same
  2 archive targets + 9 forecasters x 3 dates of `forecast_nssp_*` -- exactly
  the targets whose command text changed, nothing else invalidated.
- `make test`: 105 pass, 0 fail, 9 skips (unchanged from baseline).
- Golden replay: `covid_hosp_evaluation` capture `despoof` (ref 2026-02-18,
  EVALUATION_N_DATES=9) vs the `e1-ensemble` baseline via
  `scripts/oracle/compare.R` -- as expected, 6 frames report "column sets
  differ" (`nhsn_prod_archive`, `nssp_target_archive`, `forecast_nhsn_full`,
  `forecast_nssp_full`, `local_forecasts_and_ensembles_nhsn/nssp`), everything
  else (`nhsn_archive_data`, `nssp_archive_data`, `nhsn_latest_data`,
  `nssp_latest_data`, `scores_nhsn`, `scores_nssp`) matches exactly
  (max rel diff 0) since those read the raw, unstamped archives. A
  purpose-built ad-hoc script (aligns rows on the non-source keys --
  archives: `geo_value, time_value, version, epiweek, epiyear, season,
  season_week`; forecast/ensemble frames: `geo_value, forecast_date,
  target_end_date, quantile, forecaster, ahead` -- then diffs `value`)
  confirmed all 6 differing frames are bit-identical on every value column
  (`max abs diff = 0`, row counts match exactly on the join, no
  many-to-many blowup) and the only change is the `source` column:
  - `nhsn_prod_archive`/`nssp_target_archive`: every row now stamped
    `"nhsn"`/`"nssp"` respectively (previously absent).
  - `forecast_nhsn_full`/`forecast_nssp_full` and the local ensembles:
    `windowed_seasonal`/`windowed_seasonal_latest` carry the stamped source
    through to output (`"nhsn"` on the nhsn side, `"nssp"` on the nssp side,
    matching `scaled_pop_seasonal`'s `adding_source` guard which now sees a
    real `source` column and no longer drops it); every other forecaster
    (`cdc_baseline`, `linear`, `linear_no_population_scale`, `climate_base`,
    `climate_geo_agged`, `windowed_seasonal_extra_sources`,
    `seasonal_nssp_latest`) shows `source = NA` in both old and new captures,
    unaffected since they either never touch `source` or (the two
    `join_extra_data = TRUE` rows) have it stripped unconditionally by
    `run_forecaster()`. This pattern is identical to flu's post-de-spoof
    output schema (verified by code inspection, not a fresh flu replay, since
    flu's manifest is untouched).

## output_scale investigation (no code change; docs only, `zmoyxqkw`)

Looked at the roadmap "make output_scale per-forecaster" item since scoring
allegedly unscales flu `pop_scaling = FALSE` forecasters wrongly. Empirically
it does NOT: flu training `hhs` is per100k at archive build
(`R/targets/flu_data_targets.R:36`, `hhs = hhs / population * 1e5`), so every
current flu explore forecaster outputs per100k regardless of `pop_scaling`
(which here means "no *additional* scaling", not "counts"). Magnitude check
against the flu_hosp_explore store: raw median forecast ~1.4-1.9;
`pred * pop / 1e5` ~55-73, matching count-truth median 63 across the
scaled_pop_season, climate_linear, and scaled_pop_exogenous families. So the
blanket `x$output_scale <- "per100k"` (`R/targets/flu_forecaster_config.R:318`)
is correct today. Covid input is never per100k-converted; covid config omits
output_scale (defaults "count"); covid forecasters output counts — also correct.
output_scale is consumed only by the explore `score` target
(`R/targets/shared_utils.R:99`), not prod scoring.

Conclusion: not a live bug. The naive "fix" (derive output_scale from
`pop_scaling`/`population_scale`) would flip correctly-per100k forecasters to
"count" and multiply flu scores by ~pop/1e5 — strictly worse. The only
honest form of "per-forecaster output_scale" is explicit per-family
declarations (all "per100k" for flu today), which is behavior-preserving and
only removes the footgun for a future count-output flu forecaster (e.g. the
E2 prod-component family). Deferred as low priority; CLAUDE.md roadmap wording
corrected to match. Also dropped the now-done covid de-spoof roadmap bullet.

## param_names grid column deletion (`qvqoylsp`)

Roadmap "minor" item. Verified empirically first that `tar_map` substitutes
list-column values as literals with names intact (tiny synthetic tar_map,
deparsed frozen commands show `list(x = 1, ...)`), so `params` already carried
its names and `set_names(params, param_names)` was pure duplication. Dropped
the column from `make_forecaster_grid()`, the argument from `run_forecaster()`
and `get_partially_applied_forecaster()` (now `!!!params`), and the 5 call
sites; `primary_source` injection names its list element directly.

Unlike E0/E1 this changes command text for every forecast target (the
`param_names=` argument disappears), so no manifest-diff check — verification
is the golden alone.

- `make test`: 105 pass, 0 fail, 9 skips (unchanged).
- Flu golden: capture `param-names` (ref 2026-01-07, EVALUATION_N_DATES=1) vs
  `e3-weights` — ALL MATCH, max rel diff 0, all 13 frames.
