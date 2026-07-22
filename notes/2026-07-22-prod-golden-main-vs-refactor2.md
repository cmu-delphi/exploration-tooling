# 2026-07-22: prod golden test, main vs ds/refactor2 (covid + flu)

Full main-vs-branch golden: ran the covid and flu prod pipelines for the real
forecast Wednesday (2026-07-22) on both `main@origin` (bcd7501) and the
`ds/refactor2` tip (zqvqkpmq), and attributed every difference. **Verdict: no
unexplained diffs on either disease.** Every change traces to an intentional,
already-documented behavior change on the branch.

## Procedure (reusable for main-vs-branch goldens)

Main has no `FORECAST_REFERENCE_DATE`, no evaluation projects, and no oracle
scripts, so the usual pinned-replay golden can't run there. Instead:

- Both runs on the actual forecast Wednesday, so main's bare `Sys.Date()` and
  the branch's pinning resolve to the same forecast/generation date.
- Two jj workspaces (`jj workspace add --name golden-main -r main@origin ...`),
  so each side has its own checkout, fresh `covid_hosp_prod`/`flu_hosp_prod`
  store, and own `cache/` — the real prod stores were never touched. Shared via
  symlink/copy: `renv/library` (renv.lock is identical between the revisions),
  `.Renviron`, `aux_data`.
- Both sides built only `local_forecasts_and_ensembles_nhsn/nssp` via
  `tar_make(names = ...)` (skips notebooks and external-forecast S3 targets;
  ~2 min per run). Launched simultaneously so the cast-API archive fetches
  happen at the same moment — confirmed effective: `nhsn_archive_data` came
  back bit-identical between the runs on both diseases, `joined_archive_data`
  bit-identical on flu, i.e. zero intraday data drift to confound the diff.
- Captures via the branch's `scripts/oracle/capture.R` with
  `ORACLE_SKIP_MAKE=TRUE` (copied into the main workspace; it only reads the
  store, so it runs fine against main's build). Labels under `cache/oracle/`:
  `{covid,flu}_hosp_prod:today-main` and `:today-refactor2`.
- `oracle/compare.R` for the headline, then a schema-normalizing aligned diff
  for attribution (main's frames need `quantile = coalesce(quantile,
  quantile_value)`; branch covid frames add `source`).

## Attribution — covid

- `nhsn_archive_data` bit-identical; `nssp_archive_data` differs by exactly 79
  rows, all `wy` `nssp = NA`, present only on main — the `vlnyvwnp` NA-row
  archive fix, nothing else. All 79 are superseded by the 2026-01-14 backfill
  at today's as-of, so the covid *snapshot* is identical → no forecast
  footprint from the fix.
- `climate_base`, `climate_geo_agged`: bit-zero.
- Seasonal family (`windowed_seasonal[_latest]`, `windowed_seasonal_extra_sources`,
  `seasonal_nssp_latest`): every changed task is an exact within-task multiset
  permutation — the `sort_quantiles` opt-in.
- Stochastic three (`cdc_baseline`, `linear`, `linear_no_population_scale`):
  value changes from semantic seeding (`yprtznnv`), magnitudes plausible
  (max ~74 admissions at nhsn tail quantiles).
- Ensembles (`climate_linear`, `ens_ar_only`, `ensemble_mix`): zero changed
  (geo, date) tasks outside the union of changed-component tasks. Landmine:
  `ensemble_mix`'s effective component set is whatever the active
  `covid_geo_exclusions.csv` block weights (2026-01-07: linear 3,
  climate_base 2, climate_linear 0.001, windowed pair 3/0.05), NOT just the
  windowed pair in its spec — with only the spec components the containment
  check false-alarms on 198 tasks.

## Attribution — flu

Same classes, plus three flu-only wrinkles:

- **Rename**: main `seasonal_nssp_latest` = branch `seasonal_nssp_cheating`
  (id + as-of-policy rename; compared by mapping the ids).
- **wy NA fix has a live footprint on flu** (unlike covid): 5 of main's 83
  wy NA archive rows are still NA at the latest version, so main's
  as-of-today nssp snapshot has 5 wy NA rows the branch drops. The pooled
  nssp-trained forecasters shift genuinely on ALL tasks, but tiny:
  `windowed_seasonal_extra_sources` nssp side max 0.038 pp;
  `seasonal_nssp_cheating` nssp side max 0.0043 pp after separating out
  sorting. Same mechanism as the 2026-01-07 probe in the refactoring log.
- **Permutation + shift compose**: a task that both crossed (sorting) and
  shifted (wy) fails the naive multiset test with a scary raw max (~250
  admissions on the national nhsn tail), but comparing *sorted* within-task
  vectors decomposes it: nhsn-frame `seasonal_nssp` and
  `windowed_seasonal_extra_sources` are sorted-diff exactly 0 (pure
  reordering); only the nssp side carries the small real shift.
  Also: classify permutations per frame — the nhsn/nssp frames share task
  keys, and pooling them mixes admissions with percent units and breaks the
  multiset test.
- Everything else matches covid: climates bit-zero, stochastic three seeded,
  `windowed_seasonal` pure permutations, all three ensembles fully contained
  (active flu weight blocks: nhsn 2026-01-07, nssp 2025-11-19).

## Leftovers / follow-ups

- Captures kept under `cache/oracle/{covid,flu}_hosp_prod/today-{main,refactor2}-*`
  as the main-vs-branch reference for this week.
- Workspaces forgotten and deleted; their working-copy commits abandoned.
- Main's frames still ship the stray `quantile_value` column (pre-`rtqptkow`
  cdc_baseline naming bug) — visible schema-level confirmation that the branch
  fix is real.
- A main-side golden is only possible on the actual forecast day; once
  ds/refactor2 lands, both prod pipelines are pinnable via
  `FORECAST_REFERENCE_DATE` and this dance is unnecessary.
