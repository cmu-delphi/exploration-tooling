# Revision-aware forecaster devlog

Running log of issue-specific investigations into the revision-aware
forecaster stack (`R/forecasters/forecaster_revision_aware.R`,
`R/targets/flu_forecaster_config.R` / `covid_forecaster_config.R`).
General architecture is documented in CLAUDE.md under "Shared forecaster &
ensemble architecture" and in `notes/refactor-ideas.md`.
This file is for dated, narrative entries: what broke, how it was found, and
why the fix is what it is.
Point-in-time debugging state, not a description of current architecture.

## 2026-09-24/25: `pop_scaling` double-normalized flu's already-per-100k outcome, exploding `us` forecasts

### Symptom

Flu's `us` forecasts from several revision-aware explore variants
(`revision_aware_nssp`, `revision_aware_beds_no_season`,
`revision_aware_beds_seasonal`) were wildly off scale during peak season,
enough to dominate the aggregate `mean_wis` in
`rendered_reports/flu-prod-explore-comparison-2025_2026.html`
(`revision_aware_nssp` mean_wis 5866.96, `revision_aware_beds_seasonal`
6092.18, vs `revision_aware_beds_no_season` at a comparatively tame 640.64,
`FluSight-baseline` at 211.91).
`geomean_wis` for the same three forecasters was much closer together
(35.52, 40.00, 39.86) -- a large gap between `mean_wis` and `geomean_wis`
is the tell for a few catastrophic outliers rather than a systematic bias.

### Dead end: prelim/main NHSN signal timing

The first hypothesis (from an unrelated earlier question about flu
"not normalizing correctly") was that the flu NHSN archive was missing the
`_prelim` signal's earlier vintages, relative to the `_ew` "main" signal, the
way covidcast's separate named signals would be if only one were queried.
This turned out to be the wrong archive entirely: production's NHSN archive
(`scripts/data/build_nhsn_archive.R`) polls both CDC Socrata endpoints
directly every minute and already merges them by true polling timestamp, not
by an assumed `issue+3`/`issue+5` offset.
Not the mechanism here -- ruled out, no code changed for this.

### Root cause

Queried `joined_scores_2024_2026` (`flu_hosp_explore`) directly, mapping
scored forecaster codenames back to family names via `get_flu_forecaster_params()`
+ `add_id()`.
The blowup was concentrated almost entirely in `geo_value == "us"`: for
`revision_aware_beds_no_season`, 2980 of 3060 `us` rows (97%) had WIS over
10,000, spanning continuously from spring through fall 2025, not a single
bad date.
The one family without `pop_scaling` (`revision_aware`, `pop_scaling=FALSE`)
was far less affected (24% over that threshold).

`pop_scaling=TRUE` was the common factor across every exploding family.
Flu's `hhs` column is already population-normalized to per-100k at archive
build time (`flu_data_targets.R`, `mutate(hhs = value / population * 10L^5)`),
but `scaled_pop_seasonal_revision()`'s `pop_scaling=TRUE` path
(`forecaster_revision_aware.R:275-281` forward, `:384-389` reverse) divides
by population *again* before pooling all geos into one shared `quantile_reg`
fit, then multiplies back at the end.
Per-row that round-trip is a mathematical identity, so it doesn't bias any
single geo's own relationship between lags and target.
But it does put every geo's training rows on a different absolute scale
before pooling (`us`'s per-100k value divided by ~331M is a very different
order of magnitude than `wy`'s divided by ~580K), and `us` -- being by far
the largest-population geo -- ends up as an extreme outlier in that pooled,
doubly-scaled feature space.
The `quantile_reg` fit (method `"fn"`, Frisch-Newton) extrapolates badly for
it specifically, especially in the upper tail.

Confirmed live on real data (`joined_archive_data` in `flu_hosp_explore`,
forecast_date 2026-01-03, ahead=28, `us`, `revision_aware_beds_no_season`
config): with `pop_scaling=TRUE`, the 0.99 quantile converts to ~8.6M counts
against a truth of 15,743.
With `pop_scaling=FALSE`, same date/ahead/config, the 0.99 quantile converts
to ~87K -- still wide (4-week-ahead upper quantile), but not absurd -- and
the median actually got *more accurate* (~17,900 vs truth 15,743, vs ~43,000
with the bug).

Checked whether this also affects covid: it doesn't.
Covid's `hhs` column is raw counts (`covid_data_targets.R:255`,
`rename("hhs" := value)`), so `pop_scaling=TRUE` on covid's
`revision_aware_nssp`/`revision_aware_beds_*` families is correct as-is, not
a duplicate bug.
Left covid's config untouched.

Also checked flu prod (`pipelines/flu_hosp_prod.R`'s `revision_aware` id):
also unaffected, because it trains on `outcome = "value"` restricted to
`train_sources = "nhsn"` only, which in the prod archive is raw counts
(`get_nhsn_data_archive("flu")`, no per-100k conversion), not the explore
pipeline's per-100k `hhs`.
`pop_scaling=TRUE` is correct there too.
This bug was confined to flu's *explore* config.

### Fix

`R/targets/flu_forecaster_config.R`: set `pop_scaling = FALSE` everywhere in
the flu `revision_aware*` families that had `TRUE` (`revision_aware`'s
second duplicate-name block, `revision_aware_nssp`, `revision_aware_no_season`,
both sub-blocks each of `revision_aware_beds_no_season` and
`revision_aware_beds_seasonal`).

Also added a whitened sibling variant to each of those four families
(`scale_method="quantile"`, `center_method="median"`, `nonlin_method="quart_root"`,
matching the base `revision_aware` family's existing first block) alongside
the pre-existing unwhitened (`"none"`) one, via
`tidyr::expand_grid(...) %>% filter(...)` paired on the three params so the
cross product doesn't produce invalid combinations like
`scale_method="quantile"` + `center_method="none"`.
No changes to `forecaster_revision_aware.R` were needed for this -- per-geo
whitening (`calculate_whitening_params`/`data_whitening`) was already
implemented there and simply unused while every revision-aware config had
`scale_method="none"`.
The premise (per conversation with the user) is that `pop_scaling=TRUE` may
have been standing in for real whitening, so now that it's off, it's worth
sweeping whether real per-geo whitening does even better.
Grid roughly doubled for these four families (34 unwhitened + 34 whitened
rows), all ids stayed unique.

### Tests

`tests/testthat/test-revision-aware.R` gained two permanent tests:

- `"scaled_pop_seasonal_revision gives sane forecasts, whitened and
  unwhitened"`: synthetic multi-geo archive at per-100k scale (`us` plus
  nine real state abbreviations, values ~3-18, no revisions), asserts both
  the unwhitened and whitened `pop_scaling=FALSE` configs stay finite,
  non-negative, and within 10x of each geo's own recent observed value.
- `"pop_scaling=TRUE double-normalizes an already-per-100k outcome and blows
  up high-population geos"`: a characterization test on the same fixture
  with `pop_scaling=TRUE` reinstated, asserting `us`'s ratio exceeds 20x
  while every other geo's stays under 20x.
  This is a deliberate regression guard: it reproduces the bug rather than
  just checking current behavior, so reintroducing `pop_scaling=TRUE` for
  this family fails loudly instead of silently reopening the bug.
  Verified by hand (not committed as a test) that reverting the config fix
  makes the *first* test fail too, for both whitened and unwhitened.

Both tests needed the archive's synthetic values to sit at realistic
per-100k magnitude (~3-18, not e.g. hundreds) to actually reproduce the
blowup -- larger synthetic magnitudes fit fine regardless of `pop_scaling`,
which matches the mechanism: the pathology comes from pooling geos whose
values get divided down to near-zero by very different population
denominators, not from population disparity alone.

### Not done yet

- Explore hasn't been re-run with these config changes (that's a remote
  job); the fix is verified against real cached `joined_archive_data` via
  direct forecaster calls, not via a full sweep + scoring pass.
- No investigation yet into why the doubly-scaled, near-zero-magnitude
  feature space specifically destabilizes the Frisch-Newton solver (`method
  = "fn"`) -- confirmed as the trigger condition (small per-100k-scale
  values, not population disparity alone), not explained at the
  numerical-methods level.
