# Roadmap

Repo-wide tasks, TODOs and tech debt, roughly in priority order within each
section. Calibration-specific threads live under "Open threads" in
`notes/calibration-ledger.md`; refactor designs (E2 ensemble sweep, snapshot
validator, simplification inventory) live in `notes/refactor-ideas.md`.

## Forecast evaluation

1. **Calibration in the evaluation pipeline.** The operating point is a
   provisional disease-specific choice (covid REF-op cold, flu sqrt 0.018
   warm + leak), not yet implemented. The plan, the pipeline's current
   state and the open decisions are in `notes/calibration-ledger.md`,
   "TODO: calibration in the evaluation pipeline". The clean replay
   (`EVALUATION_FORECASTERS`, `EVALUATION_SUBSTITUTIONS=false`, AGENTS.md)
   and the harness-side 2023-24 burn-in are done; see the ledger's "Code,
   data and prod". The fixed-weight ensemble and hand-edits-on-vs-off
   questions (old 1c, 1d) are in Parked; ILINet as a held-out target (old
   1e) is in the ledger's "Ideas".

2. **NSSP-target backtesting is second-class and needs dedicated
   attention.**
   - Explore forecasts NHSN only: every family sets `outcome = "hhs"`. NSSP
     as a target exists only for the prod components, in the prod and
     evaluation pipelines and the `*_nssp_backtesting_*` reports, so no
     NSSP-target sweep has ever been run.
   - Flu NSSP scoring looks broken. In `flu_nssp_backtesting_2024_2025_on_2026-05-06`
     and `2026-07-01_flu_nssp_scoring_*`, every model, the hub ensemble
     included, has 90% coverage of 0 and WIS about equal to absolute error.
     Most likely the truth and the forecasts are on different scales
     (proportion vs percent). Fix this before reading any flu NSSP result.
   - Covid NSSP scoring looks sane (May 2026 backtest, 2024-25): our best
     component is `windowed_seasonal_latest` (mean WIS 0.11),
     `CMU-TimeSeries` 0.14, the CovidHub ensemble 0.06.

2b. **US is handled several different ways; make it one declared policy**
   (audited 2026-10-01).
   - `score_forecasts()` drops location `US` on purpose
     (`R/targets/score_targets.R:84`), so every local, external and
     calibrated NHSN score omits US. Behind that, its location → geo join
     goes through `get_population_data()`, which has both `us` and an
     `usa` alias for code `US`, so dropping only the filter double-counts
     US. The calibrated-forecast joins in `prod_shared.R` and both prod
     pipelines have the same latent duplicate. A patch (one-row-per-location
     `hub_location_crosswalk()`, used in all those joins) is in
     `_local/patches/us_scoring.patch`, checked on 2 dates per disease. It
     roughly doubles cross-geo mean WIS (US WIS ≈ sum of states), so ship it
     with a `geo_level` column and state-only means in the reports.
   - Submissions are not affected: US is present in all covid files and
     all flu files since 2024 (US ÷ sum of states median ratio 1.00 flu,
     1.03 covid). But flu 2025-11-22 to 12-06 had US at 0.58–0.71 of the
     state sum at h2–h3, and nothing checks that.
   - `climate_geo_agged` pools US into the state climatology unscaled, so
     CMU-climate_baseline's US is about state-sized relative to the pool
     (submitted US 0.5–0.9 of the state sum).
   - Flu explore: 4 `scaled_pop_data_augmented` forecasters
     (`filter_agg_level = ""`) forecast and score US; the other 129 drop it.
     The comparison notebook ranks them by mean WIS, so these 4 look worst.
     Covid explore drops US everywhere.
   - Proposed design: one canonical `"us"` from archive build on, with one
     crosswalk for every geo ↔ location join (then delete the `usa` alias
     and the redundant downstream renames). A spec column `us_method`
     (`direct` default, `sum_of_states`, `none`) in
     `FORECASTER_SPEC_DEFAULTS` and the ensemble specs, applied once in
     `run_forecaster()`, replacing the data-dependent `filter_agg_level`
     drop. Contracts: `validate_forecast_output()` requires `us` unless
     `us_method = "none"`; prod health warns when the US median is outside
     about 0.8–1.25 × the sum of state medians; `score_forecasts()` never
     drops a location silently and has unique keys.

## Revision-aware forecasting and h−1

3. **Revision-method backtests.** `revision_aware` against the
   `revision_ratio` baseline at h−1 (explore h−1 families plus
   `nowcast_notebook`, both diseases), then choose prod's h−1 component.
   Calibrating h−1 waits on this (`notes/calibration-ledger.md`).
4. **Flu explore has no current scores for the revision families.** In the
   flu explore store on S3 (`joined_scores_2024_2026`, 2026-09-19), none of
   `revision_aware`, `revision_aware_augmented`, `revision_aware_nssp`,
   `revision_aware_no_season`, `revision_aware_beds_no_season`,
   `revision_aware_beds_seasonal` or `revision_ratio` has scores under its
   current id. The 32 orphaned ids in that store score 20–60× flatline in
   2024-25, consistent with the pre-fix US population-scaling bug
   (`notes/revision_aware_forecaster_devlog.md`). The two `beds` grids are
   24 forecasters each and have never been scored for flu.
5. **Re-run covid explore families that take NSSP as an input.** Covid
   explore NSSP now has real vintages (`up_to_date_nssp_state_archive`); the
   covid store on S3 (2026-09-28) predates that. `scaled_pop_exogenous`
   scores 0.55 relative WIS against flatline there, better than the
   CovidHub ensemble (0.62), which suggests leakage from finalized NSSP.
   `revision_aware_nssp` needs the same re-run.

## Explore grid

6. **Trim `climate_linear`.** It is 73 of 182 flu forecasters and 49 of 97
   covid forecasters. In the S3 stores (WIS relative to flatline, h0–h4,
   mean of 2024-25 and 2025-26):
   - `drop_non_seasons` gives identical scores either way, for both
     diseases. Either it is a no-op on these dates or it is not wired
     through; check before dropping it.
   - `nonlin_method` moves the best score by less than 0.001.
   - `model_used` is the axis that matters, and the winner differs by
     disease: `climate` for flu (best 0.865; `climate_linear` 0.963,
     `climatological_forecaster` 1.04), `climate_linear` for covid (best
     0.854; `climate` 1.99, `climatological_forecaster` 5.3).
   - `quantiles_by_geo = TRUE` helps flu, hurts covid.
   Fixing `drop_non_seasons` and `nonlin_method` alone cuts the family to
   about a quarter (flu ~19, covid ~13).

## Parked (2026-10-01, not on the calibration path)

- Covid `windowed_seasonal_extra_sources` calibration (calibration-ledger open
  threads). It is ~86% of covid `ensemble_mix` but has no 2023-24 NSSP, so
  no burn-in.
- CMU-climate_baseline US forecast is too low (item 2b).
- `us_method` spec column and removing the `usa` alias (item 2b).
- Ensemble-base calibration headline tables (E00, E03, E09, E10 part 2) pool
  raw counts over all locations, so US is about half of their "WIS change
  %". The replay experiments report states only (except E12).
- *The ensemble with fixed weights* (was item 1c, parked 2026-10-07). The
  geo-exclusions CSVs carry the ensemble weights as well as exclusions, so "no hand edits" for
  `ensemble_mix` means one fixed weight block for every date. Decide
  which block (the default one at the top of the file, or the current
  one), check the resolved weights on one date, then replay. Not needed
  to choose the operating point: E10 already scores the finalists on
  the submitted ensemble, and E07 shows calibration acts the same way
  on the ensemble and the single-component replay. Revisit if the calibration pipeline plan (ledger TODO) needs
  a clean ensemble history.
- *Hand edits on vs off as its own question* (was item 1d, parked
  2026-10-07). A separate evaluation project (like the `_regr` ones) replaying with the edits, to measure
  whether the weekly substitutions and weight changes help. The only
  past check found is the 2025 revision report
  (`revision_summary_report_2025.Rmd`), which scored substitutions by
  whether they moved values toward the final value. Calibration needs
  only the clean replay.

## Tech debt

7. renv warns on every run (library renv 1.1.6, lockfile 1.2.3; some
   lockfile packages not installed). Harmless so far.
8. Low priority: `windowed_seasonal`'s h−1 forecast ignores the latest
   reported week, so a data substitution to that week never reaches h−1.
   Most substitutions are to that week (flu 20 of 25 rows, covid 10 of
   10). Submitted h−1 comes from other components, so nothing shipped is
   affected; revisit with the revision-aware h−1 work (item 3).
9. Refactor and cleanup threads: `notes/refactor-ideas.md`.
