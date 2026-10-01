# Roadmap

Repo-wide tasks, TODOs and tech debt, roughly in priority order within each
section. Calibration-specific threads live under "Open threads" in
`notes/CALIBRATION.md`; refactor designs (E2 ensemble sweep, snapshot
validator, simplification inventory) live in `notes/refactor-ideas.md`.

## Forecast evaluation

1. **Choose a testbed for comparing calibration with forecaster choice.**
   Calibrating needs full quantile forecasts over many rounds, not just
   scores. Candidates:
   - the calibration harness on hub submissions (what we actually
     submitted, already in use);
   - the evaluation project (`flu_hosp_evaluation` / `covid_hosp_evaluation`),
     which replays the current prod components and ensemble over past dates.
     It also replays the hand-edited weights and geo exclusions of each past
     week, which biases it toward reproducing what was done;
   - the explore stores, which already hold per-forecaster quantile forecasts
     for every family on S3 (NHSN target only);
   - a new "explore-lite" project with a handful of families.
   Undecided (2026-10-01).
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

## Revision-aware forecasting and h−1

3. **Revision-method backtests.** `revision_aware` against the
   `revision_ratio` baseline at h−1 (explore h−1 families plus
   `nowcast_notebook`, both diseases), then choose prod's h−1 component.
   Calibrating h−1 waits on this (`notes/CALIBRATION.md`).
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

## Tech debt

7. renv warns on every run (library renv 1.1.6, lockfile 1.2.3; some
   lockfile packages not installed). Harmless so far.
8. Refactor and cleanup threads: `notes/refactor-ideas.md`.
