# Roadmap

Repo-wide tasks, TODOs and tech debt, roughly in priority order within each
section. Calibration-specific threads live under "Open threads" in
`notes/CALIBRATION.md`; refactor designs (E2 ensemble sweep, snapshot
validator, simplification inventory) live in `notes/refactor-ideas.md`.

## Forecast evaluation

1. **Use the evaluation project as the calibration testbed** (current plan,
   2026-10-01). Calibrating needs full quantile forecasts over many rounds,
   not just scores. The evaluation project (`flu_hosp_evaluation` /
   `covid_hosp_evaluation`) replays the current prod components and
   ensemble weekly since 2024-11-20, which is what calibration would see in
   prod from now on. The hub-submission harness stays the record of what was
   actually submitted; the explore stores (per-forecaster quantile forecasts,
   NHSN only, on S3) can answer "does calibrating a candidate beat picking a
   different one" without a new project. Three gaps, below. Work them in
   small steps (AGENTS.md, "Working incrementally"); the S3 evaluation
   stores are empty, so even the baseline needs a run.

   **Critical path for evaluating calibration:**

   a. *Clean replay, one component.* Start with `windowed_seasonal` alone
      (the harness proxy). A single component has no ensemble weights, so
      the only hand edit to switch off is the data substitutions
      (`make_forecast_snapshot(substitutions = NULL)`). Steps: a way to run
      one forecaster (e.g. an env var that filters the grid), 2–3 dates,
      diff against the edited run (changes should appear only at the
      substituted geo-dates), then all dates, timed. Feed it to the
      calibration harness.
   b. *2023-24 as a burn-in season.* The best configs so far (E05
      `warm + single`, REF-op) warm-start from a burn-in season, and the
      replay has none. Delphi's `hhs` source
      (`confirmed_admissions_{influenza,covid}_1d`) has real issue history
      for 2023-24 (CA 2023-12-01: issues 12-06, 12-08, 12-20, 12-22), until
      HHS reporting ended 2024-04-30. Steps, each checked before the next:
      - HHS weekly archive (done 2026-10-01,
        `scripts/one_offs/hhs_2023_24_archive.R`). Decisions: NHSN's week
        ending Saturday S is the sum of hhs daily `time_value` S−7..S−1
        (exact match in 71% of flu geo-weeks; other alignments 17–24%),
        labelled `time_value = S − 3` (Wednesday). Every daily issue is a
        version; a week's value as of v sums its 7 days at their latest
        issue ≤ v, and a week appears only once all 7 days exist (first
        report lag is a median of 4 days, as with NHSN). US is hhs
        `nation`, which equals the sum of states plus territories, as NHSN
        `us` does. The `hhs` source matches the cached healthdata.gov
        snapshots exactly, so the gap below is between sources, not a
        fetch problem. **Open: level.** Finalized HHS runs below finalized
        NHSN (in-season sum ratio flu 0.968, covid 0.941, falling to ~0.91
        by April; tx/in/ks/ar/pr 15–20% low). The gap is already there in
        NHSN's first 2023-24 version, so it isn't an NHSN revision. Choose:
        use as-is with a caveat, rescale per geo by the finalized ratio
        (uses finalized data), or drop the worst geos. Also decide how to
        stitch: NHSN's archive holds 2023-24 from version 2024-11-19, so
        dates after that see NHSN history, a level jump from HHS;
      - ILI+/flusurv training extras (checked 2026-10-01). They are folded
        into `nhsn_prod_archive` (`pipelines/flu_hosp_prod.R`) with
        `version = time_value`, and ILI+ runs to 2024-07-24. A 2023-12-06
        snapshot would train on finalized ILI+ up to the forecast week
        itself. The snapshot's version-faithfulness abort can't see this
        (no row is newer than its version); only the archive-build
        `stopifnot` catches it. Decision: version each extras row at its
        season's end (`group_by(source, season)`,
        `version = max(time_value)`) and replace that `stopifnot` with a
        check on versions. Dates from 2024-11-21 on see every extras row
        either way, so the golden diff should be empty. Covid has no
        extras;
      - NSSP vintages (checked 2026-10-01): none before 2024-04-18 from any
        source (v5 API, epidatr `nssp`, the S3 Socrata snapshots, the hub
        mirror). Decision: `windowed_seasonal_extra_sources` sits out
        2023-24. Gotcha: an NSSP snapshot before the first vintage
        silently returns 0 rows, so a 2023-24 replay needs an explicit skip
        or abort for every component that reads NSSP. NHSN versions start
        2024-11-19, so the HHS archive must supply every 2023-24 target
        row;
      - `windowed_seasonal` on 3 dates in 2023-24, fan plots against the
        2023-24 hub submissions; then all 2023-24 dates.
   c. *The ensemble with fixed weights.* The geo-exclusions CSVs carry the
      ensemble weights as well as exclusions, so "no hand edits" for
      `ensemble_mix` means one fixed weight block for every date. Decide
      which block (the default one at the top of the file, or the current
      one), check the resolved weights on one date, then replay. Only
      needed once a single component's calibration results look worth
      extending to what we submit.

   **Later (not needed to evaluate calibration):**

   d. *Hand edits on vs off as its own question.* A separate evaluation
      project (like the `_regr` ones) replaying with the edits, to measure
      whether the weekly substitutions and weight changes help. The only
      past check found is the 2025 revision report
      (`revision_summary_report_2025.Rmd`), which scored substitutions by
      whether they moved values toward the final value. Calibration needs
      only the clean replay.
   e. *ILINet as a target*, to check our methods on a dataset we have not
      tuned on. `fluview` has real issue history (CA wILI for week 2023-50
      is revised weekly through May 2024) back to about 2010. Follows the
      `nssp_target_archive` + `primary_source` pattern; the ILI+ training
      rows must be dropped in this mode, and wILI is a percentage, so it
      shares the scale question with NSSP-target scoring (item 2). Prototype
      on one forecaster and a few seasons; `calibration_ili_backfill.R`
      (ILI+, no vintages) is a reference, not the base to extend.
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
