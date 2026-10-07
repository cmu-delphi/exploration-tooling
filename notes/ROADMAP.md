# Roadmap

Repo-wide tasks, TODOs and tech debt, roughly in priority order within each
section. Calibration-specific threads live under "Open threads" in
`notes/calibration-ledger.md`; refactor designs (E2 ensemble sweep, snapshot
validator, simplification inventory) live in `notes/refactor-ideas.md`.

## Forecast evaluation

1. **Use the evaluation project as the calibration testbed.** The
   operating-point work (hub harness, `notes/calibration-ledger.md`,
   "Finalists") is down to a disease-specific choice. Provisional
   (2026-10-07, not implemented): covid REF-op cold (no tracker tested helps
   at h1–h3, E20), flu sqrt 0.018 warm + leak (about 1/1.5/2.5 points over
   REF-op warm at h1–h3 on the ensemble, coverage within 0.002, the 90%
   round-bootstrap interval on that gap excluding zero at h2–h3; E10, E20).
   Done so far: a1–a4, b1–b4 (E12, E13), the structure grid on both
   anchors and diseases (E18–E21). Next: item 1f, in its own thread. Calibrating needs full quantile forecasts over many rounds,
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
      - a1–a2 done 2026-10-01: `EVALUATION_FORECASTERS`, `EVALUATION_DATES`,
        `EVALUATION_SUBSTITUTIONS=false` (AGENTS.md "Key env vars"); prod
        manifests unchanged. Filtering out any ensemble component skips all
        ensemble, submission, report and calibration targets. On vs off,
        the snapshot differs exactly at the CSV cells (flu 2025-01-08,
        2025-02-12, 2026-01-28; covid 2025-02-19; control date
        bit-identical). The forecasts move a lot at the substituted geos
        and slightly (median ~0.1%, max ~6%) at every other geo on the
        same date, because `windowed_seasonal` fits one model pooled
        across geos. So the expected diff is "only on substituted dates",
        not "only at substituted geos". Single-forecaster cost: ~40
        CPU-s per date plus ~4 min fixed overhead per run.
      - a3 done 2026-10-01: `EVALUATION_FORECASTERS=windowed_seasonal
        EVALUATION_SUBSTITUTIONS=false make eval-{flu,covid}` into the
        local stores (not pushed). Flu 11.5 min, covid 9 min, 0 errors, 98
        dates (2024-11-20 to 2026-09-30), all 53 geos, no NAs. Forecasts
        are in `forecast_nhsn_full`. Gaps: `local_scores_nhsn` never
        scores `us` (52 of 53 geos, likely a geo-name mismatch with
        `nhsn_latest_data`); NSSP inputs drop `mo` on 81 dates, `wy` on 28
        and `nh` on 5, for both diseases.
      - a4 done 2026-10-01: `ch_use("{flu,covid}_windowed_seasonal")` in
        `scripts/calibration/calibration_harness.R` reads forecasts from
        the clean store (decision: read the store, backfill only for spec
        changes; the two paths match exactly on 5 dates per disease).
        Results are E12 in `notes/calibration-ledger.md`. The two diseases'
        replays differ on 2024-11-20: flu generates on 11-21, covid on
        11-20 (NHSN's bad release; the hub-round filter drops that round).
        Decision (2026-10-01): calibrate h0 and up only. h−1 belongs to
        the revision-aware methods (item 3) and gets integrated with them
        later.
   b. *2023-24 as a burn-in season.* REF-op warm-starts from a burn-in
      season, and the replay has none. The warm start helps only in the
      first live season (E05, E18, E20), but the flu finalist uses it, so the
      stitch stays on the critical path for 1f. Its level is settled (E21:
      HHS 2–4% below the NHSN final over 2023-24). Delphi's `hhs` source
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
      - Done 2026-10-01 in the harness, not the pipeline: HHS back to
        2020-08 (issues before 2023-07 collapse to one version), stitched
        before NHSN's first version; a 2024-12-04 control matches the
        store exactly. Results: E13 in `notes/calibration-ledger.md`.
   f. *Calibration in the evaluation pipeline.* The calibration targets in
      `pipelines/{flu,covid}_hosp_prod.R` run in evaluation mode too, but
      there they calibrate the submitted `CMU-TimeSeries` history from the
      hub checkout plus only the current replayed round; learn from
      `hhs_evaluation_data` (the latest version, so final truth, not exact);
      hard-code REF-op; and are skipped when `EVALUATION_FORECASTERS` drops
      an ensemble component. To evaluate calibration there: calibrate the
      replayed forecasts over all dates, learn from per-round snapshots
      (`calibrate_hub_forecasts_exact()`), take the config as a parameter so
      prod's config and the finalist (flu: sqrt 0.018 warm + leak; covid:
      REF-op cold is prod) both run, and score states only. A burn-in for REF-op needs
      the HHS stitching (b), which today exists only in the harness.

   **Later (not needed to evaluate calibration; c and d moved to Parked):**

   e. *ILINet as a target*, to check our methods on a dataset we have not
      tuned on. `fluview` has real issue history (CA wILI for week 2023-50
      is revised weekly through May 2024) back to about 2010. Follows the
      `nssp_target_archive` + `primary_source` pattern; the ILI+ training
      rows must be dropped in this mode, and wILI is a percentage, so it
      shares the scale question with NSSP-target scoring (item 2). Prototype
      on one forecaster and a few seasons; `calibration_ili_backfill.R`
      (ILI+, no vintages) is a reference, not the base to extend.
      Source (checked 2026-10-02, `notes/ilinet-sources.md`): v5
      `fluview_ilinet` matches epidatr `fluview` at every issue outside NY
      (99.99% of state rows), with version date = CDC release Friday. State
      version history starts 2017w40 in both, so a vintage backtest has
      2017-18 on; use state `ili` (v5 has no state `wili`).
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
  on the ensemble and the single-component replay. Revisit if 1f needs
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
