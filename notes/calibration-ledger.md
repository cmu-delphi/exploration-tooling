# Calibration ledger

Post-hoc online calibration of our hub forecasts with MultiQT (Ding, Gibbs &
Tibshirani 2025). Each (location, horizon) series gets an additive offset over
the 23 quantile levels, updated by online gradient descent on pinball loss
and projected to be monotone. Dropped hub rounds are in
`notes/spoiled-submissions.md`; how to run the tests and notebooks is under
"Running things".

Every number below was re-read from the rendered notebook (or, for E12/E13,
the score CSV) on 2026-10-02; E19's come from its CSVs on 2026-10-06.
Older notes, pre-fix tables and the summary-of-reviews layers were
removed; they are in VCS history at change `uxposooz`
(`notes/CALIBRATION.md`, `notes/calibration-review.md`,
`notes/calibration-summary-2026-10-01.md`).

## How to read the numbers

- **WIS change %** is against the uncalibrated base; positive is better.
  **Coverage bias** is mean |coverage − nominal| over levels; lower is
  better.
- **There are no uncertainty estimates.** Two live seasons, 27 and 28
  rounds. This file calls a gap under 1 WIS point "no clear difference".
  That is a reading convention, not a test.
- **Use month-avg coverage, not season-pooled.** Pooling over a season (or
  two) lets ramp under-prediction cancel post-peak over-prediction, and lets
  one season's error cancel the other's. Season-pooled gains of 2–5× mostly
  vanish month by month. Month-level noise is about 0.01.
- **Report each season.** Most rankings flip between 2024-25 and 2025-26.
  Pooled numbers hide that.
- **Cold starts dilute 2024-25.** Burn-in rounds warm the learning rate but
  never step the offset, so every config without a warm start equals the
  base through Nov–Dec 2024 (35–40% of that season's WIS). Warm-vs-cold in
  2024-25 is "some offset vs none".
- **Tuning is in-sample.** The constant rates (0.018, 0.032) and the
  `off_after` dates were picked on the same two seasons they are scored on.
- **Two base forecasters.** E00, E03–E05 and E09–E11 calibrate the
  submitted ensemble (h−1…h3, all locations in headline tables). E01, E02,
  E07 and E12–E19 calibrate the clean `windowed_seasonal` replay (h0–h3,
  states only, except E12).

## What the numbers support

Large and consistent (several points, both seasons):

- **Too-large steps hurt.** Adaptive multiplier 0.1 on sqrt costs 7–17 WIS
  points at h1–h3, 0.3 costs 17–81 (E01). A constant per-100k rate above
  about 0.1 loses steeply: −3 to −4 at h3 by 0.18 (E14), −12 to −23 at 1.0
  (E09).
- **sqrt beats count.** 2–5 points at h1–h3 on the clean replay (E01),
  1.4–3.9 pooled on the ensemble, mostly from 2025-26 (E03).
- **h−1 is where calibration pays.** Covid +10 to +11% WIS and about +20%
  median absolute error, both seasons, either config (E10); flu ensemble +3
  to +6 (E00, E03).
- **Stateful learning needs a delay.** A tracker that learns once from the
  first report loses all of REF-op's h−1 gain (0.0 vs +6.1) and makes h−1
  coverage worse than base; one extra week fixes it (E00, E09). Prod re-runs
  from scratch each week, so this does not apply to prod.
- **Some covid configs are harmful.** REF-paper loses 10–19 points at h1–h3
  in 2025-26 (E10); constant 0.1 with a warm start loses 8–27 (E13).

Small, or dependent on season (0–3 points):

- **Among reasonable trackers nothing clearly wins.** REF-op (warm or cold),
  sqrt constant 0.018, rate constant 0.018–0.032 and sqrt adaptive 0.01 are
  within about 1 point of each other pooled, and the order flips by season.
  Configs with a warm start lead in 2024-25; the others lead in 2025-26
  (E07, E16, E18).
- **Warm start: first live season only.** +1 to +3 points at h2–h3 in
  2024-25, −0.7 to +0.4 in 2025-26 (E05, E13, E18). Warm configs under-cover
  the 50% interval (0.39–0.44, E13).
- **Leak (`fast_decay` 0.1): season-dependent.** On the ensemble it avoids
  a 3–5 point loss at h1–h3 in 2025-26 and costs 0.4–1.3 in 2024-25 (E04,
  E05). On the clean replay it adds 0.5–1.3 at h1–h3 in both seasons and
  costs 0.4 at h0 (E18).
- **Why the leak and a small constant rate land close (E19).** Both limit
  the adaptive step's late-season overreaction, in different ways. The
  adaptive eta comes from base residuals, so the leak does not change it;
  relative to interval width it rises from about 0.015 in January to 0.10
  at h3 in May 2025, 3–4× the constant rate's. Without the leak the offset
  keeps growing into March–April, when the base over-predicts (h3 median
  offset 0.24 of width in April 2025 vs REF-op 0.11). REF-op tracks the
  ramp as fast (0.045 in February) and the leak empties it by May. Constant
  0.018 barely corrects the ramp (0.013 in February) and has little to
  overshoot with. In April–May 2026 the base under-predicted and the leak
  dropped an offset that was still right (0.01 vs 0.14 at h3), which fits
  sqrt 0.018 leading in 2025-26. The link to WIS is inferred, not scored.
- **Switching off from 1 Mar or 15 Feb.** On the clean replay: +2 to +4 at
  h1–h3 over sqrt adaptive 0.03 (E02), +0.4 to +0.7 over sqrt constant 0.018
  (E17). The date was chosen after seeing the March losses. About half of
  the 2025-26 gain at h2 is offsets frozen in February 2025 carried into
  December, not a spring fix. On the ensemble it is the weakest candidate in
  2024-25 (E07).
- **Per-level eta:** about +1 point at h1–h3 (E02).
- **March costs every tracker.** Up to −28% WIS within March at h2, about 1
  point of the season (E16). The base already over-predicts after the peak
  and the offsets are still positive.
- **The correction is a small upward shift.** Offsets are a few percent of
  the 90% interval width; the width ratio is 1.00–1.04. REF-op's gain comes
  from raising the lower half; the upper half loses beyond h0 (E15).

Coverage:

- **Month-avg, no flu config moves coverage much.** Every config is within
  about 0.025 of base, most within 0.01. REF-op and the warm configs on the
  ensemble are 0.012–0.025 better than base at h1–h3 (E05, E09). Constant
  rates are no better than base, or slightly worse at h3 (E09, E14).
- **Season-pooled coverage gains are cancellation.** For example, E11's
  "4–5× lower bias" is about 2× per season, and nothing month by month.

Finalists (E10 part 1, E18), states only, h1/h2/h3 pooled over both seasons:

- **Flu: sqrt 0.018 with a warm start leads on both bases.** Ensemble
  +2.5/+2.7/+4.3 vs REF-op warm +1.9/+1.5/+2.3; replay +1.6/+1.5/+2.7 vs
  +1.1/+0.8/+1.2. On the ensemble all of its lead is 2024-25; in 2025-26 it
  is −0.2/−0.9/−1.7, slightly below REF-op warm. Month-avg coverage: on the
  ensemble REF-op warm is the only config below the base at every horizon;
  sqrt warm is level with the base there and 0.009–0.017 above it at h2–h3
  on the replay.
- **Covid: sqrt 0.018 is worse than REF-op cold on the ensemble.**
  −2.7/−3.7/−4.2 vs −0.1/−0.3/−0.8, from 2025-26 (−6.9 to −8.3 at h1–h3;
  REF-op has the lower WIS in 44–45 of 52 states). At h−1 sqrt 0.018 gains
  the most of any config, +14.9 vs REF-op +10.2 (E10 part 2). No config
  helps covid at h1–h3 on either base.
- **The season flip is broad, but the pair is unmatched.** REF-op warm
  beats sqrt 0.018 cold in 41–45 of 52 flu states in 2024-25 and 14–18 in
  2025-26 (E18). Cold configs play the base through Nov–Dec 2024, and on the
  same replay sqrt 0.018 warm beats REF-op warm at every horizon in both
  seasons, so the per-state flip is partly the start, not the tracker. E18's
  matched pairs (warm vs warm, cold vs cold) are written, not yet rendered.

Operating point: for covid nothing tested beats REF-op cold (prod) at
h0–h3. For flu, sqrt 0.018 warm is ahead of REF-op warm (prod) by about 1
point at h1–h2 and 2 at h3 on the ensemble, all of it in 2024-25, with
month-avg coverage closer to the base than REF-op's.

## Reference configs

- **REF-paper**: count scale, adaptive eta (mult 0.03, window 20), carry,
  2023-24 burn-in, `settle_days` 14 (one extra week of revision delay),
  single offset term.
- **REF-op** (prod): REF-paper with sqrt, eta floor 1e-3, warm start
  `slow_init = "burn_in_quantile"`, slow term mult 0.003, leak
  `fast_decay = 0.1`. "REF-op cold" drops the warm start and burn-in.

Both are `cal_ref_args()` in `R/calibration/views.R`; notebooks run them via
`cal_run()`, which also picks the learning truth (`"exact"`: re-run from
scratch each round on that round's data, as prod does).

**Notebooks.** One notebook per experiment, `eNN_*.Rmd` in
`reports/writeups/calibration_experiments/`, rendered to
`reports/calibration_experiments/` with `just calibration-experiments [name]`.
The index groups them by topic (finalists, flu and covid, learning rate,
tracker structure, late season, learning truth).

Each comparison notebook leads with the month view (`_month_view.Rmd`):
WIS, coverage bias and median error by month of reference date, then
by-horizon tables of WIS reduction % and season-pooled / month-avg coverage
bias. All scores are states only. Sweep notebooks (E01, E09, E16) lead
with their coverage-vs-WIS curves and follow with a month view of a few rates
around the elbow. Per-state fan panels and the gallery of best and worst
forecasts are only in `prod_views.Rmd`, for the prod configs. Conclusions
live in this ledger, not in the notebooks.

## Experiments

All flu unless stated; all post-fix (2026-09-17 sqrt clamp and issue-round
gating), exact learning truth, spoiled submissions excluded.

| id | notebook | base | question | what the numbers show | caveats |
|---|---|---|---|---|---|
| E00 | `e00_vintage_backtest` | ensemble | learn from final, vintage or exact data? | exact ≈ final within 0.6 points except REF-op h−1 (2.2, 2024-25 only). Vintage at settle 7 loses the h−1 gain. Settle 7 vs 14: REF-paper +1 to +3 at every horizon; REF-op tie (flips by season) | |
| E01 | `e01_eta_settings` | replay, cold | adaptive eta settings | mult ≥ 0.1 costs 7–81 at h1–h3. 0.01 beats 0.03 by 0.5–2.4 at h1–h3 in both seasons; 0.03 better at h0 in 2024-25 (0.8). Window and carry vs reset: ≤ 2.4, mostly < 1. Count is 2–5 worse than sqrt | month-avg coverage of 0.01 vs 0.03 within noise |
| E02 | `e02_eta_variants` | replay, cold | `off_after`, per-level eta, seasonal window | off 1 Mar / 15 Feb +2 to +4 at h1–h3 over the 0.03 reference, both seasons, no month-avg coverage cost. Per-level +1. Seasonal window, burn-in, off 1 Apr: < 1 | dates in-sample; half the 2025-26 gain is December carry-over; "seasonal window" also changes `lr_window` 20 → 10 |
| E03 | `e03_scale` | ensemble | count vs sqrt vs rate | sqrt +1.4 to +3.9 over count pooled, mostly 2025-26. Coverage: neither better (sqrt worse per season pooled, better month-avg only in 2024-25) | rate identical to count in every cell (adaptive rate is scale invariant) |
| E04 | `e04_offset_structure` | ensemble | leak, slow term | leak +2.5 to +5.6 at h1–h3 in 2025-26, −0.4 to −1.3 in 2024-25; month-avg coverage flips the same way (±0.015). Slow term without warm start: ≤ 0.5 | |
| E05 | `e05_warm_start` | ensemble | warm start from burn-in | warm +1.7 to +3.2 at h2–h3 in 2024-25, ±0.4 in 2025-26, for every tracker. REF-op ≈ warm + leak (≤ 0.4). Month-avg coverage: all within 0.01 of each other | |
| E07 | `e07_across_eras`; the candidate ranking (formerly E07b) is in `e10_covid` | both, same rows | do results transfer between ensemble and replay? | Same direction on both; ensemble gains about 1 point more with REF-op warm. Replay base 5–7% worse pooled, 10–33% worse at h0–h1 in 2025-26. Candidate order flips by season on both; rank correlation of coverage in 2025-26 is 0.31 | |
| E09 | `e09_lr_delay` | ensemble | constant rate × revision delay | knee 0.056: +3 to +5.5 over REF-paper at h1–h3; REF-op +1.2 to +2.4 over knee. Extra delay costs about 1 h−1 point per week. Above 0.1 loses steeply | month-avg: no constant rate beats base at h1–h3 |
| E10 | `e10_covid` | flu + covid, ensemble + replay | the finalists on both diseases and bases (part 1); REF-paper, REF-op and sqrt 0.018 on the covid ensemble at h−1–h3 (part 2) | see "Finalists" above. Part 2: h−1 +10 to +15 (sqrt 0.018 largest). REF-op h1–h3: +1.9 to +2.8 in 2024-25, −3.0 to −4.3 in 2025-26. REF-paper −10 to −19 and sqrt 0.018 −7.7 to −9.5 at h1–h3 in 2025-26 | part 1 is h0–h3 on rows both bases have; part 2 tables run to 2026-09-19, including the 2026 summer wave |
| E11 | `e11_constant_lr` | ensemble | constant 0.1 vs adaptive | REF-op best pooled WIS at every horizon (up to 2 over warm + sqrt). Constant 0.1 gives up 1.5–2.3 at h−1/h0 vs adaptive sqrt | constant 0.1 is effectively cold; its coverage edge is season-pooled only |
| E12 | none (`calibration_ws_replay.R`) | replay, cold, flu + covid | first look at the clean replay | flu: REF-op cold leads constant 0.1 by 0–2.5. Covid: REF-op cold h0 +1.3 states, losses at h1–h3 in 2025-26 | **all locations** (US is 47% of WIS); covid h0 gains are mostly US |
| E13 | none (`calibration_ws_replay.R burn_in`) | replay, flu + covid | 2023-24 HHS burn-in | flu warm +1 to +2.6 in 2024-25, ≤ 0 in 2025-26. Covid: warm worse at h0–h1, better at h2–h3 (+1.3, +4.4); constant 0.1 warm −8 to −27 | HHS used as-is (3–9% below NHSN); coverage is season-pooled only |
| E14 | folded into `e16_sqrt_constant_lr` | replay, cold | constant rate grid by season | every rate 0.0032–0.056 is ≥ 0 in both seasons and within 1.2 of each other; 0.018 vs 0.032 ≤ 0.3. Above 0.1 loses | month-avg: no rate ≤ 0.032 beats base beyond 0.01 |
| E15 | `e15_wis_sources` | replay, cold | where the WIS gain comes from | offsets a few % of interval width; REF-op gain from the lower half and the median shift | width-only variant's coverage not measured; split is not additive |
| E16 | `e16_sqrt_constant_lr` | replay, cold | constant rate on sqrt scale | sqrt 0.018 about +1 over rate constants at h0; at h1–h3 within 0.2 pooled, up to +0.7 in 2025-26. vs REF-op cold: behind in 2024-25 h0–h1, ahead elsewhere; month-avg coverage tie | 0.018 picked in-sample |
| E17 | `e17_late_decay` | replay, cold | shrink or stop offsets from 1 Mar | `late_decay` fixes March but costs about 0.5 at h0, otherwise ±0.2. Off 1 Mar +0.4 to +0.7 at h1–h3 for sqrt 0.018 (mostly 2025-26), ≤ 0 for rate 0.018 | `late_decay` shrinks the stored offset, so it carries into the next season |
| E18 | `e18_ref_op_bridge` | replay | which piece of REF-op matters | REF-op vs sqrt 0.018 within 0.6 pooled; REF-op ahead in 2024-25 by 1.3–1.5, behind in 2025-26 by 0.8–1.3. Warm start +0.9 to +2.0 in 2024-25, −0.2 to −0.7 in 2025-26. No step moves month-avg coverage beyond 0.008. Branch sqrt 0.018 warm: +3.4/+1.6/+1.6/+2.5 pooled, best of all, but month-avg coverage 0.011–0.018 worse than base at h2–h3 | steps 0 and 4 are the same cached runs as E13; steps 4 and 5 identical (scale invariance) |
| E19 | `e19_tracker_internals` (was a script; the numbers here are the script's, on final truth, until the notebook is rendered) | replay | why do the leak and constant 0.018 score alike? Step size, offset and base bias by month on the E20 grid; carry vs reset on both anchors | REF-op cold and no-leak eta identical; adaptive eta / width peaks Apr–May (h3 0.10) vs constant ≤ 0.03. h3 median offset / width, Feb/Mar/Apr/May 2025: REF-op 0.045/0.078/0.114/0.009, no leak 0.048/0.109/0.237/0.199, constant 0.013/0.026/0.057/0.074; Apr/May 2026: 0.093/0.013, 0.165/0.149, 0.101/0.143. Share of truths above the base median at h3: Mar 0.27–0.43, Apr–May 2026 0.62–0.71 | script numbers: final truth, h1–h3, median-level offset only, no WIS by month. The notebook uses exact truth and adds WIS by month and the resets; not yet rendered |
| E20 | `e20_structure_grid` | replay | warm start and leak on both anchors: {adaptive 0.03, constant 0.018} × {cold, warm} × {no leak, leak 0.1}, plus REF-op warm and cold | not yet run | replaces E03–E05 and E11's questions on the replay; those stay until read against it. The slow term is left out (E18 step 2) |
| E21 | `e21_data_by_season` | replay data | how the two live seasons differ as data: shape, revisions, learning-truth gap at lags 1–4, HHS-stitched burn-in vs NHSN | 2024-25: 27 rounds from 2024-11-23, peak 2025-02-08 at 55.6k (states summed), total 559k; 2025-26: 28 rounds from 2025-10-18, peak 2026-01-03 at 42.5k, total 336k. State peaks within ±1 week of the national one: 37 vs 47 of 52. Revisions: 26% vs 40% of (state, week) keys never revised; relative spread > 10% for 48% vs 39%. Value at lag 2 (what the tracker learns from) vs final: median gap −1.1% vs 0.0%, \|gap\| > 10% for 19% vs 14% of keys; at lag 1, 44% vs 41%. HHS vs NHSN final over 2023-24: 0.957–0.978 in Nov–Apr (states summed), per-state median 1.004 | describes the data only; no tracker runs. Revision keys start at the first archive version (2024-11-19) |

E06 (ILI+ burn-in) and E08 (old covid) were run on code or data later found
broken and were not rebuilt.

## Known issues

- **Run cache ignores code.** `cal_run_cached()` keys on inputs and
  arguments, not code; the E00 and E09 caches are keyed by chunk code or
  name. Clear `cache/calibration/experiments/` after changing
  `R/calibration/`.

Checked on 2026-10-02: the whole cache was cleared and every experiment
re-run from scratch (E12, E13 and every notebook). Every table matched
the cached results cell for cell, so no result above came from a stale
run.

Checked and resolved on 2026-10-02:

- **`scales` is applied.** `rate` equals `count` to every digit with the
  adaptive rate (E03, E09, E18 steps 4–5) because the adaptive rate is
  scale invariant: all 53 locations match a population scale, adaptive
  runs agree to 1e-11 with and without it, and with a constant rate the
  calibrated quantiles differ by up to 300 admissions.
- **E10 pools through the 2026 summer on purpose**, to include covid's
  summer wave; its headline and month tables run to 2026-09-19.
- **Month-view base column** in E07 (one base shown for configs on two
  forecasters) and the **`late_decay` docstring** (now says it shrinks the
  stored offset) were fixed.

## Review status

| notebook | status |
|---|---|
| E00, E01 | verified |
| E02, E07, E09 | commented; changes made, not re-verified |
| E10 part 1, E18 | changes made (finalists runs, state counts), not reviewed; E18's matched pairs not yet rendered |
| E19, E20 | written 2026-10-06, not run |
| E21 | rendered 2026-10-06, not reviewed |
| E03, E04, E05, E10, E11, E14, E15, E16, E17 | not reviewed |

On 2026-10-02 every notebook's introduction and Findings were rewritten to
state setup and measured sizes only; the rewritten text has not been
reviewed.

## Code, data and prod

- `R/calibration/qt.R`: the tracker (`qt_track`, `qt_learning_rate`,
  `qt_project`). A port of the authors' Python, tested against it (see
  "Running things").
- `R/calibration/calibrate.R`: `calibrate_hub_forecasts()` and
  `calibrate_hub_forecasts_exact()`, plus metrics. Options beyond the
  paper: `transform`, `scales`, `lr_slow` / `fast_decay` /
  `burn_in_learns_slow` / `slow_init` (two-term offset and warm start),
  `off_after`, `late_decay`, `lr_seasonal`.
- `R/calibration/hub_data.R`: hub readers (`hub_read_forecasts()` drops
  spoiled submissions), `nhsn_read_truth()`.
- `R/calibration/views.R`: reference configs, cached runs, standard views.
- `scripts/calibration/calibration_ws_replay.R`: E12/E13. Scores in
  `cache/calibration/ws_replay_scores_*.csv`.
- `scripts/calibration/calibration_ws_replay.R` also holds the notebook
  helpers: `ws_grid_configs()` (the E19/E20 structure grid),
  `ws_prod_specs()`, `ws_run_spec()`, `ws_wis_by_season()` and
  `ws_internals()` (per-round eta and offsets from an exact run).
- `scripts/calibration/calibration_ili_backfill.R`: replays
  `windowed_seasonal` over the ILI+ state history (2010–2024) into
  hub-schema parquets in `cache/calibration/`, for an ILI+ burn-in (thread closed 2026-10-02, unused).
- The pre-experiment notebooks (`reports/writeups/calibration/`) and the old
  E07 scripts were deleted on 2026-10-02; they are in VCS history. Their
  only analyses not in the current suite are the staleness lagged
  correlation, the ILI+ base-bias table, per-round tracker internals (eta,
  offsets before and after projection) and a per-level reliability plot.
- **Data.** Flu hub submissions 2023-10-14 … 2026-05-30, 53 locations,
  h−1…h3. 2023-24 is burn-in. NHSN vintages from
  `get_nhsn_data_archive()` start 2024-11-19. With `settle_days` 14 the h3
  offset is always 5 rounds stale.
- **Prod (2026-09-21).** Flu and covid prod write a secondary submission,
  `CMU-TimeSeries-Calibrated`, via the targets `calibrated_ensemble_nhsn`,
  `make_calibrated_submission_csv` and `local_calibrated_scores_nhsn`. Flu
  runs REF-op with the 2023-24 burn-in; covid runs REF-op without burn-in
  or warm start. The covid submission is mostly
  `windowed_seasonal_extra_sources` at h1–h3 and `revision_aware` at h−1
  (weights in `pipelines/covid_geo_exclusions.csv`, edited weekly).

## Running things

Commands run from the repo root (inside the rocker container,
`distrobox enter rocker -- <command>`, if R isn't on the host). The hub
checkouts are siblings of this repo: `../FluSight-forecast-hub` (sparse:
CMU model-output only) and `../covid19-forecast-hub`.

**Port tests.** `R/calibration/qt.R` ports the authors' Python
(`projectedQT`). The oracle is `../multiQT` on branch `delphi-fixes`, which
fixes several defects in the published code (see that branch's commit
message); the R port implements only the fixed behavior.

```sh
# tracker tests: bit-exact oracle fixtures plus properties
Rscript -e 'testthat::test_file("tests/testthat/test-qt.R")'
# whole suite
Rscript -e 'testthat::test_dir("tests/testthat")'

# regenerate the fixtures (made with lr_window = 50, which test-qt.R spells out)
(cd ../multiQT && uv run --with numpy --with scikit-learn --with matplotlib python make_r_fixtures.py)
cp ../multiQT/r_fixtures/*.csv tests/testthat/fixtures/qt/

# cross-check on real hub series; expect ALL MATCH within 1e-8
Rscript scripts/calibration/calibration_export_series.R
(cd ../multiQT && uv run --with numpy --with scikit-learn --with matplotlib python check_r_port_real_series.py)
```

One porting trap: numpy broadcasts `Y - Yhat` along the last axis, while R
recycles a vector down columns. Getting it wrong silently corrupts every
learning rate; `test-qt.R` pins `eta` against a hand-built residual matrix.

**Notebooks and scripts.**

```sh
just calibration-experiments              # all notebooks and the index
just calibration-experiments finalists    # one notebook and the index
Rscript scripts/calibration/calibration_ws_replay.R 12           # E12 (12 workers)
Rscript scripts/calibration/calibration_ws_replay.R 12 burn_in   # E13
```

Tracker runs are cached under `cache/calibration/` by `cal_run_cached()`,
keyed on inputs and arguments but not code: clear the cache after changing
`R/calibration/`. E00 and E09 keep their own caches, cleared by hand.

## Open threads

Checked against the code on 2026-10-02. Repo-wide items are in
`notes/ROADMAP.md`. Add a thread only if its answer could change the
operating point.

| item | status |
|---|---|
| **Reorganization (2026-10-06).** The constant-rate line was never given the structure analysis (E03–E05, E11 ran the adaptive rate on the ensemble; E16 inherited "cold" from E11's framing and E13's constant 0.1 warm covid loss). Plan: (1) E18 matched-pair scatters, warm vs warm and cold vs cold; (2) E20, the {anchor} × {start} × {leak} grid on the replay; (3) E19 as a notebook on that grid with WIS by month and carry vs reset; (4) E21, the seasons as data. Then re-read E03–E05 and E11 against E20 and retire or keep them | written, not run: E18's new pair uses cached runs; E19 and E20 need about 7 new exact runs (the four leak variants, adaptive warm without leak, the two resets; the rest are cached from E01, E13, E16 and E18). E21 rendered 2026-10-06. Render with `just calibration-experiments e18_ref_op_bridge e19_tracker_internals e20_structure_grid` inside the rocker container |
| Uncertainty for WIS and coverage differences (bootstrap over rounds or locations) | open; needed before any ranking under ~2 points means anything |
| Shrink only the played offset after the peak, keep the stored state | open; `late_decay` shrinks the stored state |
| Disease-specific operating point | provisional (2026-10-02): covid REF-op cold; flu REF-op warm or sqrt 0.018 warm, to confirm after the notebook review. Not implemented |
| HHS burn-in level | E21: 2–4% below the NHSN final in Nov–Apr 2023-24 (states summed), per-state median ratio 1.004. Used as-is (E13, E18); settle it with item 1f if a warm flu config is chosen |
| `windowed_seasonal_extra_sources` retrospective | parked (ROADMAP) |
| Method ideas: cross-horizon gradients for staleness, data-driven phase gating, scale proxy | open; no code |
| Collaborator email | unknown |

Closed 2026-10-02 without further work: ramp vs post-peak split (the month
view shows it), h−1 calibration (left to the revision-aware methods,
ROADMAP 3), ILI+ burn-in (the HHS burn-in covers it), gallery review (review
uses the month view), speeding up exact runs.
