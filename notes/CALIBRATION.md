# Online quantile calibration (MultiQT)

Post-hoc online calibration of our submitted forecasts, following MultiQT
(Ding, Gibbs & Tibshirani 2025): per (location, horizon) series, an additive
offset vector over the 23 hub quantile levels is updated by online gradient
descent on pinball loss, with an adaptive learning rate and isotonic
projection. Full repro commands live in `notes/calibration-runbook.md`.

# State

## Code

- `R/calibration/qt.R` — the tracker (`qt_track`, `qt_learning_rate`,
  `qt_project`, `qt_delay_from_dates`). Bit-exact against the authors' Python
  on the `delphi-fixes` branch of `~/repos/delphi/multiQT` (their published
  code had several bugs we fixed and reported; see that branch's commit
  message). Oracle fixtures in `tests/testthat/test-qt.R`, plus a real-series
  cross-check (`scripts/calibration/calibration_export_series.R`).
- `R/calibration/hub_data.R` — FluSight hub adapters: `hub_read_forecasts()`
  (CMU-TimeSeries submissions from the sparse hub checkout),
  `nhsn_read_truth()` (latest NHSN vintage from `get_nhsn_data_archive()`, on
  the hub's Saturday grid; replaced the hub target-data CSV on 2026-09-23),
  `hub_label_seasons()`.
- `R/calibration/calibrate.R` — `calibrate_hub_forecasts()` driver (burn-in,
  season policy, ~5 s for all 265 series) and metrics: `hub_coverage()`,
  `hub_coverage_summary()`, `hub_quantile_loss()`, `hub_rolling_tradeoff()`.
  Method options beyond the paper: `transform` (power/log space),
  `scales` (per-location, per-era divisors so rounds from different sources
  share one tracker), `lr_slow` + `fast_decay` + `burn_in_learns_slow` (the
  two-term offset, below), `off_after`, `lr_seasonal`. (`lr_geo_pool`, the
  geo-pooled eta in the sweep table below, was removed 2026-09-17: it never
  beat the baseline.)
- `scripts/calibration/calibration_ws_backfill.R` — replays flu prod's `windowed_seasonal`
  over the NHSN seasons (hub schema, cached to
  `cache/calibration/ws_pseudo_hub_forecasts.parquet`); `scripts/calibration/calibration_ws_experiments.R`
  runs the one-forecaster-across-eras comparison (below).
- `scripts/calibration/calibration_ili_backfill.R` — runs flu prod's `windowed_seasonal`
  forecaster on the ILI+ state history (2010–2024, Wednesday labels, snapshot
  truncated one week to mimic reporting lag) and writes a pseudo-hub
  `forecasts`/`truth` pair in hub schema to `cache/calibration/ili_pseudo_hub_*.parquet`
  (~30 min). Rebuilt 2026-09-23 after the latency fix described in "The ILI+
  replay was broken" below; ILI+ results older than that are invalid.
- `scripts/calibration/calibration_harness.R` — targets-based harness for calibrating our
  own forecasters outside the hub path (covid
  `windowed_seasonal_extra_sources`, aheads 0:4, `covid_hosp_evaluation`
  store). WIP.

The index of experiments (what was varied against what, and which results
are stale) is `notes/calibration-ledger.md`.

## Learning truth (2026-09-30)

Until 2026-09-30, `calibrate_hub_forecasts()` learned only from `truth`, the
latest NHSN vintage: the gradients, the adaptive eta pool and the
`burn_in_quantile` warm start all saw finalized values, and only the timing of
each reveal (`settle_days`) was realistic. Every number in this file below
that date was measured that way.

`learn_truth` now selects what the tracker learns from (scores stay on
finalized truth). A round with reference date `d` sees data published by
`d - 3`. Modes, as `cal_run(learn = )`:

- `"final"`: the old behavior.
- `"exact"`: what prod does. Prod re-runs the tracker from scratch each week on
  that week's data, so at round `t` every revealed outcome has its value as
  of `t`. `calibrate_hub_forecasts_exact()` replays this with one run per
  round (about 1.2 min per config with 12 forked workers).
- `"vintage"`: one run in which each outcome keeps its value at the round
  that revealed it: a stateful tracker that never revisits a step.

Findings (E00, `e00_vintage_backtest.Rmd`, flu, both live seasons, spoiled
submissions excluded):

- Final vs exact is small except at h−1: REF-op at settle 14 is
  +7.6/+5.4/+2.4/+1.8/+2.2 final vs +5.4/+4.9/+2.4/+1.7/+2.2 exact, coverage
  bias 0.067–0.040 vs 0.074–0.041. REF-paper is within 0.6 points.
- Vintage mode is not a stand-in for exact: at settle 7 it learns once from
  the under-reported first report (REF-op h−1 0.0, coverage bias 0.121).
- Under the exact (prod) design, settle 7 beats or ties settle 14 at every
  horizon at both references (REF-op +6.1/+5.1/+2.6/+1.8/+2.2), because next
  week's run corrects what an early reveal got wrong. A stateful tracker would
  prefer the longer delay. The right revision delay depends on the design.

## Constant learning rate × revision delay (E09, 2026-09-30)

`e09_lr_delay.Rmd`: the plain tracker (REF-paper) on the rate scale (per
100k) with a constant learning rate, swept over 0.001–3.2 × extra delay 0–3
weeks, exact and stateful learning truth. Pooled over locations:

- The coverage/WIS trade-off is L-shaped, with a knee at a learning rate of
  about 0.05–0.1 per 100k. At 0.056 and +1 week (re-run): WIS
  −3.2/−2.5/−1.1/−0.5/−0.2% (negative is better), coverage bias
  0.040/0.031/0.026/0.027/0.027. That matches or beats REF-paper
  (−3.2/−1.9/+2.0/+2.7/+5.3%, 0.044/0.030/0.032/0.034/0.032) at every
  horizon.
- With the adaptive rate, rate scale and count scale give identical results
  (the adaptive rate rescales with the location), so the rate scale matters
  only with a constant rate.
- REF-op gains more WIS than any constant rate at h0–h3, at about twice the
  coverage bias.
- Re-run design (prod): shorter delay is better, mostly at h−1/h0. Stateful
  design: zero extra delay makes h−1 coverage worse than base (learning from
  the under-reported first report); one extra week fixes it.

## Covid at the references (E10, 2026-09-30)

`e10_covid.Rmd`, exact learning truth, REF-op without the warm start (as in
covid prod), rounds 2024-11-30 to 2026-09-19 (the spoiled 2024-11-23 round
excluded). The covid base is already calibrated at h1–h3 (coverage bias
0.046/0.036/0.028), so calibration there is neutral to slightly harmful
(REF-op 0.0/−0.6/−1.3% WIS) or harmful (REF-paper −4.5/−7.3/−10.3%). h−1, which over
E10's rounds is 100% the `climate_linear` ensemble (`revision_aware` took
over h−1 only on 2026-09-23), gains +10.2% WIS (REF-paper +11.4%). REF-op's losses come from 2025-26 and from
September–October. Candidate change: calibrate covid at h−1 (maybe h0) only.

## Clean `windowed_seasonal` replay, no burn-in (E12, 2026-10-01)

`scripts/calibration/calibration_ws_replay.R` with `ch_use("{flu,covid}_windowed_seasonal")`,
forecasts read from the evaluation store's clean replay (substitutions off, all
53 geos; harness backfill matches it exactly), h0–h3 (h−1 is left to the
revision-aware methods), exact learning
truth, hub rounds only (flu 55, covid 89). This changes several things at
once compared with E07-B (no burn-in, clean replay, store truth, all geos),
so it is not a one-axis comparison. Pooled WIS change %, h0..h3:

| disease | config | h0 | h1 | h2 | h3 |
|---|---|---|---|---|---|
| flu | E11 constant rate | +1.1 | −0.4 | −0.9 | −1.2 |
| flu | REF-op cold | +2.9 | +1.1 | +0.5 | 0.0 |
| covid | E11 constant rate | +0.5 | −4.9 | −6.9 | −5.1 |
| covid | REF-op cold | +2.8 | −1.3 | −2.6 | −2.3 |

- Flu base bias matches E07-B's `windowed_seasonal` (share of truth above
  the median to within 0.01), so the substitutions barely move the
  aggregate bias.
- Flu E11 cuts coverage bias 4–5× (L1 ~0.02) at a ~1% WIS cost, but
  under-covers the 50% interval (0.45–0.48 at h0–h3). Check that before
  adopting it cold.
- Covid: gains at h0, losses at h1–h3, worst in 2025-26, the same
  season pattern as E10. Supports calibrating covid at h0 only.
- Per-season tables: `cache/calibration/ws_replay_scores_{flu,covid}.csv`.
  Rerun with `burn_in_seasons = "2023-2024"` once the HHS burn-in exists
  (ROADMAP 1b).

## Clean replay with a 2023-24 HHS burn-in (E13, 2026-10-01)

E12's setup plus 2023-24 as a burn-in (HHS data at real versions, back to
2020, stitched before NHSN in `ch_inputs_with_burn_in()`; run
`calibration_ws_replay.R 12 burn_in`). WIS change %, h0/h1/h2/h3, both
seasons pooled, states only (US excluded):

| disease | config | cold | warm |
|---|---|---|---|
| flu | REF-op | 2.0/0.6/0.1/−0.2 | 2.8/1.1/0.9/1.1 |
| flu | E05 single | 1.9/−0.2/−1.1/−1.7 | 2.7/0.4/−0.1/−0.1 |
| flu | E11 constant 0.1 | 0.7/−0.2/−0.7/−1.0 | 1.0/0.0/−0.7/−1.1 |
| covid | REF-op | 1.3/−1.1/−1.7/−1.6 | −0.1/−1.8/−0.4/2.8 |
| covid | E11 constant 0.1 | −0.9/−4.3/−4.8/−3.4 | −2.9/−9.2/−13.2/−17.9 |

- Flu: the warm start adds 0.5–1.6 points for REF-op and E05, but the
  warm configs under-cover the 50% interval (0.40–0.44).
- Covid: 2023-24 over-predicted while the live seasons under-predict at
  h0, so the warm start is wrong-signed there. E11 warm is badly harmful.
- Open thread 1 answered: a warm start doesn't help E11's constant rate
  (flu) and hurts it (covid).
- US is ~45% of all-locations WIS, but states-only and US-only changes
  point the same way. Tables: `cache/calibration/ws_replay_scores_burn_in_*.csv`.

## Constant rate by season (E14) and where the WIS gains come from (E15), 2026-10-01

Flu, clean `windowed_seasonal` replay, exact, cold, states only.
`e14_lr_by_season.Rmd`, `e15_wis_sources.Rmd`.

- E14: a constant rate of 0.018–0.032 per 100k improves WIS at every
  horizon in both seasons and cuts coverage bias in both (2025-26 reaches
  ~0.01 by 0.032; 2024-25 keeps improving up to ~0.3, at a WIS cost past
  0.056). From 0.1 (E11's rate) WIS worsens at h1–h3 in both seasons. The
  knee here is lower than E09's 0.05–0.1 (ensemble, burn-in). Neither
  REF-op cold nor sqrt adaptive is good in both seasons at every horizon.
- E15: offsets are small (median 2–5% of the base 90% width; width ratio
  1.00–1.04), which is why fan plots look unchanged, and nearly always
  upward. Gains come from the center and shoulders, not the tails. For
  REF-op, shifting every quantile by the median offset alone gives more WIS
  than the full calibration; width/shape changes cost WIS and buy coverage.
  E11 0.1 has a long tail of offsets large relative to narrow intervals
  (top 10%: 0.26–0.55 of the 90% width vs 0.08–0.20 for REF-op).

## Data

FluSight CMU-TimeSeries submissions: 83 rounds (2023-10-14 … 2026-05-30), 53
locations, horizons −1…3, 23 quantile levels. Season 2023-24 is a burn-in
(incomplete horizons/locations): its residuals warm the learning rate but no
gradient steps are taken. Waiting 14 days for NHSN to settle makes the reveal
delay exactly `horizon + 2` rounds, so the offset at h3 is always 5 rounds
stale.

As-of NHSN vintages for the gallery come from
`cache/calibration/nhsn_archive_flu.parquet`, copied from the oracle capture
`cache/oracle/flu_hosp_prod/today-main-prmpwlvz/nhsn_archive_data.parquet`
(versions 2024-11-19 … 2026-07-22, so both live seasons are covered; no
vintages exist before 2024-11-19 anywhere). Note: the S3
`nhsn_data_archive.parquet` object stalled at version 2026-01-30 — the
polling job appears to have stopped; refresh the copy from a newer oracle
capture if one exists. Since 2026-09-23 `get_nhsn_data_archive("flu")` (the
cast API, versions 2024-11-19 onward) serves both truth (`nhsn_read_truth()`)
and vintages live; the findings notebook uses it.

## Findings (factorial sweep, `calibration_qt_flu.Rmd`; count space, pre-fix)

Sweep: `season_policy` {carry, reset} × `lr_window` {8, 20, 50, Inf} ×
`lr_mult` {0.3, 0.1, 0.03, 0.01}; results in `cache/calibration/sweep.rds`.

- **The multiplier is the control knob; the window barely matters.** Per
  horizon, WIS-improvement spread across multipliers is 8–63 points; across
  windows 1.5–5. Windows 20/50/Inf are nearly indistinguishable at this data
  size. The expanding-window learning-rate ratchet is real but immaterial.
- **The paper's `mult = 0.1` overpays for calibration**: calibration error
  improves 70–85% but WIS degrades −3.6% (h0) to −22.9% (h3). `mult = 0.03`
  gets equal-or-better calibration error at h ≥ 1 at a fraction of the WIS
  cost (−2 to −6%). `mult = 0.01` is near WIS-parity but converges too slowly
  (bad first-live-season calibration error, 0.07–0.10). The real tradeoff is
  convergence speed vs steady-state sharpness cost.
- **Horizon −1 calibration improves WIS outright (+4%)** — calibration is
  free there.
- **Carry vs reset across the off-season is minor** (carry slightly better at
  h3, otherwise a wash).
- The WIS cost grows with horizon, consistent with `h + 2` staleness: a stale
  additive offset does the most damage where the seasonal ramp is steep.
- Residual scale swings 1–2 orders of magnitude within a season, so an
  additive offset learned near the peak is badly sized for the trough.

**Original operating point (count space): `lr_mult = 0.03`, `lr_window = 20`,
`season_policy = "carry"`** (20 over the untested 15 because it is a
validated grid point and the window is immaterial). At this multiplier the
tracker needs most of a season to warm up, which makes burn-in/state-carrying
the central integration design issue.

**Current operating point: `transform = "sqrt"`, `lr_mult = 0.03`,
`floor = 1e-3`, `lr_window = 20`, carry, `slow_init = "burn_in_quantile"`,
`lr_slow = list(mult = 0.003)`, `fast_decay = 0.1`.** Re-measured 2026-09-23
(after the correctness fixes, NHSN truth, all 53 hub locations): WIS change
+8.9/+5.4/+2.6/+1.9/+1.9 at h−1…3 over both live seasons, calibration error
0.040–0.073 against 0.098–0.118 uncalibrated. Current numbers for every
variant are in `reports/writeups/calibration/calibration_findings_flu.Rmd`
(section "Current numbers" below). With exact learning truth and spoiled
submissions excluded (2026-09-30): +5.4/+4.9/+2.4/+1.7/+2.2, coverage bias
0.041–0.074 against 0.095–0.111 uncalibrated.

**Numbers dated before 2026-09-17 predate the two correctness fixes**
("Correctness fixes" below) and used the hub target-data CSV as truth. The
sqrt-space tables from 2026-09-16 were also hit by the unclamped sqrt
inverse, so they have been replaced; the count-space and 2026-08-27 tables are
kept as a record of how the design was reached, not as current numbers.

## Notebooks

All in `reports/writeups/calibration/`, rendered into `rendered_reports/`.
Each takes a `hub_dir` param (the hub checkout, relative to the repo root;
defaults `../FluSight-forecast-hub` and `../covid19-forecast-hub`) and scores
against the latest NHSN vintage via `nhsn_read_truth()`.

- `calibration_findings_flu.Rmd` — the current summary: base bias on NHSN,
  the same forecaster's bias on ILI+, staleness (lagged correlation of played vs needed
  shift), the leak by month, count vs sqrt, ensemble vs `windowed_seasonal`.
  All numbers are post-fix.
- `calibration_qt_flu.Rmd` — the parameter-sweep EDA. Frozen as
  the sweep record.
- `calibration_qt_seasons_flu.Rmd` — whole-season views of four
  states × two live seasons, one panel per (state, season): every-other-round
  fans (80% band) for h 0–3, the NHSN vintage each round saw painted over its
  fortnight, finalized truth, and an eta strip; plus collapsible internals
  tables at h2 (eta, pre-PAVA hidden offset, post-PAVA offset, base,
  calibrated at 5 levels). The states are picked from the baseline run: best
  and worst by WIS change and best and worst by median absolute-error change,
  pooled over horizons and both live seasons, deduplicated (as of 2026-09-16:
  AK best on both, so HI is the best-AE panel; NY worst WIS; CT worst AE).
  Repeated for six method variants (baseline, off after March 1 / February 15,
  per-level eta, geo-pooled eta, seasonal window) with a headline
  WIS/calibration-error comparison at the top, plus a by-month breakdown of the
  baseline (WIS change, share of base WIS, median shift) that shows where in
  the season the method gains and loses.
- `calibration_qt_gallery_flu.Rmd` — fixed operating point. Slim
  headline table (WIS + calibration error per horizon per season), then a
  ranked per-forecast gallery: top-N (location, round) panels ordered by mean
  absolute quantile displacement relative to the base median. Each panel:
  as-of NHSN snapshot (data the forecaster saw), finalized truth, base and
  calibrated fans (50% + 90% ribbons, horizons −1…3), captioned with MAD, WIS
  delta, and whether truth escaped the base 90% band. Seasons 2024-25 and
  2025-26 only, flu only. A dynamic/paginated app version was considered and
  rejected for now in favor of static top-N HTML.

## Method variants (`calibrate_hub_forecasts()` options; 2026-08-27, pre-fix)

All count space at the original operating point; WIS change vs base, both
live seasons, by horizon −1/0/1/2/3. Pre-fix numbers (see above); the
transform rows measured on 2026-09-16 were removed, see "Current numbers":

| variant | option | h−1 | h0 | h1 | h2 | h3 |
|---|---|---|---|---|---|---|
| baseline | — | +4.3 | +1.8 | −1.9 | −5.3 | −6.7 |
| off after April 1 | `off_after = "04-01"` | +4.9 | +1.4 | −1.3 | −2.8 | −3.9 |
| off after March 1 | `off_after = "03-01"` | +4.5 | +2.0 | +0.4 | 0.0 | +0.2 |
| off after February 15 | `off_after = "02-15"` | +3.0 | +2.5 | +1.3 | +0.8 | +0.9 |
| per-level eta | `lr_args$per_level = TRUE` | +4.2 | +1.8 | −1.3 | −3.4 | −5.4 |
| geo-pooled eta (option since removed) | `lr_geo_pool = <pop>` | +4.4 | +2.0 | −2.2 | −5.5 | −7.7 |
| seasonal eta window (carry) | `lr_window = 10, lr_seasonal = list(half_width_weeks = 5)` | +2.9 | +0.1 | −3.0 | −5.5 | −7.5 |
| seasonal eta window (reset) | same + `season_policy = "reset"` | +3.2 | +1.1 | −2.1 | −4.7 | −7.0 |

- Eta was never pooled across horizons: each (location, horizon) series has its
  own tracker and its own eta (pooled over the 23 levels and the window).
- **Switching off after April 1** (play base, no updates, offsets carry to next
  season) roughly halves the WIS cost at h2/h3 — the spring tail, where
  peak-tuned additive offsets are out of regime, is where most of the damage
  was.
- **Switching off after March 1 removes the WIS cost entirely** (h1–h3 within
  ±0.4 of base, h−1/h0 still positive) — the out-of-regime tail starts around
  March, not April. First variant that is WIS-neutral-or-better at every
  horizon.
- **February 15 is better still at h0–h3** (+2.5/+1.3/+0.8/+0.9,
  WIS-*positive* everywhere) at the cost of ~1.5 points at h−1 vs March 1 —
  the post-peak descent is already out of regime, not just the spring tail.
  The notebook now runs the March-1 and Feb-15 cutoffs (April 1 dropped).
- **Per-level eta** is a modest gain at h ≥ 1: the pooled 0.9 quantile of
  |residual| is dominated by the outer levels and over-steps the median.
- **Geo-pooled eta** gives visibly smoother eta trajectories but no WIS gain.
- **Seasonal window** is worse everywhere on the headline. In the panels its
  eta at season start is *smaller* than baseline's (last year's same weeks were
  a slow ramp), so the carried-over spring offsets take longer to unwind.
  Reset instead of carry recovers some of that (h2 −4.7) but still trails
  baseline; 2024-25 is fine (+5.7/+2.8/+0.1 at h−1/0/1), 2025-26 is uniformly
  worse. The notebook's seasonal section now runs the reset variant.
- **Transforms** (`transform = "log1p"` / `"sqrt"` / `"quartic_root"`, the
  last being the forecasters' whitening map `(x + 0.01)^0.25`). Y and all 23
  base quantiles are mapped, residuals/eta/offsets/PAVA all live in the mapped
  space, the played quantiles are mapped back and clamped at zero, and scoring
  is on counts. Coverage indicators are invariant under a monotone map, so the
  gradient is identical to count space; only the step geometry changes, and
  multipliers are not comparable across spaces. The 2026-09-16 measurements
  of these variants predate the correctness fixes (the sqrt ones were also hit
  by the unclamped inverse) and were removed. Re-measured since: sqrt vs
  count only, in "Current numbers" below. Log space and fourth root have not
  been re-run; before the fixes log space was worse than count space at h ≥ 1
  and fourth root at mult 0.03 under-stepped, but neither result should be
  relied on. If scale-awareness is retried beyond sqrt, normalize by a smooth
  scale proxy (trailing 4-week truth, floored) rather than a log.

## Where the WIS moves (2026-09-16, count-space baseline, pre-fix)

By calendar month of the reference date, both live seasons pooled. These are
pre-fix count-space numbers; the mechanism (the spring and October losses,
the stale wrong-signed offset) is re-confirmed post-fix for the sqrt variants
in the findings notebook, with the numbers in "Current numbers" below.

- **The mass is in December–February.** At h3, Dec + Jan carry ~75% of the
  season's base WIS and Mar–May ~7%. Calibration is −2% in Dec/Jan at h2/h3
  and −33/−120/−84% in Mar/Apr/May. Of the −6.7% headline at h3, roughly 56%
  comes from the spring on tiny mass and 44% from small losses on the huge
  Dec–Feb mass. The gains are Feb at h−1/h0 (+7.6/+4.1%) and Mar at h−1
  (+11.5%).
- **The base median is biased low almost everywhere**, by 10–60% of truth
  (Dec: 37–59% at h0–h3, truth above the base median in 90% of December
  forecasts). The direction of the needed correction is stable; only its
  magnitude swings, by two orders of magnitude within a season.
- **The tracker gets the sign wrong at every turn**, because the offset is a
  5-round-stale count. NY h3, 2025-26: Oct 18 opens with hidden = +159 carried
  from spring; the five May outcomes revealed that round (where the played
  forecast had over-covered) knock it to +15, then −9 by Nov 22. Then
  Nov 29 – Dec 20 reveal *nothing* (the 5-week Oct/Nov gap plus the 5-round
  delay), so the offset sits at −9 while base under-predicts by 500–1900.
  January pushes it to +148 chasing December, when truth is already falling;
  February over-corrects back to −13; spring climbs to +281 on a base of
  26–47 with eta stuck at 85 (the 20-round window still holds January
  residuals in the hundreds). The calibrated h3 median is *below* base in 90%
  of December forecasts across all states.
- **Eta is not the lever.** The gradient is a coverage indicator whose sign is
  set by outcomes 5 rounds old; eta only scales how fast the offset chases
  it. Any eta large enough to unwind a stale offset quickly is large enough
  to have built it. That is why the seasonal-eta variants lose (small eta at
  season start slows the unwind of last spring's offsets) and why the
  cutoff variants win (they stop the chase where it is wrong-signed and,
  with carry, hand the next season a mid-February offset instead of a May
  one). The multiplier sweep in `calibration_qt_flu.Rmd` said the same thing
  from the other side: mult trades convergence speed against overshoot with
  no setting that wins both.

### Two-term offset: slow + fast (2026-09-16)

`qt_track()` now carries `hidden = slow + fast`. Both take the same coverage
gradient; `fast` is the paper's iterate (short eta window, obeys the season
policy, optional per-round leak `fast_decay`), `slow` has its own long window
and small multiplier, never decays, ignores the season policy, and may train
through burn-in (`burn_in_learns_slow`). Without `lr_slow` and with
`fast_decay = 0` the output is bit-identical to before (tests pin this).

The 2026-09-16 comparison table for this section (sqrt space) predated the
correctness fixes and was removed; the post-fix numbers for the variants that
were re-run (single term, leak 0.1, and the warm-start combinations) are in
"Current numbers" below. What still holds:

- **The leak is what fixes the spring and h2/h3.** Re-measured: decay 0.1
  alone takes h3 from −1.4% to +0.5% and cuts the h3 March/October losses from
  about −47% to −17% and −48% to −14%, but gives back most of the coverage
  gain (calibration error 0.022–0.043 single-term vs 0.073–0.089 leaky): a
  correction that must be continually refreshed cannot hold coverage through
  a 5-round reveal lag.
- **A slow gradient term on its own** (no warm start) was measured only
  before the fixes: at mult 0.01 it barely moved over one burn-in plus one
  live season, and at mult 0.03 it acted like a second fast term. Not re-run;
  the warm start below replaced it as the way to hold the persistent level.

### Is a learned seasonal offset viable?

Checked on the only pair of live seasons with the same base forecaster
(2024-25 → 2025-26, 53 locations × 27 season-rounds, aligned by
`season_round`), base-median residual relative to the base median:

| horizon | cor(rel residual, same season_round, YoY) | Spearman |
|---|---|---|
| −1 | −0.01 | 0.02 |
| 0 | 0.05 | 0.13 |
| 1 | 0.13 | 0.21 |
| 2 | 0.32 | 0.25 |
| 3 | 0.30 | 0.26 |

Raw count residuals correlate at 0.4–0.7, but that is geography (big states
have big residuals every year), not seasonality. Smoothing last year's
residual over ±2…8 season-rounds does not raise the correlation (it drifts
down slightly), and aligning seasons on each location's peak round instead of
the calendar changes nothing, because the two seasons peaked within 3 rounds
of each other (median season_round 11 vs 8) so the two axes coincide.
Pooling the prior nationally helps a little (0.45 at h2). In fourth-root
residuals the correlations are higher (0.14–0.19 at h0, 0.36–0.44 at
h2/h3), since that is a variance-stabilized quantity. So: weak-to-moderate
year-over-year signal at long horizons, none at short, from one season pair
whose phases happened to line up; the test of whether phase-alignment matters
needs a season that peaks late. The learned hidden offsets
themselves correlate YoY at 0.11 (h2 median). Pooled nationally by 4-round
bin, the relative residual is positive in nearly every bin both years but
its size differs by year (bin 5: +56% vs +20%; bin 17: +23% vs −29%), and
the 2025-26 axis is shifted by the 5-week Oct/Nov gap. Conclusion: a
per-(location, horizon, season-week) offset learned from one prior season is
noise at h ≤ 1 and weak at h2/h3; the stable, learnable component is a
*constant* low bias, not a seasonal profile. Revisit after more seasons of a
stable base forecaster, and align on peak-relative rather than calendar
weeks if so.

### Warm start from the burn-in season (2026-09-16)

`slow_init = "burn_in_quantile"` sets the slow term per (horizon, level) to the
conformal offset implied by the burn-in rounds pooled over locations: the
`level`-quantile of `Y − Yhat[level]` in working units. The tracker then runs
on top of it. Same setup as the two-term table (sqrt, mult 0.03, window 20,
carry, hub 2023-24 burn-in). The 2026-09-16 table was pre-fix and was removed;
re-measured numbers are in "Current numbers" below.

- **One season of batch conformal offsets is already WIS-positive at every
  horizon** (+0.4 to +1.9, re-measured) while removing a third of the
  coverage error: the persistent low bias is real and cheap to correct.
- **Warm start + leaky fast tracker is WIS-positive at every horizon** and
  halves the coverage error (+8.3/+5.1/+2.6/+2.0/+2.1, calibration error
  0.046–0.080 vs 0.098–0.118 base). Its h3 gain comes from December–January
  (+4% each, the months holding ~75% of the h3 base WIS); its spring losses
  are only partly contained by the leak (h3 March −28%), because the warm
  start lives in the slow term, which does not decay.
- Adding a slow gradient term on top of the warm start was, before the fixes,
  a small dial between WIS (mult 0.003) and coverage (mult 0.01); the chosen
  point is 0.003 with decay 0.1. Post-fix only 0.003 was re-run.
- The h3 warm-start offsets at the outer levels are large (0.95 level: +2.9
  scaled-sqrt units for CA) because the 2023-24 ensemble was badly
  under-dispersed at long horizons; it is the correct conformal answer for
  that season but a reminder that a one-season warm start inherits that
  season's base forecaster. In prod the warm start would come from the
  previous live season(s) of the *current* forecaster.

### ILI+ burn-in (2026-09-16): invalid, measured on the broken replay

The idea: the prod flu forecasters already train on ILI+ (percent positive ×
percent ILI, state level, 2010–2024) as faux-versioned augmentation rows, so
*forecast* the ILI+ seasons with the same `windowed_seasonal` forecaster and
use those residuals as a multi-season burn-in (13 usable seasons instead of
one). The pseudo-hub tables are on the ILI+ percent scale, so the tracker runs
in scaled units: `scales` divides each location's rounds by an era-specific
divisor (90th percentile of in-season truth) before the sqrt transform, so
offsets and eta learned on ILI+ carry over as fractions of a typical
season-peak level.

The result recorded here on 2026-09-16 (ILI+ residuals carry no information
about the live bias; an ILI+-trained slow term costs 130–500% WIS) was
measured on a broken replay (see "The ILI+ replay was broken" below) and has
been removed. The `scales` machinery is fine and stays.

The useful implication is the opposite one: the hub's own 2023-24 burn-in
*does* share the live bias, so the slow term should be warm-started from it
directly (`slow_init = "burn_in_quantile"`: the conformal offset per horizon
and level, pooled over locations) rather than accumulated by gradient steps.

### Correctness fixes (2026-09-17)

A review of the calibration code found two defects that affect every table
above dated 2026-09-16; the fixes are on `ds/calibrate` as separate commits.

1. **`sqrt` inverse was `x^2`, not `pmax(x, 0)^2`.** The isotonic projection
   runs on the sqrt scale, so a played vector with negative low quantiles is
   monotone there but squaring folds the negative part back up and the
   count-space quantiles cross. On the hub run this hit 578 of 20,393 forecast
   sets in the single-term baseline and 1,708 at the operating point, almost
   all at levels 0.01–0.05 in small geos at the trough. `quartic_root` already
   clamped; `sqrt` now does the same, and `tests/testthat/test-calibrate.R`
   exercises the negative regime.
2. **Gating was by reveal round, not issue round.** `update_from[t]` gated the
   step *taken at* round `t`, but that step applies gradients of the rounds in
   `delay[[t]]`, which at the first live round are the last `h + 2` burn-in
   rounds (revealed in a burst after the off-season gap). So the live season
   opened with several stale spring gradients, and `off_after` rounds played at
   base were learned from the following October. Both masks are now indexed by
   the round a forecast was issued at; burn-in and switched-off outcomes still
   feed the eta pool but never step.

Re-measured after both fixes (sqrt, mult 0.03, floor 1e-3, window 20, carry,
hub 2023-24 burn-in), WIS change vs base by horizon −1…3 and calibration error
range. The issue-round gating is what removes most of the h2/h3 loss of the
single-term tracker: the stale burst at season start was a large part of it.

| variant | h−1 | h0 | h1 | h2 | h3 | cal err |
|---|---|---|---|---|---|---|
| sqrt, single term (was +8.0/+4.1/−0.4/−3.1/−3.4) | +8.0 | +4.5 | +0.8 | −0.6 | −1.4 | 0.022–0.043 |
| operating point: warm start + slow 0.003 + fast decay 0.1 (was +9.0/+5.3/+1.9/+0.8/+0.5) | +9.0 | +5.6 | +2.7 | +1.9 | +1.9 | 0.040–0.072 |

Also in the same set of commits: `lr_geo_pool` removed (never beat baseline);
`lr_slow` aborts with a constant `lr` or per-level eta (it silently ignored its
own `mult` before); `off_after` and `fast_decay` abort when combined (the leak
would run through the off window, so "frozen offset" would be false); the
hub reader aborts on duplicate (round, level) rows; the accidentally committed
`covid_hosp_prod/workspaces/` binaries were dropped from history and the
pattern is now ignored. The older notebooks have not been re-rendered since;
`calibration_findings_flu.Rmd` has the post-fix numbers.

### The ILI+ replay was broken (fixed 2026-09-23)

Symptom: `windowed_seasonal`'s ILI+ pseudo-hub forecasts did not follow their
input. Log-log slope of median on truth was 0.40 at h0 (0.29 at h3). Indiana
on 2018-01-03 had a last observed value of 2.28 and an h0 median of 0.14.

Cause: `adjust_latency = "extend_lags"` (the `default_args_list()` default)
computes one latency per column as `forecast_date - min over geos of that
geo's last time_value`, i.e. the *worst* state, and adds it to every lag. The
ILI+ archive has states whose series end years before the forecast date (DC
stops 2015-04-08; `hhs > 1e-4` also drops trailing zero weeks). On 2018-01-03
the latency was 1001 days, so every predictor was about three years old.
Every round from 2011 on was affected, at every horizon, with latencies from
weeks to about nine years. The NHSN replay was not affected: all NHSN states
are current, and the ILI+/flusurv augmentation rows are in `keys_to_ignore`.
The same trap applies to any archive in which one geo stops reporting, NHSN
included (a single late state would stretch every state's lags).

Fix: `ili_forecast_one()` passes the states whose last observation predates
the snapshot's latest week as `keys_to_ignore = list(list("geo_value", ...))`,
so they are left out of the latency check. They still contribute training
rows; they get no forecast that round (forecast locations per round are
unchanged: min 26, median 45, max 48). Indiana's h0 median is now 2.44.

Checks on the rebuilt replay (all seasons): log-log slope 0.73/0.86/0.78/
0.69/0.59 at h−1..h3. The overall slope is pulled down by the 43% of points
near the trough (truth under 10% of the location's 90th percentile, where
the zero clip and whitening flatten the median); above that the slope is
0.92/0.98/0.90/0.81/0.70.

h−1 is not an echo of the last observed week, in this replay or in prod.
`filter_minus_one_ahead()` drops the week at `as_of + ahead`, because
otherwise the target column equals the extended lag-7 predictor and
`quantile_reg` freezes. With that week gone, the latency is 14 days and h−1
is a one-week forecast from the week before.

Still invalid, not re-run on the fixed replay: the ILI+ burn-in result above
and setup C in "One forecaster across eras" below. The bias on ILI+ is in
section 1.2 of the findings notebook: about level (43–46% of truth above the
median; summed level within ±4% at every horizon), with the same peak
shortfall at h3 as NHSN. So the "bias flips sign with the data source"
reading does not hold. On ILI+ the base is about level on average, not
biased high.

### Current numbers (2026-09-23; superseded by the exact re-runs below)

Re-run 2026-09-30 with exact learning truth (the prod re-run design) and
spoiled submissions excluded (`notes/spoiled-submissions.md`) in
`e03_scale.Rmd`, `e04_offset_structure.Rmd`, `e05_warm_start.Rmd`. WIS change
% (positive is better) and coverage bias, same setup (base
0.111/0.105/0.095/0.098/0.102):

| variant | h−1 | h0 | h1 | h2 | h3 | cal err h−1…h3 |
|---|---|---|---|---|---|---|
| count space, single term | +3.2 | +1.9 | −2.0 | −2.7 | −5.3 | 0.044/0.030/0.032/0.034/0.032 |
| sqrt, single term | +5.1 | +4.0 | +0.7 | −1.3 | −1.4 | 0.049/0.032/0.026/0.028/0.026 |
| sqrt, leak 0.1 | +4.8 | +3.7 | +1.6 | +0.8 | +0.6 | 0.090/0.082/0.075/0.079/0.081 |
| warm start only | +0.5 | +1.3 | +0.9 | +1.2 | +2.1 | 0.098/0.075/0.062/0.065/0.062 |
| warm start + single term | +5.1 | +4.7 | +1.3 | −0.2 | +0.3 | 0.046/0.020/0.013/0.016/0.014 |
| warm start + leak 0.1 | +5.0 | +4.5 | +2.3 | +1.8 | +2.4 | 0.080/0.060/0.050/0.052/0.047 |
| operating point | +5.4 | +4.9 | +2.4 | +1.7 | +2.2 | 0.074/0.054/0.045/0.046/0.041 |

The ordering of the variants is unchanged from the finalized-truth table.
Warm start + single term (not run before) is the best-calibrated variant, and
with REF-op it brackets the WIS/coverage trade-off. At h3, sqrt single term
loses more than count in October (−35% vs −27%) and March (−42% vs −26%);
the leak brings those to −12% and −14%. The finalized-truth numbers follow for
the record.

From `reports/writeups/calibration/calibration_findings_flu.Rmd`: post-fix,
truth from `nhsn_read_truth()`, all 53 hub locations, the submitted ensemble,
sqrt unless stated, mult 0.03, floor 1e-3, window 20, carry, 2023-24 burn-in,
both live seasons pooled. WIS change % vs base, then calibration error
(base 0.118/0.105/0.095/0.098/0.102):

| variant | h−1 | h0 | h1 | h2 | h3 | cal err h−1…h3 |
|---|---|---|---|---|---|---|
| count space, single term (the paper) | +4.2 | +2.4 | −0.8 | −3.4 | −5.6 | 0.033/0.020/0.025/0.027/0.029 |
| count space, leak 0.1 | +6.0 | +2.9 | +0.6 | +0.3 | −0.8 | 0.079/0.062/0.061/0.066/0.065 |
| sqrt, single term | +7.9 | +4.4 | +0.7 | −0.5 | −1.4 | 0.043/0.027/0.022/0.023/0.022 |
| sqrt, leak 0.1 | +8.1 | +4.4 | +2.0 | +1.1 | +0.5 | 0.089/0.078/0.073/0.078/0.080 |
| warm start only, no tracking | +0.4 | +1.1 | +0.8 | +1.1 | +1.9 | 0.105/0.076/0.063/0.066/0.062 |
| warm start + leak 0.1 | +8.3 | +5.1 | +2.6 | +2.0 | +2.1 | 0.080/0.057/0.049/0.050/0.046 |
| operating point (warm start + slow 0.003 + leak 0.1) | +8.9 | +5.4 | +2.6 | +1.9 | +1.9 | 0.073/0.051/0.042/0.044/0.040 |

h3 WIS change % by month of the reference date, and each month's share of the
h3 base WIS:

| variant | Oct | Nov | Dec | Jan | Feb | Mar | Apr | May |
|---|---|---|---|---|---|---|---|---|
| sqrt, single term | −48.4 | +6.5 | +1.9 | +0.6 | −1.9 | −47.3 | −36.2 | +20.1 |
| sqrt, leak 0.1 | −13.5 | +5.2 | +0.6 | +1.2 | +1.3 | −17.2 | +8.5 | +6.8 |
| warm start + leak 0.1 | −26.9 | +5.7 | +4.1 | +4.1 | +0.6 | −27.5 | −4.7 | +3.3 |
| operating point | −28.9 | +6.4 | +4.3 | +4.2 | 0.0 | −33.2 | −6.6 | +5.4 |
| share of base WIS % | 0.2 | 6.1 | 41.5 | 33.5 | 11.8 | 4.7 | 1.1 | 1.1 |

Staleness, measured directly: correlate the played median shift at round `t`
with the needed shift (`sqrt(truth) − sqrt(base median)`) at round `t − k`,
per series, averaged over locations (single-term sqrt tracker). At `k = 0` the
correlation is negative from h0 up (−0.22/−0.25/−0.31/−0.35 at h0…h3): the
offset currently played pushes the wrong way on average. It peaks at
`k = 4/6/7/9` for h0…h3, i.e. the reveal lag `h + 2` plus two to four rounds of
build-up. By direction alone (share of >1% moves with the right sign), no
tracker beats the static warm start at h0–h3 (67–69%); the single-term count
tracker is worst (56–62%).

Base bias on NHSN (share of forecasts with truth above the median, h−1…h3):
ensemble 69/65/63/63/63, `windowed_seasonal` 58/63/64/67/70; summed median vs
summed truth −11…−38% and −8…−46%. At h3 the bias concentrates at the peak
(median 68–69% low when truth is above the location's 90th-percentile level);
at h0 it is a fairly even 5–20% low.

Count vs sqrt: the ratio of the relative median shift at the trough bin to
that at the peak bin is 32–38 in count space and 5–10 in sqrt.

### One forecaster across eras (2026-09-17)

Question: are the hub-season gains an artifact of three seasons of one
ensemble, and does the tracker behave the same on a decade of the *same*
forecaster? `scripts/calibration/calibration_ws_backfill.R` replays flu prod's
`windowed_seasonal` (prod archive, seed, substitutions) over the NHSN seasons
in hub schema: 2024-25 and 2025-26 as honest as-of replays, 2023-24 with the
`"cheating"` policy (NHSN vintages start 2024-11-19). With the ILI+ pseudo-hub
(`scripts/calibration/calibration_ili_backfill.R`) this is one forecaster from 2011 to
2026. `scripts/calibration/calibration_ws_experiments.R` runs three setups, all sqrt,
mult 0.03, window 20, carry, on the 50 locations common to all sources and
on the hub's submission rounds only:

- **A** submitted ensemble, burn-in 2023-24 (the reference).
- **B** `windowed_seasonal` on NHSN, burn-in 2023-24.
- **C** `windowed_seasonal` on the ILI+ decade (2011-12 burn-in, 2012-2020 and
  2022-23 tracked live) then NHSN 2023-26, whitened per location and era by
  the 90th percentile of in-season truth (`scales`), offsets carried across
  the source switch. **C is invalid**: its ILI+ half comes from the broken
  replay (above). Its rows and the ILI+ numbers were removed.

Variants: `single` (paper's tracker), `leaky` (`fast_decay = 0.1`),
`operating` (warm start + slow 0.003 + leaky fast). WIS change % vs base
(post-fix, hub target-data truth, 50 locations; the findings notebook re-runs
A and B on NHSN truth, with the same pattern):

| setup / variant | 2024-25 h−1…h3 | 2025-26 h−1…h3 |
|---|---|---|
| A single | +11.7 / +2.6 / +1.1 / −0.1 / −1.3 | −2.6 / +2.0 / −2.1 / −3.4 / −2.8 |
| A operating | +11.2 / +3.8 / +2.6 / +2.5 / +3.8 | +0.4 / +3.9 / +0.2 / −0.5 / −0.7 |
| B single | +0.7 / +2.0 / +0.3 / −0.7 / −1.5 | −1.4 / +2.3 / −0.5 / −0.7 / −0.3 |
| B leaky | +1.4 / +2.1 / +0.8 / +0.3 / −0.3 | −0.3 / +1.7 / +0.4 / +0.8 / +1.3 |
| B operating | +2.1 / +2.7 / +1.1 / +0.7 / +0.8 | −0.1 / +2.3 / +0.4 / +0.5 / +0.6 |

Calibration error (mean over levels; base first) for B:
2024-25 base 0.074–0.168, single 0.053–0.088, operating 0.048–0.093;
2025-26 base 0.044–0.088, single 0.007–0.016, operating 0.016–0.042.

Fraction of truth above the base median (the sign of the bias):

| base | 2023-24 h−1…h3 | 2024-25 | 2025-26 |
|---|---|---|---|
| ensemble | 0.56 / 0.55 / 0.55 / 0.53 / 0.52 | 0.83 / 0.74 / 0.73 / 0.74 / 0.76 | 0.58 / 0.63 / 0.59 / 0.60 / 0.60 |
| windowed_seasonal on NHSN | 0.55 / 0.54 / 0.58 / 0.62 / 0.68 | 0.59 / 0.68 / 0.68 / 0.72 / 0.76 | 0.58 / 0.66 / 0.65 / 0.65 / 0.65 |

Reading:

- **The low bias is not the ensemble's alone.** `windowed_seasonal` on NHSN
  under-predicts in all three seasons (0.55–0.76), growing with horizon, so
  there is something to correct; the gains are smaller than the ensemble's
  (+0.4 to +2.7% at the operating point vs up to +11%) because the component
  is less miscalibrated to begin with (base cal err 0.07–0.17 vs the
  ensemble's 0.13–0.19 in 2024-25) and because the ensemble's h−1 is a
  different, worse model (climate_linear).
- **On NHSN with one forecaster (B)** the single-term and operating variants
  are close, with `operating` slightly ahead on WIS and `single` far ahead on
  coverage in 2025-26; the leaky variant gives small, consistent WIS gains
  with the weakest coverage gains.
- **Overfitting verdict (NHSN only).** The tracker is not an artifact of the
  ensemble: it helps `windowed_seasonal` the same way, with smaller gains.
  Whether the warm start transfers to a different data source is open (the
  ILI+ evidence for "it does not" came from the broken replay). A prod design
  should warm-start from the *current* forecaster's most recent NHSN season,
  or use the leaky tracker with no warm start when that season is
  unavailable.

### Directions this points to, ranked

1. **Split the offset into a slow level term and a fast tracker.** Done
   (two-term tracker + conformal warm start, above); this is the current
   operating point. Open: warm-starting from more than one season, and a
   relative (log-space) slow term so the warm start transfers across the
   scale swing more evenly than a sqrt-space constant does.
2. **Attack the staleness directly, not eta.** h−1 is free because its lag is
   one round; h3 loses because its lag is five. Short-horizon coverage
   indicators for the same target weeks are known ~4 rounds before h3's
   own, and the base forecaster's bias direction is shared across horizons
   (all horizons under-predict in the ramp). Feeding the fresh h−1/h0
   gradient into h2/h3's update (a cross-horizon proxy signal) is the only
   idea on the list that changes *when* the offset turns rather than how fast.
   The lagged-correlation measurement in "Current numbers" is the baseline to
   beat: a fix should move the best-matching lag toward `k = 0`.
3. **Phase-gate updates on data, not the calendar.** The Feb-15 cutoff works
   because both live seasons peaked around the turn of the year; a
   late-peaking season would break it. Gating on the base median's trailing
   trend (post-peak ⇒ stop updating, or shrink) expresses the same rule
   without a hard-coded date.
4. **Scale-aware offsets via a smooth proxy**, if sqrt is not enough (the
   log-space result is pre-fix and was not re-run).
5. **Eta variants** (seasonal, per-level, pooled): parked. None can fix a
   wrong-signed step.

## Production integration (2026-09-21)

Calibrated forecasts are now threaded into both flu and covid production pipelines as a secondary submission, `CMU-TimeSeries-Calibrated`, leaving the primary `CMU-TimeSeries` ensemble unchanged.

**Architecture:** three new targets added to each prod script, executed after the ensemble:

- `calibrated_ensemble_nhsn` (`cue = tar_cue("always")`) — reads all past `CMU-TimeSeries` submissions from the hub checkout via `hub_read_forecasts()`, appends the current round's `ensemble_mix` output converted to hub schema via `internal_to_hub_forecasts()` if not already present, reads finalized truth, and calls `calibrate_hub_forecasts()`.
  Skips (returns `NULL`) when `g_submission_directory == "cache"` (no hub checkout set).
- `make_calibrated_submission_csv` — writes the calibrated quantiles to `model-output/CMU-TimeSeries-Calibrated/<reference_date>-CMU-TimeSeries-Calibrated.csv` in the hub checkout.
- `local_calibrated_scores_nhsn` — scores the calibrated forecasts against `nhsn_latest_data` via `score_forecasts()`; fed into the ongoing score report alongside the base forecasters.

**Operating point (flu):** `transform = "sqrt"`, `lr_args = list(mult = 0.03, floor = 1e-3)`, `lr_window = 20`, `season_policy = "carry"`, `slow_init = "burn_in_quantile"`, `lr_slow = list(mult = 0.003)`, `fast_decay = 0.1`, `burn_in_seasons = "2023-2024"`.
Flu gets the 2023-24 burn-in for free: the hub checkout already contains all past CMU-TimeSeries flu submissions.

**Operating point (covid):** same except `burn_in_seasons = character(0)`, `slow_init = NULL` (no hub burn-in available; covid submissions begin 2024-11-23).

**Notebooks:** `CMU-TimeSeries-Calibrated` is added to `ongoing_score_report.Rmd` (`our_forecasters`, score table styling, line width).
The per-date `forecast_report.Rmd` notebook receives calibrated forecasts for the current round's reference date, converted back to internal schema (state abbr, Wednesday `forecast_date`, count-space quantile values) via the `notebook` target in `prod_shared.R`.

**What the covid submission actually is:** the submitted covid forecast is `ensemble_mix`, whose components are `windowed_seasonal`, `windowed_seasonal_extra_sources` and `revision_aware` (`pipelines/covid_hosp_prod.R`). Its raw weights come from `pipelines/covid_geo_exclusions.csv`, and `ensemble_weighted()` renormalizes them per (geo, ahead). The 2026-09-23 block sets `windowed_seasonal = 0.5`, `windowed_seasonal_extra_sources = 3` and `revision_aware = 1` at aheads −1 and 0 only. The effective mix is therefore:
- aheads 1–3: ~86% `windowed_seasonal_extra_sources`, ~14% `windowed_seasonal`;
- ahead 0: ~67% / ~11% / ~22% `revision_aware`;
- ahead −1: 100% `revision_aware`, because `drop_negative_aheads` strips the two AR components there.

Before 2026-09-23 (all of E10's rounds), the mix had no `revision_aware`: ahead −1 was 100% the `climate_linear` ensemble, since the AR components were filtered out of negative aheads.

`windowed_seasonal_extra_sources` excludes mo and wy, so those states fall back to `windowed_seasonal`. Blocks before 2026-09-16 used `windowed_seasonal = 0.05`, which made the split ~98% / ~2%. The `climate_linear` rows in the CSV are not `ensemble_mix` components. So covid forecast-quality work, calibration included, is mostly about `windowed_seasonal_extra_sources`. The CSV is edited by hand each week, so re-derive the weights before relying on these numbers.

**Model metadata:** `CMU-TimeSeries-Calibrated.yml` created in both `../FluSight-forecast-hub/model-metadata/` and `../covid19-forecast-hub/model-metadata/` (`designated_model: false`).

**Bug fixed en route:** `nssp_archive` in both `flu_data_targets.R` and `covid_data_targets.R` had a duplicate-key error (pre-existing since August 2026): `compactify = TRUE` can retain multiple rows per `(geo_value, time_value)` when the value changed across issues, and the subsequent `mutate(version = time_value + 7)` flattened all to the same version, producing duplicate keys in `as_epi_archive()`.
Fix: `group_by(geo_value, time_value) %>% slice_max(version, n = 1L, with_ties = FALSE) %>% ungroup()` before the version clobber.

# Open threads

Calibration-specific open items, roughly in priority order. Repo-wide items
(evaluation testbed, NSSP-target backtesting, revision-aware backtests) are
in `notes/ROADMAP.md`.

1. **Warm start on the constant-rate tracker.** E11 (constant 0.1 per 100k,
   rate scale, +1 week, re-run) beats the cold adaptive trackers on coverage
   and is the most stable across seasons, but `warm + sqrt, adaptive` still
   wins at h0–h1. Try E11's constant rate with
   `slow_init = "burn_in_quantile"`.
2. **Calibrate h0 and above only.** Revisit h−1 calibration once prod's h−1
   component is chosen (`notes/ROADMAP.md`, revision-method backtests); the
   harness h−1 history predates the 2026-09-23 switch to `revision_aware`.
3. **Covid per-horizon calibration** (calibrate h−1 and perhaps h0, pass
   h1–h3 through; E10): untested.
4. **Re-run E07, E01 and E02 on vintages** (still marked stale in the
   ledger). E03–E05 already use the re-run design.
5. **Re-run the ILI+ experiments on the fixed replay**: the ILI+ burn-in and
   setup C (does a warm start learned on ILI+ transfer to NHSN?).
6. **Sweep `windowed_seasonal`, the harness proxy, for spoiled
   submissions.** Needs a variant of the sweep script that reads the harness
   forecasts (`notes/spoiled-submissions.md`).
7. **`windowed_seasonal_extra_sources` retrospective** via
   `scripts/calibration/calibration_harness.R` (covid, evaluation store): does
   calibration help our best component forecaster, not just the submitted
   ensemble?
8. **Review the ranked gallery.** Does the WIS cost concentrate at turning
   points (the `h + 2` staleness prediction)? Are the biggest offsets fixing
   real miscalibration or chasing data-revision artifacts?
9. **Method extensions**, in the order argued under "Directions this points
   to": slow relative level term + fast tracker; cross-horizon proxy
   gradients for staleness; data-driven phase gating; scale proxy. Pooling
   offsets across locations and per-level learning rates are lower priority.
10. **Speed up exact tracker runs**, if more exact sweeps are coming. An
    exact config replays prod's weekly re-run, about 55 full tracker runs,
    so cost grows with the square of the number of rounds (E09: about 72
    configs, about 1.5 h on 14 cores).
    - Quick wins, all exact, about 2–2.5x: sort-based "data as of round t"
      lookup instead of a grouped slice_max (about 4–5 s per round);
      validate `arg_match` once outside the loop (about 15%); skip isoreg
      when quantiles are already ordered (about 8%); plain vectors instead
      of tibble/glue in the per-series loop (about 15%). Check bit-for-bit
      against cached runs.
    - Checkpointing, another 5–10x: start round t's run from round t−1's
      tracker state at the first round whose data differ. Needs
      `qt_track()` to save and resume offsets, the learning-rate window and
      the slow term; test for exact equality with the full re-run.
11. **Send the collaborator email** (sweep findings; h−1 free win; carry vs
    reset a wash).
12. **Agentic triage** of gallery panels against a fixed rubric (data
    revision visible? base missed the turn? calibration helped/hurt?) if
    manual skimming of the ranked gallery proves insufficient.
13. **Dynamic gallery app** (Shiny/Posit or a served page with dynamic data
    fetch) if the static top-N format becomes limiting.
