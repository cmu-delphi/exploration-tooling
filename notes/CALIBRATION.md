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
  (~30 min). **Its forecasts are broken** (see "The ILI+ replay is broken"
  below); nothing built on it should be trusted until it is fixed and rebuilt.
- `scripts/calibration/calibration_harness.R` — targets-based harness for calibrating our
  own forecasters outside the hub path (covid
  `windowed_seasonal_extra_sources`, aheads 0:4, `covid_hosp_evaluation`
  store). WIP.

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
(section "Current numbers" below).

**Numbers dated before 2026-09-17 predate the two correctness fixes**
("Correctness fixes" below) and used the hub target-data CSV as truth. The
sqrt-space tables from 2026-09-16 were also hit by the unclamped sqrt
inverse, so they have been replaced; the count-space and 2026-08-27 tables are
kept as a record of how the design was reached, not as current numbers.

## Notebooks

All in `reports/writeups/calibration/`, rendered into `rendered_reports/`.
Only `calibration_findings_flu.Rmd` runs as-is: the other six still call
`hub_read_truth()` / `hub_read_covid_truth()` (and the covid ones
`HUB_COVID_DIR`), which were removed on 2026-09-23. Switching the flu ones to
`nhsn_read_truth("flu")` is a one-line change each.

- `calibration_findings_flu.Rmd` — the current summary: base bias on NHSN,
  the broken ILI+ replay, staleness (lagged correlation of played vs needed
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

### ILI+ burn-in (2026-09-16): invalid, the replay is broken

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
measured on a broken replay (see "The ILI+ replay is broken" below) and has
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

### The ILI+ replay is broken (2026-09-23)

`windowed_seasonal`'s ILI+ pseudo-hub forecasts
(`cache/calibration/ili_pseudo_hub_forecasts.parquet`) do not follow the data
the model was given. The h−1 forecast is a forecast of the last observed week,
yet the log-log slope of median on truth is 0.49 at h−1 (0.34 at h3), against
1.04 for the same forecaster on NHSN. The median is several times too high at
the trough and about half of truth at the peak, even at h−1. Indiana on
2018-01-03: the snapshot's last value (2017-12-27 label) is 2.28 and matches
truth, but the h−1 median for that week is 0.05. Re-running
`ili_forecast_one()` with current code reproduces the cached values exactly,
and every run warns *"There is less latency at bake time than there was at
prep time … will discard the most recent data"*. The suspect is epipredict's
latency adjustment (`adjust_latency = "extend_lags"`, the default in
`default_args_list()`) interacting with the one-week snapshot truncation in
`ili_forecast_one()`; not yet confirmed. The NHSN replay
(`calibration_ws_backfill.R`) is unaffected.

Invalidated: the ILI+ burn-in result above; setup C and every ILI+ number in
"One forecaster across eras" below; the "bias flips sign with the data
source" reading. The ILI+ question (does the warm start transfer to a
different data source?) is open again until the replay is fixed and rebuilt.

### Current numbers (2026-09-23)

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

**Model metadata:** `CMU-TimeSeries-Calibrated.yml` created in both `../FluSight-forecast-hub/model-metadata/` and `../covid19-forecast-hub/model-metadata/` (`designated_model: false`).

**Bug fixed en route:** `nssp_archive` in both `flu_data_targets.R` and `covid_data_targets.R` had a duplicate-key error (pre-existing since August 2026): `compactify = TRUE` can retain multiple rows per `(geo_value, time_value)` when the value changed across issues, and the subsequent `mutate(version = time_value + 7)` flattened all to the same version, producing duplicate keys in `as_epi_archive()`.
Fix: `group_by(geo_value, time_value) %>% slice_max(version, n = 1L, with_ties = FALSE) %>% ungroup()` before the version clobber.

# Roadmap

Possible next steps, roughly ordered:

0. **Fix and rebuild the ILI+ replay** (`calibration_ili_backfill.R`, ~30 min):
   confirm the latency-adjustment diagnosis, fix, check the h−1 log-log slope
   is near 1, then re-run the ILI+ question (bias sign on ILI+; setup C;
   whether the warm start transfers across data sources) and restore section
   1.2 of the findings notebook. Also switch the six older calibration
   notebooks from the removed `hub_read_truth()` / `hub_read_covid_truth()` to
   `nhsn_read_truth()`.
1. **Review the ranked gallery** — does the WIS cost concentrate at turning
   points (the `h + 2` staleness prediction)? Are the biggest offsets fixing
   real miscalibration or chasing data-revision artifacts?
2. **Send the collaborator email** (sweep findings; h −1 free win; carry vs
   reset a wash).
3. **`windowed_seasonal_extra_sources` retrospective** via
   `scripts/calibration/calibration_harness.R` (covid, evaluation store) — does
   calibration help our best component forecaster, not just the submitted
   ensemble?
4. **Integration design** for prod: where the tracker lives (per-forecaster
   vs post-ensemble), how state persists week to week, and how to handle the
   warm-up (burn-in on a past season? carry state across seasons? a
   burn-in-free learning-rate schedule?).
5. **Method extensions**, in the order argued under "Directions this points
   to": slow relative level term + fast tracker; cross-horizon proxy
   gradients for staleness; data-driven phase gating; scale proxy. Pooling
   offsets across locations and per-level learning rates are lower priority.
6. **Agentic triage** of gallery panels against a fixed rubric (data revision
   visible? base missed the turn? calibration helped/hurt?) if manual
   skimming of the ranked gallery proves insufficient.
7. **Dynamic gallery app** (Shiny/Posit or a served page with dynamic data
   fetch) if the static top-N format becomes limiting.
