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
  cross-check (`scripts/calibration_export_series.R`).
- `R/calibration/hub_data.R` — FluSight hub adapters: `hub_read_forecasts()`
  (CMU-TimeSeries submissions from the sparse hub checkout),
  `hub_read_truth()` (hub finalized truth CSV), `hub_label_seasons()`.
- `R/calibration/calibrate.R` — `calibrate_hub_forecasts()` driver (burn-in,
  season policy, ~5 s for all 265 series) and metrics: `hub_coverage()`,
  `hub_coverage_summary()`, `hub_quantile_loss()`, `hub_rolling_tradeoff()`.
- `scripts/calibration_harness.R` — targets-based harness for calibrating our
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
capture if one exists.

## Findings (factorial sweep, `scripts/reports/calibration_qt.Rmd`)

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

**Chosen operating point: `lr_mult = 0.03`, `lr_window = 20`,
`season_policy = "carry"`** (20 over the untested 15 because it is a
validated grid point and the window is immaterial). At this multiplier the
tracker needs most of a season to warm up, which makes burn-in/state-carrying
the central integration design issue.

## Notebooks

- `scripts/reports/calibration_qt.Rmd` — the parameter-sweep EDA. Frozen as
  the sweep record; rendered HTML alongside.
- `scripts/reports/calibration_qt_seasons.Rmd` — whole-season views, condensed
  to CA only (was five states) to understand one series in depth × two live
  seasons, one panel per season: every-other-round fans (80% band) for h 0–3,
  the NHSN vintage each round saw painted over its fortnight, finalized truth,
  and an eta strip; plus collapsible internals tables at h2 (eta, pre-PAVA
  hidden offset, post-PAVA offset, base, calibrated at 5 levels). Repeated for
  six method variants (baseline, off after March 1 / February 15, per-level
  eta, geo-pooled eta, seasonal window) with a headline WIS/calibration-error
  comparison at the top.
- `scripts/reports/calibration_qt_gallery.Rmd` — fixed operating point. Slim
  headline table (WIS + calibration error per horizon per season), then a
  ranked per-forecast gallery: top-N (location, round) panels ordered by mean
  absolute quantile displacement relative to the base median. Each panel:
  as-of NHSN snapshot (data the forecaster saw), finalized truth, base and
  calibrated fans (50% + 90% ribbons, horizons −1…3), captioned with MAD, WIS
  delta, and whether truth escaped the base 90% band. Seasons 2024-25 and
  2025-26 only, flu only. A dynamic/paginated app version was considered and
  rejected for now in favor of static top-N HTML.

## Method variants (`calibrate_hub_forecasts()` options; 2026-08-27)

All at the operating point above; WIS change vs base, both live seasons, by
horizon −1/0/1/2/3:

| variant | option | h−1 | h0 | h1 | h2 | h3 |
|---|---|---|---|---|---|---|
| baseline | — | +4.3 | +1.8 | −1.9 | −5.3 | −6.7 |
| off after April 1 | `off_after = "04-01"` | +4.9 | +1.4 | −1.3 | −2.8 | −3.9 |
| off after March 1 | `off_after = "03-01"` | +4.5 | +2.0 | +0.4 | 0.0 | +0.2 |
| off after February 15 | `off_after = "02-15"` | +3.0 | +2.5 | +1.3 | +0.8 | +0.9 |
| per-level eta | `lr_args$per_level = TRUE` | +4.2 | +1.8 | −1.3 | −3.4 | −5.4 |
| geo-pooled eta | `lr_geo_pool = <pop>` | +4.4 | +2.0 | −2.2 | −5.5 | −7.7 |
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

# Roadmap

Possible next steps, roughly ordered:

1. **Review the ranked gallery** — does the WIS cost concentrate at turning
   points (the `h + 2` staleness prediction)? Are the biggest offsets fixing
   real miscalibration or chasing data-revision artifacts?
2. **Send the collaborator email** (sweep findings; h −1 free win; carry vs
   reset a wash).
3. **`windowed_seasonal_extra_sources` retrospective** via
   `scripts/calibration_harness.R` (covid, evaluation store) — does
   calibration help our best component forecaster, not just the submitted
   ensemble?
4. **Integration design** for prod: where the tracker lives (per-forecaster
   vs post-ensemble), how state persists week to week, and how to handle the
   warm-up (burn-in on a past season? carry state across seasons? a
   burn-in-free learning-rate schedule?).
5. **Method extensions**, unordered: scale-aware or multiplicative offsets
   (the within-season residual-scale swing); pooling offsets across
   locations; per-level learning rates; explicit handling of turning-point
   staleness.
6. **Agentic triage** of gallery panels against a fixed rubric (data revision
   visible? base missed the turn? calibration helped/hurt?) if manual
   skimming of the ranked gallery proves insufficient.
7. **Dynamic gallery app** (Shiny/Posit or a served page with dynamic data
   fetch) if the static top-N format becomes limiting.
