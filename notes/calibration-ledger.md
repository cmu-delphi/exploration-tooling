# Calibration experiment ledger

Index of every calibration comparison: what was varied, against what, and
whether the result still stands. Findings and numbers live in
`notes/CALIBRATION.md`; this file only tracks the design of each comparison.

Rule: each experiment names a reference config and varies one axis from it.
When an experiment has to move several axes (E09), it includes bridge rows
that step from the reference one axis at a time.

## Axes

| axis | values seen | reference value |
|---|---|---|
| disease | flu, covid | flu |
| base forecaster | submitted ensemble, `windowed_seasonal` | submitted ensemble |
| working scale | count, rate (per 100k), sqrt, log1p, quartic root | sqrt |
| learning truth | final (latest vintage), vintage (value at reveal; a stateful tracker), exact (prod's weekly re-run) | exact (E00 on; E01–E08 used final) |
| learning rate type | adaptive+ (`mult`, `floor`, `lr_window`), constant (0.001–3.2 per 100k, E09) | adaptive+ |
| `lr_mult` | 0.3, 0.1, 0.03, 0.01 | 0.03 |
| `lr_window` | 8, 20, 50, Inf | 20 |
| season policy | carry, reset | carry |
| burn-in | 2023-24 hub, ILI+, none | 2023-24 hub |
| `settle_days` (extra revision delay) | 7, 14, 21, 28 (= 0–3 weeks extra) | 14 |
| offset structure | single term, leak (`fast_decay`), warm start, slow term | see REF-op |
| update gating | always, `off_after` date | always |
| eta variants | pooled, per-level, geo-pooled, seasonal window | pooled |

Reference configs:

- **REF-paper**: flu, ensemble, count, adaptive+ mult 0.03, window 20,
  carry, 2023-24 burn-in, settle 14, single term.
- **REF-op** (current operating point, in prod): REF-paper with sqrt, floor
  1e-3, warm start `burn_in_quantile`, slow mult 0.003, `fast_decay` 0.1.

Both are `cal_ref_args()` in `R/calibration/views.R`; notebooks run them
through `cal_run()`, which also picks the learning truth. Experiment notebooks
live in `reports/writeups/calibration_experiments/` and render to
`reports/calibration_experiments/` (with this ledger as `index.html`) via
`just calibration-experiments [notebook ...]`.

## Views

Standard outputs an experiment can report. The goal is for every live config
to have all of them.

| id | view |
|---|---|
| V-head | WIS % change and L1 coverage bias by horizon (× season) |
| V-month | WIS % change by month of reference date, with base-WIS share |
| V-state | per-state season panels with as-of vintages (a few states) |
| V-gallery | a few ranked forecasts (see the note below) |
| V-ae | absolute error of the median, change vs base |
| V-curve | coverage bias vs WIS curve over a swept parameter, per location + aggregate |

V-gallery selection: ranking best/worst n by a single metric filled the old
gallery with small states at the end of the season. There, a mid-season
upward offset that no longer applies inflates the error on a tiny base. A
gallery should show only a few panels and pick them so that one regime
cannot take over. For example, weight the ranking by the forecast's share of
base WIS, and pick within strata (season phase × location size).

## Experiments

"Code" is pre-fix or post-fix relative to the 2026-09-17 correctness fixes
(sqrt clamp, issue-round gating).

| id | question | varies | reference | disease | code | truth | views | where | status |
|---|---|---|---|---|---|---|---|---|---|
| E00 | how much did final truth flatter results; can vintage stand in for exact? | learning truth {final, vintage, exact} × settle {7, 14} | REF-paper, REF-op | flu | post-fix | all three | V-head | `e00_vintage_backtest.Rmd` | done: exact is the default; vintage is not a stand-in |
| E01 | which eta settings? | season policy × window × mult | REF-paper | flu | pre-fix | final | V-head, V-curve (mult) | `calibration_qt_flu.Rmd` | stale |
| E02 | cutoff and eta variants | `off_after`, per-level, geo-pool, seasonal window (one at a time) | REF-paper | flu | pre-fix | final | V-head, V-month, V-state | `calibration_qt_seasons_flu.Rmd` | stale |
| E03 | which working scale? | count vs sqrt vs rate (log1p, quartic pre-fix only) | REF-paper | flu | post-fix | exact | V-head, V-ae, V-month, V-state, V-gallery | `e03_scale.Rmd` | done |
| E04 | leak / two-term offset | single, leak 0.1, slow term, slow + leak | REF-paper at sqrt | flu | post-fix | exact | V-head, V-ae, V-month, V-state, V-gallery | `e04_offset_structure.Rmd` | done |
| E05 | warm start from burn-in | warm vs cold, paired, for single / leak / slow + leak; warm alone | REF-paper at sqrt | flu | post-fix | exact | V-head, V-ae, V-month, V-state, V-gallery | `e05_warm_start.Rmd` | done |
| E06 | ILI+ as burn-in | burn-in source | REF-op | flu | post-fix | final | V-head | notes only | invalid (broken replay) |
| E07 | one forecaster across eras | base forecaster (A ensemble / B `windowed_seasonal`; C invalid) | REF-paper at sqrt | flu | post-fix | final | V-head | `calibration_ws_experiments.R`, findings notebook | current, needs vintage rerun |
| E08 | covid | disease | old REF-paper (count, window 50) | covid | pre-fix | final | V-head, V-state, V-gallery | `calibration_qt_*_covid.Rmd` | stale, replaced by E10 |
| E09 | constant lr × revision delay | constant lr grid × `settle_days` {7, 14, 21, 28}; bridge rows below | REF-paper | flu | post-fix | exact (+ vintage, for a stateful tracker) | V-curve, V-head | `e09_lr_delay.Rmd` | done |

| E10 | covid at the references | disease (REF-paper, REF-op without warm start as in covid prod) | REF-paper, REF-op | covid | post-fix | exact | V-head, V-ae, V-month, V-state, V-gallery | `e10_covid.Rmd` | done |

E09 bridge rows, each one axis from the previous: REF-paper → rate scale →
constant lr (sweep) → `settle_days` (sweep).

Diagnostics that are analyses rather than configs (all flu, finalized truth):
year-over-year seasonal offset viability, staleness lagged correlation, base
bias on NHSN and ILI+. See `notes/CALIBRATION.md`.

## Gaps

- **Learning truth.** E01, E02 and E06–E08 learned from finalized NHSN (E00
  shows the error is small outside h−1). E03–E05 have been re-run exactly.
- **E07** (ensemble vs `windowed_seasonal`) has not been re-run exactly.
