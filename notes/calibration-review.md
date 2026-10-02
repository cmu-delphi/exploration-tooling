# Calibration review

Tracks which experiment notebooks Dmitry has read and verified, his
comments, and what was done about them. Notebooks render to
`reports/calibration_experiments/`; sources are in
`reports/writeups/calibration_experiments/`.

Status: **verified** (read and agreed), **commented** (read, changes asked
for), **not reviewed**.

## How the experiments build on each other

1. **E00: learn from data as published.** Finalized data flatters the h−1
   and h0 gains, though not by much. Every experiment from E00 on learns
   from each week's published data (the "exact" design), with no further
   work needed.
2. **E07: one fixed forecaster is enough.** On identical rows, calibration
   gains are similar on the submitted ensemble and on `windowed_seasonal`,
   and the trackers rank the same way. So experiments can focus on the
   clean `windowed_seasonal` replay, which is free of hand edits and
   spoiled submissions. Caveat: the tracker ranking transfers, but a
   date-based switch-off (`off_after`) does not; it depends on each
   forecaster's spring bias (last on the ensemble in 2024-25, second on
   `windowed_seasonal`).
3. Experiments before E07's re-run (E03–E05, E09–E11) used the submitted
   ensemble; E01, E02 (new versions) and E12–E17 use the clean replay.

## Month view (added 2026-10-01)

Every notebook now has a "Month view: WIS, coverage, median error" section
(WIS reduction %, within-month L1 coverage bias, median absolute-error
reduction %, per month and horizon, states only) and a month-average
coverage column next to its season-pooled one. Season-pooled coverage
claims mostly don't survive: where one changed, the notebook's Findings end
with a "**Month view (2026-10-01):**" line. Verdicts:

| notebook | coverage claim | verdict |
|---|---|---|
| E00 | exact ≈ final; vintage +0 weeks worse at h−1 | holds |
| E01 | 0.03 is the coverage elbow; carry beats reset | reversed: 0.01 lowest month by month; carry ≈ reset |
| E02 | switching off costs coverage | weakened: mostly an artifact; better at h3 |
| E02 | per-level eta, seasonal window coverage | reversed (small differences) |
| E03 | sqrt ≈ count on coverage | holds; sqrt better at h2–h3 |
| E04 | the leak nearly triples coverage bias | reversed: no difference month by month |
| E05 | warm + single best calibrated; warm start cuts bias a third | reversed / weakened: REF-op lowest; warm start −5 to −15% |
| E07 | coverage ranks the same on A and B | reversed: rankings differ by month |
| E09 | knee beats REF-paper on coverage | reversed: REF-op lowest; knee worse than base at h1–h3 |
| E10 | covid base already calibrated at h1–h3 | weakened |
| E11 | constant rate better coverage everywhere | reversed: worst of five, worse than base at h1–h3 |
| E14 | coverage keeps improving with lr | reversed: minimum near 0.01–0.032; ≥0.056 worse than base at h1–h3 |
| E15 | REF-op width changes buy coverage | holds, smaller |
| E16 | sqrt 0.018 ≈ rate 0.032 on coverage | reversed: sqrt better; rate worse than base in 2025-26 |
| E17 | decay / off-after give back coverage | weakened: small effect month by month |
| E18 | warm start, leak, constant rate move coverage | reversed: no step moves month-avg coverage by more than 0.008 |

Differences under about 0.01 are within the month-level noise floor.

## Notebooks

| exp | forecaster | status | comments | done |
|---|---|---|---|---|
| E00 vintage backtest | ensemble | verified | Finalized data exaggerates h−1/h0 gains a little; use vintages, no more work. | — |
| E01 eta settings | clean replay | verified | Recap of the August sweep: carry slightly better, window 20 as good as larger, the multiplier has an elbow. Note: on this replay 0.03 is the coverage elbow and 0.01 the WIS-safe choice. Earlier: (1) Curve axis looked flipped vs E09. (2) x/o markers unlabeled. (3) "Rebuilds the original" is misleading: it re-asks the question on a new setup. | (1) E09 flipped to match; (2) legend added; (3) intro reworded with a what-changed table |
| E02 eta variants | clean replay | commented | Headline: switching off in Feb/Mar recovers WIS, loses coverage. (1) Coverage numbers hard to find in the WIS / bias cells. (2) Month plot: unclear whether positive % is good; the sign convention must be labeled explicitly in every notebook. | intro reworded; (1) tables split into WIS and coverage, plus a WIS vs coverage scatter; (2) every notebook now labels "WIS reduction % (positive = calibrated WIS lower than base)" and "L1 coverage bias (lower is better)" (all computations already used that sign). Open: season-level coverage lets ramp and post-peak errors cancel, so the switch-off's coverage cost may be mostly an artifact; month-level coverage would settle it. |
| E03 scale | ensemble | not reviewed | | |
| E04 offset structure | ensemble | not reviewed | | |
| E05 warm start | ensemble | not reviewed | | |
| E07 across eras | both | commented | Needs concrete base WIS numbers, not "similar". Expand into the ensemble vs `windowed_seasonal` comparison: base WIS side by side, base bias by month, every operating-point candidate on both. | intro reworded; expanded with base WIS side by side (B/A 0.93–1.01 in 2024-25, 1.05–1.33 in 2025-26), base bias by month, and five candidates on both forecasters |
| E09 lr × delay | ensemble | commented | WIS axis inverted relative to the other notebooks. | flipped to "WIS change %, positive is better" |
| E10 covid | ensemble | not reviewed | | |
| E11 constant lr | ensemble | not reviewed | | |
| E14 lr by season | clean replay | not reviewed | | |
| E15 WIS sources | clean replay | not reviewed | | |
| E16 sqrt constant lr | clean replay | not reviewed | | |
| E17 late decay | clean replay | not reviewed | | |
| E18 REF-op bridge | clean replay | not reviewed | | |

E12 and E13 have no notebooks; they live in `notes/CALIBRATION.md` and
`scripts/calibration/calibration_ws_replay.R`. E06 and E08 were superseded
(by E13 and E10) and not rebuilt.

The summary for sharing is `notes/calibration-summary-2026-10-01.md`.
