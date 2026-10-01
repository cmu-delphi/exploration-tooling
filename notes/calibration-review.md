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
   spoiled submissions.
3. Experiments before E07's re-run (E03–E05, E09–E11) used the submitted
   ensemble; E01, E02 (new versions) and E12–E17 use the clean replay.

## Notebooks

| exp | forecaster | status | comments | done |
|---|---|---|---|---|
| E00 vintage backtest | ensemble | verified | Finalized data exaggerates h−1/h0 gains a little; use vintages, no more work. | — |
| E01 eta settings | clean replay | commented | (1) Curve axis looked flipped vs E09. (2) x/o markers unlabeled. (3) "Rebuilds the original" is misleading: it re-asks the question on a new setup. | (1) E09 flipped to match; (2) legend added; (3) intro reworded with a what-changed table |
| E02 eta variants | clean replay | not reviewed | | intro reworded (same issue as E01) |
| E03 scale | ensemble | not reviewed | | |
| E04 offset structure | ensemble | not reviewed | | |
| E05 warm start | ensemble | not reviewed | | |
| E07 across eras | both | commented | Needs concrete base WIS numbers, not "similar". Expand into the ensemble vs `windowed_seasonal` comparison: base WIS side by side, base bias by month, every operating-point candidate on both. | intro reworded; expansion in progress |
| E09 lr × delay | ensemble | commented | WIS axis inverted relative to the other notebooks. | flipped to "WIS change %, positive is better" |
| E10 covid | ensemble | not reviewed | | |
| E11 constant lr | ensemble | not reviewed | | |
| E14 lr by season | clean replay | not reviewed | | |
| E15 WIS sources | clean replay | not reviewed | | |
| E16 sqrt constant lr | clean replay | not reviewed | | |
| E17 late decay | clean replay | not reviewed | | |

E12 and E13 have no notebooks; they live in `notes/CALIBRATION.md` and
`scripts/calibration/calibration_ws_replay.R`. E06 and E08 were superseded
(by E13 and E10) and not rebuilt.

The summary for sharing is `notes/calibration-summary-2026-10-01.md`.
