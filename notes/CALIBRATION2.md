Open threads and TODOs

Decided but not done
4. Sweep windowed_seasonal, the main ensemble component used as a proxy in the calibration harness. It needs a variant of the sweep script that reads the harness forecasts. Recorded in notes/spoiled-submissions.md.
5. h−1 is its own track, separate from calibration. Since 2026-09-23 prod no longer uses the AR models at h−1: `ensemble_mix` drops them at negative aheads and h−1 is about 99.9% `revision_aware`, which predicts the finalized value from the week's first report (checked on the 2026-04-04 round). Next: score `revision_aware` against the `revision_ratio_nowcast` baseline at h−1 through explore (explore's `g_aheads` has no −7 yet). On 2026-04-04 the baseline gave OH 64 / CO 34 / MN 34 (finalized ≈60/34/36) vs `revision_aware` 84/41/43; its lag-7 term drags toward the previous week in sharp declines.
   Calibration focuses on h0 and above. Harness h−1 history predates 2026-09-23 (AR with the reported week dropped for flu, `climate_linear` for covid), so h−1 offsets learned from it don't apply to the current h−1. Revisit h−1 calibration only after the h−1 component is settled.

Open questions from earlier work
6. Halved US cause (2024-12-14, 2024-12-21, 2025-01-04): unconfirmed. The us/usa mixup is my best guess.
7. Covid 2026-04-25 cause (a stale fetch during the cast API v2→v5 change): likely but unconfirmed; there's no store to replay.
8. Experiments not yet re-run on vintages: E07 (ensemble vs windowed_seasonal), and E01/E02, still marked stale.
9. Covid per-horizon calibration (h0 only; h−1 deferred, see 5): untested.
10. Replay drift: the July 2026 flu replay doesn't reproduce the 2025-11-22 submission, and the May 2025 covid replay doesn't reproduce 2024-11-23. That's expected, since code and data changed, but don't read replays as "what was submitted".

Housekeeping
11. Old run caches: E09 and the knitr caches were moved aside (cache/calibration/experiments/*.pre-spoiled). I can delete them once the new renders check out.
12. just calibration-experiments doesn't work on the host, because Rscript isn't installed there. It needs RSCRIPT="distrobox enter rocker -- Rscript". Should I make that the Justfile default?
13. renv warns on every run: the library has renv 1.1.6 but the lockfile wants 1.2.3, and some lockfile packages aren't installed. It's harmless so far.
14. No bookmark yet: the branch is about a dozen jj commits on top of ds/calibrate2. Tell me when you want one moved for a PR.

---

The faster snapshot gives identical output and is about 25x faster: 0.15–0.3 s instead of 3.6–4.9 s.

Why E09 is slow

- Each "exact" config is really about 55 tracker runs. Prod re-runs the tracker from the first round each week, so replaying it means one full run per live round, each over a longer history. Total work grows with the square of the number of rounds.
- E09 has about 72 exact configs. That's 60 sweep runs plus 12 reference runs, around 4,000 full tracker runs, at roughly 65 s per config on 14 cores.
- The stateful runs are cheap, since each config is a single run.

Profile of one full run (13.5 s at the last round), plus the snapshot step:

┌──────────────────────────────────────────────────────────┬─────────────────────────────────────┬─────────────────────────────────────────────────────────────────┬──────────────────────────────────────────┐
│                           cost                           │                share                │                               fix                               │                  exact?                  │
├──────────────────────────────────────────────────────────┼─────────────────────────────────────┼─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────┤
│ Rebuilding "data as of round t" with a grouped slice_max │ ~4–5 s per round, on top of the run │ Sort-based lookup (tested above)                                │ Yes, identical output                    │
├──────────────────────────────────────────────────────────┼─────────────────────────────────────┼─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────┤
│ rlang::arg_match called on every tracker step            │ ~15% of the run                     │ Validate once, outside the loop                                 │ Yes                                      │
├──────────────────────────────────────────────────────────┼─────────────────────────────────────┼─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────┤
│ Isotonic projection (isoreg) on every step               │ ~8%                                 │ Skip it when quantiles are already ordered, which is most steps │ Yes, PAVA leaves ordered input unchanged │
├──────────────────────────────────────────────────────────┼─────────────────────────────────────┼─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────┤
│ tibble/glue building inside the per-series loop          │ ~15%                                │ Plain vectors in the hot path                                   │ Yes                                      │
└──────────────────────────────────────────────────────────┴─────────────────────────────────────┴─────────────────────────────────────────────────────────────────┴──────────────────────────────────────────┘

Quick wins. Together those should make exact runs about 2–2.5x faster, so E09 drops from about 1.5 h to about 40 min. They're all exact, and I'd check that by comparing results bit-for-bit against the cached runs. About 30 minutes of work.

Bigger, algorithmic win. Two consecutive weekly re-runs see identical data for everything older than the last few weeks, because revisions settle within a few weeks. So the run for round t could start from a checkpoint of round t−1's run at the first round where their data differ, instead of from the beginning. That turns the quadratic cost into roughly linear: probably another 5–10x, bringing E09 to a few minutes. It needs the tracker to save and resume its state (offsets, learning-rate window, slow term). That's a moderate refactor to qt_track(), and it can be tested for exact equality against the current full re-run. Half a day or so, with tests.

About the running render. It started with the old code and has roughly 1–1.5 hours left. Restarting it after the quick wins would save little once you count the implementation time. I'd let it finish and do the quick wins next, so later sweeps benefit. Then decide on the checkpointing based on how many more exact sweeps you expect. Want me to start on the quick wins now? Editing the code won't affect the running render, which loaded everything at start.
