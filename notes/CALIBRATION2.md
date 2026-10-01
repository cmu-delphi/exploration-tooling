Open threads and TODOs

Main priority (calibration)
1. Run all E00–E10 notebooks.
2. Pick a constant learning rate from E09: the bottom-left of the elbow. Add that setting to a notebook that compares it with the adaptive learning rate. Use the "rerun" method rather than "stateful".
 
Secondary (tech debt and other methods)
3. Reduced-set explore run: the most promising families, not the full sweep.
4. Revision-method backtests: `revision_aware` against the `revision_ratio` baseline at h−1 (explore h−1 families plus `nowcast_notebook`, both diseases), then choose prod's h−1 component. Covid NSSP families need re-running, since covid explore NSSP now has real vintages.

Low priority
5. Calibrate h0 and above only. Revisit h−1 calibration after 4 settles; the harness h−1 history predates the 2026-09-23 switch to `revision_aware`.
6. Sweep windowed_seasonal, the harness proxy, for spoiled submissions; needs a variant of the sweep script that reads the harness forecasts (`notes/spoiled-submissions.md`).
7. Re-run E07, E01 and E02 on vintages (still marked stale).
8. Covid per-horizon calibration (h0 only): untested.
9. Speed up exact tracker runs, if more exact sweeps are coming. An exact config replays prod's weekly re-run, about 55 full tracker runs, so cost grows with the square of the number of rounds (E09: about 72 configs, about 1.5 h on 14 cores).
    - Quick wins, all exact, about 2–2.5x: sort-based "data as of round t" lookup instead of a grouped slice_max (about 4–5 s per round); validate `arg_match` once outside the loop (about 15%); skip isoreg when quantiles are already ordered (about 8%); plain vectors instead of tibble/glue in the per-series loop (about 15%). Check bit-for-bit against cached runs.
    - Checkpointing, another 5–10x: start round t's run from round t−1's tracker state at the first round whose data differ. Needs `qt_track()` to save and resume offsets, the learning-rate window and the slow term; test for exact equality with the full re-run.

Housekeeping
10. Delete the moved-aside caches (`cache/calibration/experiments/*.pre-spoiled`) once the new renders check out.
11. `just calibration-experiments` needs `RSCRIPT="distrobox enter rocker -- Rscript"` on the host; make it the Justfile default?
12. renv warns on every run (library renv 1.1.6, lockfile 1.2.3; some lockfile packages not installed). Harmless so far.
13. `ds/calibrate2` mixes calibration, h−1, explore and prod-health work; consider splitting it into separate PRs.
