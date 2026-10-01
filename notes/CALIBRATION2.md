Open threads and TODOs

Main priority (calibration)
1. Warm start on the constant-rate tracker. E11 (constant 0.1 per 100k, rate scale, +1 wk, re-run) beats the cold adaptive trackers on coverage and is the most stable across seasons, but `warm + sqrt, adaptive` still wins at h0–h1. Try E11's constant rate with `slow_init = "burn_in_quantile"`.
 
Secondary (tech debt and other methods)
2. Reduced-set explore run: the most promising families, not the full sweep.
3. Revision-method backtests: `revision_aware` against the `revision_ratio` baseline at h−1 (explore h−1 families plus `nowcast_notebook`, both diseases), then choose prod's h−1 component. Covid NSSP families need re-running, since covid explore NSSP now has real vintages.

Low priority
4. Calibrate h0 and above only. Revisit h−1 calibration after 3 settles; the harness h−1 history predates the 2026-09-23 switch to `revision_aware`.
5. Sweep windowed_seasonal, the harness proxy, for spoiled submissions; needs a variant of the sweep script that reads the harness forecasts (`notes/spoiled-submissions.md`).
6. Re-run E07, E01 and E02 on vintages (still marked stale).
7. Covid per-horizon calibration (h0 only): untested.
8. Speed up exact tracker runs, if more exact sweeps are coming. An exact config replays prod's weekly re-run, about 55 full tracker runs, so cost grows with the square of the number of rounds (E09: about 72 configs, about 1.5 h on 14 cores).
    - Quick wins, all exact, about 2–2.5x: sort-based "data as of round t" lookup instead of a grouped slice_max (about 4–5 s per round); validate `arg_match` once outside the loop (about 15%); skip isoreg when quantiles are already ordered (about 8%); plain vectors instead of tibble/glue in the per-series loop (about 15%). Check bit-for-bit against cached runs.
    - Checkpointing, another 5–10x: start round t's run from round t−1's tracker state at the first round whose data differ. Needs `qt_track()` to save and resume offsets, the learning-rate window and the slow term; test for exact equality with the full re-run.

Housekeeping
9. renv warns on every run (library renv 1.1.6, lockfile 1.2.3; some lockfile packages not installed). Harmless so far.
