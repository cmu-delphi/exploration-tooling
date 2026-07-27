# BLAS oversubscription timing experiment (2026-07-19)

Question from the CLAUDE.md roadmap: does crew-worker-count x BLAS-thread-count
oversubscription slow down the pipeline, and would pinning BLAS to 1 thread per
worker help? Measurement only, no code changes.

## Setup

- Container: `distrobox enter rocker` (rocker/tidyverse:latest), R 4.5.3.
- BLAS: OpenBLAS pthread, `/usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so`
  (both BLAS and LAPACK). `RhpcBLASctl` is not installed in this image, so
  thread counts were checked via env vars rather than `blas_get_num_procs()`.
- Cores: `nproc` = 16 inside the container.
- Crew workers: `set_targets_config()` (`R/targets/shared_utils.R:220-236`) sets
  `num_workers = min(max(detectCores() - 4L, 1L), 30L)` -> **12 local workers**
  for this machine (16 - 4 = 12, under the 30 cap).
- Env var propagation check (validity gate for the whole experiment): pushed a
  throwaway task to a 2-worker `crew_controller_local()` that reports
  `Sys.getenv("OPENBLAS_NUM_THREADS")`. Result: **"1" when the parent process
  had `OPENBLAS_NUM_THREADS=1` set, "UNSET" otherwise** — env vars propagate
  cleanly from the shell into crew workers (they're forked/spawned R processes
  inheriting the parent env, no callr/mirai env-passing needed). So the pinned
  runs below are a valid test of the hypothesis, not a no-op.
- Command: `env TAR_RUN_PROJECT=flu_hosp_evaluation EVALUATION_N_DATES=1 FORECAST_REFERENCE_DATE=2026-01-07 Rscript scripts/run.R`
  wrapped in a throwaway driver script that, per run: invalidates the 16
  `forecast_{nhsn,nssp}_*_2026.01.07` targets in the `flu_hosp_evaluation`
  store only, times `tar_make()`, and pulls per-target `seconds` from
  `tar_meta()`. No other store touched; no pipeline source files edited.
- Configs compared: baseline (ambient BLAS threading, i.e. unset ->
  OpenBLAS's own default, effectively per-call thread choice up to core count)
  vs. pinned (`OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1`
  exported to the whole run, so all 12 workers inherit it). 3 replications
  each.

## Results

### Wall time (full `tar_make()`, includes 16 forecast branches + submission/scoring targets)

| Config   | Rep 1  | Rep 2  | Rep 3  | Mean   | Stdev |
|----------|--------|--------|--------|--------|-------|
| Baseline | 64.02s | 62.84s | 66.63s | 64.50s | 1.94s |
| Pinned   | 73.35s | 64.67s | 62.56s | 66.86s | 5.72s |

Pinned rep 1 rebuilt 185 targets vs. 173 in every other run (an unrelated
upstream target came due), inflating that one wall time; excluding it, pinned
reps 2-3 average 63.6s, in line with baseline.

### Sum of per-target `seconds` across the 16 forecast targets (CPU-bound work only, excludes scheduling/IO)

| Config   | Rep 1   | Rep 2   | Rep 3   | Mean    | Stdev |
|----------|---------|---------|---------|---------|-------|
| Baseline | 118.86s | 117.80s | 119.04s | 118.57s | 0.67s |
| Pinned   | 110.09s | 111.89s | 108.81s | 110.26s | 1.55s |

Pinned is ~7% lower in summed target-seconds, consistently across all 3
reps (non-overlapping ranges), but this doesn't translate into a wall-time
win — the 16 forecast targets run 12-wide on 12 workers, so wall time is
dominated by the single slowest target, not the sum.

### Slowest targets (baseline rep 2 vs. pinned rep 2, representative)

| Target                                                    | Baseline (s) | Pinned (s) |
|------------------------------------------------------------|-------------:|-----------:|
| forecast_nhsn_windowed_seasonal_2026.01.07                 | 25.7         | 20.7       |
| forecast_nssp_windowed_seasonal_extra_sources_2026.01.07   | 11.5         | 11.2       |
| forecast_nhsn_windowed_seasonal_extra_sources_2026.01.07   | 11.4         | 11.4       |
| forecast_nhsn_seasonal_nssp_cheating_2026.01.07            | 11.4         | 11.9       |
| forecast_nssp_windowed_seasonal_2026.01.07                 | 10.3         | 10.2       |
| forecast_nssp_seasonal_nssp_cheating_2026.01.07            | 10.2         | 10.7       |

The single slowest target (`forecast_nhsn_windowed_seasonal`, the long pole in
every rep) shows the largest and most consistent pinned improvement (26-28s ->
20-22s across reps), but the next 5 targets are within noise of each other
either direction. Since wall time on a 12-worker/16-target run is set by
roughly the 2 longest-running targets, the windowed_seasonal improvement is
the one piece of the summed-seconds gain that could plausibly show up in wall
time — and rep 3 (62.56s) does land at the low end of the baseline
distribution, but the sample is too small and noisy to call this more than
suggestive.

## Conclusion

- Env var propagation to crew workers is confirmed working — pinning via
  `OPENBLAS_NUM_THREADS`/`OMP_NUM_THREADS`/`MKL_NUM_THREADS` in the parent
  shell is a valid mechanism here, not a dead end.
- Evidence for oversubscription is weak-to-mild, not the clear win the roadmap
  hypothesis anticipated: summed per-target CPU-seconds drops ~7% when pinned
  (consistent across reps), but overall wall time does not move outside noise
  (pinned mean is actually slightly higher, driven by one rep that rebuilt
  extra unrelated targets; excluding that outlier, pinned and baseline wall
  times are indistinguishable at this sample size).
- The one target that stands out is `forecast_nhsn_windowed_seasonal`, the
  long pole of the batch, which improved consistently under pinning (~25% less
  time). That's the most credible signal in this dataset that BLAS
  oversubscription costs something, but it's one target across 3 reps each —
  not enough to generalize.
- Caveat (per task instructions): `flu_hosp_evaluation` at
  `EVALUATION_N_DATES=1` runs only 16 forecast targets on 12 workers (worker
  count roughly matches target count), so most targets barely overlap in
  time — this is a much smaller contention scenario than a real prod run
  (more forecast/ahead/geo combinations, more simultaneous workers all doing
  BLAS calls at once). A prod-scale or `EVALUATION_N_DATES>1` rerun of this
  same protocol would be a better test of the oversubscription hypothesis;
  this run neither confirms nor rules it out at prod scale.
