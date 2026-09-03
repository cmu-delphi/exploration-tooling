# TimesFM backtest

Zero-shot backtest of Google's TimesFM 3.0 (`google/timesfm-3.0-pytorch`,
weights under a non-commercial license: research only, not for submissions)
on the flu prod data, compared against the R forecasters.

Three steps, all from the repo root. The R steps need `Rscript` on PATH; on a
machine where R lives in a container, run make inside it, e.g.
`distrobox enter rocker -- make tsfm-dump`.

1. `make tsfm-dump` — R reads the `flu_hosp_prod` targets store and writes
   `cache/tsfm/*.parquet`: `archive` (versioned nhsn/nssp/ILI+/flusurv rows,
   Wednesday-labeled `time_value`), `truth` (finalized, Saturday
   `target_end_date`), `forecast_schedule` (forecast vs generation date),
   `r_forecasts` (the real-time prod forecasts and ensembles) and
   `population`.
2. `make tsfm-forecast args="--mode geo_multivariate"` — Python replays the
   schedule: for each forecast date it takes the as-of snapshot of the archive
   at the generation date (latest version published on or before it, plus the
   manual `flu_data_substitutions.csv` corrections), builds weekly contexts,
   and asks TimesFM for the horizon covering aheads -1..3. Modes:
   `univariate` (one series per geo and source), `geo_multivariate` (nhsn and
   nssp of a geo forecast jointly), `panel` (all geos and sources in one
   multivariate input). The 9 native quantiles (0.1..0.9) are widened to the
   23 hub quantiles by interpolation plus normal-tail extrapolation. Output:
   `cache/tsfm/timesfm_forecasts.parquet`, same schema as `r_forecasts`.
3. `make tsfm-report target=nhsn` — renders
   `scripts/reports/timesfm_comparison.Rmd` to `reports/`, scoring everything
   on the common tasks with WIS on all 23 quantiles and on the 9 shared ones.

Ahead -1 is a nowcast of the already-reported week; TimesFM emits the
observed as-of value there, so compare on aheads >= 0.

## First results (2026-09-03, forecast dates 2025-10-01..2026-07-08)

Zero-shot, context 200 weeks, common tasks, aheads 0..3, states only. WIS on
the 9 native quantiles (0.1..0.9), relative to `windowed_seasonal`:

| forecaster                       | nhsn rel. WIS | nssp rel. WIS |
|----------------------------------|---------------|---------------|
| timesfm3_geo_multivariate        | 0.89          | 0.49          |
| timesfm3_univariate              | 0.99          | 0.48          |
| windowed_seasonal                | 1.00          | 1.00          |
| ensemble_mix (submission)        | 1.21          | 0.87          |

The nhsn gain grows with horizon (about 0.9 at ahead 0, 0.87 at ahead 3) and
comes with slightly narrow intervals (90% coverage 0.82..0.90). Caveats: the
fall 2025 reporting outage means October forecasts run off mid-September data
for every model; the R forecasts are the real-time prod runs while TimesFM is
an archive replay; TimesFM's outer quantiles are extrapolated, not predicted.

Ideas not yet tried: `--mode panel`, longer or shorter `--context-weeks`,
feeding ILI+ history as a covariate, fine-tuning (see the upstream
`timesfm-forecasting/examples/finetuning`), and the 2024/25 season.
