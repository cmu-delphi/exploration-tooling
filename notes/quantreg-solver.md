# Quantile regression solver (`br` vs `fn`)

The prod forecasters fit `epipredict::quantile_reg()`, which calls
`quantreg::rq()`. `g_quantreg` in each `pipelines/*_hosp_prod.R` sets
`method = "fn"` (Frisch-Newton interior point). quantreg's default is `"br"`
(Barrodale-Roberts simplex).

## Why not `br`

`br` can fail to terminate. On 2026-10-28 two covid targets stayed `dispatched`
for 30+ minutes (a normal run takes about 15 minutes):
`forecast_nhsn_windowed_seasonal_extra_sources` and
`forecast_nhsn_seasonal_nssp_latest`, both at `aheads = -1` (-7 days).
The worker sat at one core with constant memory, and an `Rprof` of the run put
97% of samples in `.Fortran` under `rq.fit.br`. The same inputs finish in about
2 seconds with `method = "fn"`.

Removing any single geo from the input (including on-time ones) also made `br`
finish, so the stale NSSP geos (`de`, `ga`, `ia`) were not the trigger.

## The trigger: the outcome equals a predictor

For the nowcast ahead (-7 days) the recipe's outcome column (`ahead_7_value`)
is identical to a predictor (`lag_7_value`) in every row, so the fit is exact
(OLS residual sd ~5e-17) and every quantile is the same value (zero-width
forecasts, under both solvers and for every geo). The predictors themselves are
full rank (5 of 5), so there is no "singular" error at -7.

The hanging call, captured from `rq.fit.br`, has a 1800 x 5 design (the
seasonal training window) and `tau = 0.3`. Replayed alone:

- `quantreg::rq.fit.br(x, y, tau = 0.3)` with the exact `y` does not finish.
- The same call with 1e-6 noise added to `y` finishes in 0.6 s.

An exact fit makes the simplex problem massively degenerate. We have not
looked inside the Fortran code, so "cycling" is the likely reading, not a
confirmed one. Whether `br` stalls depends on the data: the same exact-fit
structure finished for other forecasters, and for the same data with a geo
removed.

## Behaviour that differs between solvers

Calling `scaled_pop_seasonal(ahead = -1)` directly (one day, without the
pipeline's x7 `ahead_multiplier`) on the same data:

| solver | result |
| --- | --- |
| `br` | error `Singular design matrix` |
| `fn` | no error; warning `Error info = 1 in stepy: possibly singular design`, zero-width quantiles |

So `fn` turns a loud failure into a quiet warning. `targets` records warnings in
`tar_meta(fields = warnings)`, but nothing in the pipeline flags them.

## `br` vs `fn` outputs

Running `covid_hosp_prod` for 2026-10-28 into a copy of the `br` store and
comparing the 48 branches that finished under both:

- Every forecaster that uses `g_quantreg` agrees to within about 3e-5 of each
  geo's forecast range (largest case, `revision_aware`); most differences are
  below 1e-8.
- Median ratio `fn/br` is 1.00 at the 1%, 50% and 99% quantiles for every
  changed forecaster.
- This is one forecast date, not a backtest.

`notes/revision_aware_forecaster_devlog.md` records a separate problem: `fn`
extrapolated badly when features were pop-scaled to very small magnitudes.
The prod grids here use `pop_scaling = FALSE` for the `g_quantreg` forecasters.

## Hang protection

`forecast_resources()` (`R/targets/shared_utils.R`) sets a 30 minute per-target
timeout on `forecast_nhsn` and `forecast_nssp`. When it fired in the local run
the pipeline stopped and the stuck workers exited, but `tar_make()` reported
the cryptic `invalid subscript type 'closure'` instead of naming the target.
