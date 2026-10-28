# Debugging a forecaster: look at the design matrix

When one forecaster misbehaves (hangs, errors, zero-width or wild quantiles),
go straight to the data the model is fit on. Most problems we have hit show up
there: an outcome column equal to a predictor, a constant or all-NA column,
rank deficiency, tiny magnitudes after population scaling, or far fewer rows
than expected. `notes/quantreg-solver.md` is a worked example.

The approach: replay one branch outside `targets`, and use `trace()` to save
the arguments of a fit function. That needs no edits to the forecaster code.

## 1. Replay one branch

Each forecast branch is one forecaster x forecast date x ahead
(`tar_map` over the grid and schedule, `pattern = map(aheads)`).

- Inputs: `targets::tar_read_raw("full_data_<id>_<date>", store = "<project>")`,
  and for extra sources `nssp_exogenous_data_<id>_<date>` (names as in
  `tar_manifest()` / the runner log).
- The call: `tar_manifest(fields = command, names = starts_with("forecast_nhsn_<id>"), script = ...)`
  shows the `run_forecaster(...)` command with the grid's params substituted.
  Remember the pipeline multiplies `aheads` by `ahead_multiplier` (7 for the
  weekly targets).
- Run it inside `callr::r(..., timeout = 60)`. A solver stuck in Fortran
  ignores interrupts, so a separate process with a timeout is the only
  reliable way to stop it.

For a branch that errored, `tar_option_set(workspace_on_error = TRUE)` plus
`tar_workspace(<branch>)` restores its exact inputs instead.

`_local/stuck/repro.R` and `_local/stuck/capture_rq.R` (gitignored) are the
scripts from the quantreg hang, if present.

## 2. Hook a fit function

```r
trace("<fn>", quote(saveRDS(as.list(environment()), "<path>.rds")),
      where = <namespace or globalenv()>, print = FALSE)
```

The tracer runs at the start of every call, so the saved file holds the last
call's arguments. To keep every call, append to a list with `<<-` instead.

| Hook | `where` | What you get | Covers |
| --- | --- | --- | --- |
| `run_workflow_and_format` | `globalenv()` (after `source("R/load_all.R")`) | `preproc`, `train_data`; `bake(prep(preproc, train_data), train_data)` gives the named design matrix including the outcome | smoothed_scaled, scaled_pop, scaled_pop_seasonal, flusion, no_recent_outcome |
| `fit_xy.model_spec` / `fit.model_spec` | `asNamespace("parsnip")` | predictors and outcome as parsnip receives them (`fit.model_spec` is the formula path used by revision_aware) | any parsnip trainer |
| `rq.fit` | `asNamespace("quantreg")` | the exact numeric `x`, `y`, `tau`, `method` the solver gets, one call per quantile level | every `quantile_reg` forecaster |
| `lm.fit` | `asNamespace("stats")` | `x`, `y` for `lm()` fits | baseline_linear (per geo) |

Start at the engine (`rq.fit`) when the symptom is numeric (hang, singular
design, odd quantiles). Start at `run_workflow_and_format` when you need to
know which recipe step produced a column.

## 3. What to check

- `dim(x)`, rows per geo, and the training window's date range.
- `qr(x)$rank` vs `ncol(x)`, and constant or all-NA columns.
- Any predictor identical to the outcome. An exact fit makes `br` stall and
  gives zero-width quantiles.
- Column scales: population-scaled features can be ~1e-6 and break `fn`
  (`notes/revision_aware_forecaster_devlog.md`).
- Rows dropped by NA handling (compare to `nrow(train_data)`).
- Replay the solver alone on the saved `x`, `y` (for example
  `quantreg::rq.fit(x, y, tau = 0.5, method = "br")`) to separate data problems
  from pipeline problems.
