# Where the logs are for a pipeline run

| What | Where |
| --- | --- |
| Runner output (`tar_make` progress, timestamped) | `cache/logs/prod_{covid,flu,rsv}`, `cache/logs/eval_{covid,flu,rsv}` (Makefile `tee`, or `scripts/run_prod_if_fresh.R` on the systemd path) |
| Each crew worker's stdout and stderr | `local_logs/<project>/<run id>/<random>.log` |
| Warnings and errors per target | `tar_meta(fields = c(warnings, error), store = "<project>")` |
| Freshness polls and start/finish of the whole systemd run | `cache/logs/prod_forecast_freshness.log` |
| systemd unit output | `journalctl --user -u prod-forecasts.service` |

`rg` skips `cache/` and `local_logs/` (gitignored); use `rg --no-ignore`.

## Following a run

`make logs project=covid_hosp_prod` (`scripts/tail_logs.sh`) follows the
runner log and every worker log of the project's latest run as one stream.
Lines are prefixed `[runner]` or `[worker <first 6 chars of the log name>]`. It
picks up worker logs that crew creates mid-run, since crew starts workers
lazily and replaces them after they idle out. Explore projects have no runner
log, so only their worker logs are followed.

The runner log is where to look first: with the `timestamp` reporter
(`reporter_make` in `_targets.yaml`, passed explicitly by `scripts/run.R` and
`run_prod_if_fresh.R`), each target gets a timestamped `dispatched` and
`completed [duration]` line. Worker logs show what the target code prints:
`message()` output, stdout, and warnings as they happen. targets also stores
the warnings in its metadata when the target finishes.

## Worker log directories

crew names worker logs with random tokens and records no mapping, so the
directory carries the meaning instead. `worker_log_dir()`
(`R/targets/shared_utils.R`) builds `local_logs/<TAR_RUN_PROJECT>/<TAR_RUN_ID>`.
`scripts/run.R` and `run_project_pipeline` set `TAR_RUN_ID` to a timestamp like
`20261028T140000`, so run directories sort by start time. Calling `tar_make()`
yourself without `TAR_RUN_ID` gets a fresh timestamp each time the pipeline
script is loaded.

Worker startup is kept quiet: `set_targets_config()` sets
`RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` (skips renv's out-of-sync check, which
can also spend seconds scanning dependencies) and `QUIET_PACKAGE_STARTUP=true`,
which makes `.Rprofile` drop package attach and masking messages. What remains
is crew's own `sessionInfo()` header, which `make logs` filters out.

## Inspecting a live run

- `make status` has an "in flight" section: targets that `tar_progress()`
  reports as `dispatched`, with their dispatch time from the runner log, and
  today's warnings and errors from `tar_meta()`. A target stays `dispatched`
  if the runner died, so check the elapsed time.
- `tar_progress(store = "<project>")` in an R session (not `TAR_RUN_PROJECT`,
  which only `scripts/run.R` reads).
- `ls -l /proc/<pid>/fd | rg local_logs` maps a worker pid to its log file
  without printing its command line.
- `forecast_nhsn` / `forecast_nssp` time out after 30 minutes
  (`forecast_resources()` in `R/targets/shared_utils.R`); see
  `notes/quantreg-solver.md`.
