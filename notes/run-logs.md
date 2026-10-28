# Where the logs are for a pipeline run

| What | Where |
| --- | --- |
| Everything, interleaved: runner output plus every worker's events (one file per project) | `cache/logs/<project>_events.log`, e.g. `covid_hosp_prod_events.log` |
| Runner output (`tar_make` progress lines) | `cache/logs/prod_{covid,flu,rsv}` (Makefile `tee`) |
| Freshness polls and start/finish of the whole systemd run | `cache/logs/prod_forecast_freshness.log` |
| Each crew worker's stdout and stderr (random file names) | `local_logs/` |
| systemd unit output | `journalctl --user -u prod-forecasts.service` |

`rg` skips `cache/` and `local_logs/` (gitignored); use `rg --no-ignore`.

## The events file

`cache/logs/<project>_events.log` is the one place to look. Every line starts
with who wrote it:

- `[runner] <text>`: a line of `tar_make` output. The `prod-*` and `eval-*`
  recipes in the `Makefile` copy it in (`tee_events`), so this needs `make`; a
  bare `Rscript scripts/run.R` only writes the `run start` / `run end` lines.
- `[worker <pid> <target>] <iso time> <text>`: written by a crew worker through
  `log_event()` (`R/event_log.R`). `run_forecaster` writes `start` / `done` for
  each forecast, and `with_event_log()` also copies every warning, message and
  error the forecaster raises (`warning <id> aheads=<n>: <text>`). They are
  still raised normally, so `targets` records them too.

Lines from different processes interleave in the order they were written, so
`grep '\[worker 1234 '` or a target name picks out one worker. The raw stdout of
workers that are not forecasts (fetches, reports) is not in this file; it stays
in `local_logs/`.

`make status` has an "in flight" section built from it: a worker whose last
`start` / `done` line is a `start` is running that forecast, with how long it
has been going and whether the pid still exists (`alive = FALSE` means the
worker died). It also counts today's worker warnings and errors and shows the
last three.

## Inspecting a live run

- `tar_progress(store = "<project>")` in an R session (not `TAR_RUN_PROJECT`,
  which only `scripts/run.R` reads); targets in `dispatched` are with a worker.
- `ls -l /proc/<pid>/fd | rg local_logs` maps a worker pid to its log file
  without printing its command line.
- `forecast_nhsn` / `forecast_nssp` time out after 30 minutes
  (`forecast_resources()` in `R/targets/shared_utils.R`); see
  `notes/quantreg-solver.md`.
