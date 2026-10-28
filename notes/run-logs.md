# Where the logs are for a pipeline run

| What | Where |
| --- | --- |
| Events from the runner and every crew worker (one file per project) | `cache/logs/<project>_events.log`, e.g. `covid_hosp_prod_events.log` |
| Runner output (`tar_make` progress lines) | `cache/logs/prod_{covid,flu,rsv}` (Makefile `tee`) |
| Freshness polls and start/finish of the whole systemd run | `cache/logs/prod_forecast_freshness.log` |
| Each crew worker's stdout and stderr (random file names) | `local_logs/` |
| systemd unit output | `journalctl --user -u prod-forecasts.service` |

`rg` skips `cache/` and `local_logs/` (gitignored); use `rg --no-ignore`.

## The events file

`log_event()` (`R/event_log.R`) appends `<iso time> pid=<pid> <message>`. The
runner (`scripts/run.R`) writes `run start` / `run end`; `run_forecaster` writes
`start <id> aheads=<n> rows=<n>` and `done <id> aheads=<n> in <s>s` for each
forecast. Because workers inherit `TAR_RUN_PROJECT`, they write to the same
file, so one `tail -f` shows the whole run.

`make status` has an "in flight" section built from it: a worker whose last
event is a `start` is running that forecast, with how long it has been going and
whether the pid still exists (`alive = FALSE` means the worker died).

Only forecasts and the run itself write events. Other targets (data fetches,
reports) do not, so a hang there shows up in `tar_progress()` but not here.

## Inspecting a live run

- `tar_progress(store = "<project>")` in an R session (not `TAR_RUN_PROJECT`,
  which only `scripts/run.R` reads); targets in `dispatched` are with a worker.
- `ls -l /proc/<pid>/fd | rg local_logs` maps a worker pid to its log file
  without printing its command line.
- `forecast_nhsn` / `forecast_nssp` time out after 30 minutes
  (`forecast_resources()` in `R/targets/shared_utils.R`); see
  `notes/quantreg-solver.md`.
