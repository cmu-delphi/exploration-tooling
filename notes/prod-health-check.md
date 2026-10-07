# Prod health check

Each prod run renders one health notebook per disease,
`rendered_reports/{forecast_date}_{disease}_health_on_{run_date}.html`. The
latest ones are linked at the top of "Most recent week" on the reports site, and
all of them under "Weekly Health Checks". Check them before the fan plots.

## What it checks

The `health_coverage` target (built per forecast date in
`build_prod_ensemble_targets()`, `R/targets/prod_shared.R`) compares what the
submitted `ensemble_mix` was configured to use with what it actually got, for
the nhsn and nssp signals. The logic lives in `R/prod_health.R`.

- **Submission coverage**: every (location, horizon) that should be submitted,
  and whether it was. Expected locations are those any forecaster produced for
  the signal, minus locations whose weights are all zero.
- **Substituted components**: every (component, location, horizon) with a
  positive configured weight that produced no forecast. `ensemble_weighted()`
  spreads a missing component's weight over the components that are present at
  that (location, horizon), so the ensemble still submits, but with a different
  mix. Typical cases:
  - `revision_aware` missing at h−1 during an NHSN reporting gap, or for a
    location that reports late: h−1 is then `climate_linear` alone.
  - An AR component missing for a few locations.
  AR components dropped at negative horizons by `drop_negative_aheads` are a
  design choice, not a gap, and are not listed.
- **Pipeline errors and warnings**: read from the targets metadata
  (`tar_meta(fields = c(warnings, error))`) after `tar_make()` finishes, for
  this round's targets and for shared data targets. Forecasters that decline to
  forecast (for example `revision_aware` when the h−1 week isn't reported) warn,
  and those warnings land here. Warnings printed during `tar_make()` are not
  otherwise seen, since the run is unattended.

## Status and alerts

- **ok**: nothing missing.
- **attention**: some components were substituted or a few (location, horizon)
  pairs are missing. Shown in the notebook only.
- **fail**: some signal and horizon is missing for more than a quarter of its
  locations.
- **error**: a pipeline target errored, or `health_coverage` wasn't built.

`scripts/run_prod_if_fresh.R` runs both pipelines, renders both health
notebooks, and publishes the site even when something failed, so the notebook
explaining a failure is online. On `fail`, `error`, a failed publish step, or
stale data at the 14:00 cutoff, it posts one Slack message through the incoming
webhook in `SLACK_WEBHOOK_URL`, exits nonzero, and leaves the day unfinished so
the next half-hourly firing retries. The same message is sent at most once a
day. If the webhook isn't set or the post fails, it logs a CRITICAL line saying
the alert was not delivered.
After every publish, failed or not, it also posts a message with links to the
site and to the prod reports and health notebooks rendered that day.
The first run to end at or after 14:00 posts the output of `make status`
(`scripts/prod_status.R`), once a day; delete `cache/prod_status_posted_<date>`
to post it again.

The webhook is a Slack incoming webhook for the team channel. Set it in the
`forecaster` user's `~/.Renviron` on the prod box (the project `.Rprofile`
reads that file); never commit it.
