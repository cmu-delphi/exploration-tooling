Systemd user units replacing the `forecaster` crontab.

These were written and validated (via `systemd-analyze calendar`) on a dev
machine, not the production box. Review the `WorkingDirectory=` paths in each
`.service` file before installing — they assume the repo lives at
`~/prod/exploration-tooling-2024` and NWSS data at `~/nwss_data`, matching the
original crontab.

## Mapping from the old crontab

| Cron job | Unit pair |
| --- | --- |
| `build_nhsn_archive.R` every 5 min | `nhsn-archive-builder.{service,timer}` |
| `build_nssp_archive.R` every 5 min | `nssp-archive-builder.{service,timer}` |
| `get_forecast_data.R` Tue 09:00 | `get-forecast-data.{service,timer}` |
| `make prod-log` Wed 10:10 | `prod-forecasts.{service,timer}` (see below) |
| `nwss_download_script.py` daily 06:25 | `nwss-download.{service,timer}` |

All timers carry `TimeZone=America/Los_Angeles` to match the old
`CRON_TZ=America/Los_Angeles`.

## Forecast freshness gate

`prod-forecasts.timer` fires every 30 minutes from 07:00 to 14:00 Pacific on
Wednesdays, instead of once at 10:10. Each firing runs `make
prod-log-if-fresh`, i.e. `Rscript scripts/run_prod_if_fresh.R`, which:

1. Skips immediately if today's forecast already completed
   (`cache/prod_forecast_done_<date>` marker).
2. Checks NHSN/NSSP freshness by calling `check_data_freshness()` (`R/utils.R`,
   threshold 7 days) directly via `Rscript`, once per `TAR_PROJECT` for each of
   `covid_hosp_prod` and `flu_hosp_prod`. This builds (or confirms up to date)
   the `nhsn_archive_data`/`nssp_archive_data` targets via `tar_make()` and
   checks the latest `time_value` actually present in each archive — not just
   the upstream API's self-reported "latest update" field, which has been
   unreliable. Because these are `tar_change()` targets, the build step itself
   re-fetches from upstream if new data is available, so no separate
   invalidation step is needed.
3. If fresh: runs the covid and flu prod pipelines (`tar_make()` in-process,
   logged to `cache/logs/prod_covid`/`prod_flu`), syncs `reports/` with S3,
   calls `update_site()`, and deploys to netlify — in that order, aborting and
   logging `CRITICAL` on the first step that fails. On success, writes the
   marker.
4. If stale:
   - before 14:00: waits for the next scheduled firing (which will re-check
     and re-fetch).
   - at 14:00 (last firing of the day): logs a `CRITICAL` line to
     `cache/logs/prod_forecast_freshness.log` and gives up for the day — no
     forecast is submitted.

## Install (on the production box, as the `forecaster` user)

```sh
mkdir -p ~/.config/systemd/user
cp deploy/systemd/*.service deploy/systemd/*.timer ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now \
  nhsn-archive-builder.timer \
  nssp-archive-builder.timer \
  get-forecast-data.timer \
  prod-forecasts.timer \
  nwss-download.timer

# Required so these run without an active login session:
sudo loginctl enable-linger forecaster
```

Check status with `systemctl --user list-timers` and `journalctl --user -u
<unit>`. Once confirmed working, remove the corresponding entries from
`crontab -e` for the `forecaster` user.
