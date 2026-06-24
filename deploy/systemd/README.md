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
Wednesdays, instead of once at 10:10. Each firing runs
`scripts/run_prod_if_fresh.R`, which skips if today's forecast already
completed, checks NHSN/NSSP freshness via `check_data_freshness()`
(`R/utils.R`), and either runs the full forecast/publish pipeline or waits for
the next firing. At the 14:00 cutoff, if data is still stale, it logs a
`CRITICAL` line to `cache/logs/prod_forecast_freshness.log` and skips the
forecast for the day. See the script for the exact steps and log paths.

## Install (on the production box, as the `forecaster` user)

```sh
mkdir -p ~/.config/systemd/user
ln -s "$(pwd)"/deploy/systemd/*.service "$(pwd)"/deploy/systemd/*.timer ~/.config/systemd/user/
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
