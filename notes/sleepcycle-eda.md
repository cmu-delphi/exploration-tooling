# Sleepcycle EDA

`sleepcycle_eda` is a targets project (no forecasting) that compares the sleepcycle `pct_cough_2_plus_7dav`
state signal with NHSN flu/COVID admissions and NSSP flu/COVID ED visit percentages, and renders
`reports/writeups/sleepcycle/sleepcycle_cough.qmd` to `rendered_reports/sleepcycle_cough.html`.

Run: `just sleepcycle-eda` (inside the rocker distrobox; needs a Delphi epidata key for rate limits).
The data targets are defined by `sleepcycle_data_targets()` in `R/targets/sleepcycle_data_targets.R`, so other projects can reuse them.

- Sleepcycle covers 43 states. The "national" series is a population-weighted mean (NHSN: sum) over those states only.
- All sources use their latest vintage, not real-time as-of data. Sleepcycle has almost no revisions (archive starts April 2026),
  so an as-of comparison would need spoofed report times. Not done yet.
- Cough 2+ looks coincident with flu, not leading it, and tracks COVID poorly (see the report).
