# Spoiled hub submissions

Forecasts we submitted to the hubs that were broken by a pipeline bug, not by
the model. They stay in the hub forever, so anything that reads our hub
submissions (the calibration experiments, scoring) should drop them.
`hub_read_forecasts()` does this by default from `HUB_SPOILED_SUBMISSIONS` in
`R/calibration/hub_data.R`; add new cases there and here.

Covid submissions were checked for both patterns below and are clean.

## Flu, 2025-11-22 round: h−1 is zero everywhere

Every quantile at horizon −1 is exactly 0, in all 53 locations. h0–h3 are
normal.

Cause: the first forecast after the autumn 2025 data gap (forecast date
2025-11-19). That week the AR models were kept out of negative aheads, so h−1
came only from the `climate_linear` ensemble. The weights block added that day
in `pipelines/flu_geo_exclusions.csv` (commit `492bade`) dated the `linear`,
`climate_base` and other climate rows `2024-11-19` instead of `2025-11-19`.
The weights parser matches dates exactly, so those forecasters had no weight,
and the `climate_linear` ensemble came out as 0. The AR models were let into
h−1 from 2025-11-26 (`24f366d`), and the typo was fixed on 2026-09-30.
Before that fix, replays of 2025-11-19 in the prod targets store reproduced
the zero `ensemble_clim_lin`.

## Flu, US, rounds 2024-12-14, 2024-12-21 and 2025-01-04: halved

The US forecast is about half the sum of the state forecasts at every horizon
(ratio 0.45–0.54). In every other live round the US median is within a few
percent of the state sum at h−1 and h0. The cause is not confirmed. Commits
from early 2025 fixed several `us` / `usa` / `US` mixups, and averaging the
US forecast with a zero duplicate would halve it exactly.

## How to find more

`scripts/one_offs/sweep_submission_deviations.R [flu|covid]` joins every
submitted forecast with the NHSN value known when it was made and the
finalized value, writes `cache/submission_sweep_{disease}.csv`, and prints
two flags at h−1 and h0:

- **suspect**: the median is off by more than 2x from both the value known
  then and the finalized value. This is the useful one for pipeline bugs.
- **missed**: the finalized value is outside the 95% interval and the median
  is off by more than 2x. This also catches data problems (bad first
  reports, holiday delays, the autumn 2025 gap).

A round where many locations are flagged together usually means a pipeline
problem; scattered single locations are usually model error or bad data. The
halved-US check (US median vs the sum of state medians) is not in the script;
it is a one-line duckdb query over the hub CSVs.
