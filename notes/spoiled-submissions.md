# Spoiled hub submissions

Forecasts we submitted to the hubs that were broken by a pipeline bug, not by
the model. They stay in the hub forever, so anything that reads our hub
submissions (the calibration experiments, scoring) should drop them.
`hub_read_forecasts()` does this by default from `HUB_SPOILED_SUBMISSIONS` in
`R/calibration/hub_data.R`; add new cases there and here.

Covid submissions were checked for the two flu patterns (all-zero quantiles,
halved US) and are clean. The candidates found by the sweep below are
recorded with their causes but are not yet excluded.

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

## Covid, 2024-11-23 round: about 2.4x too high everywhere (candidate)

At h−1 and h0 the median is about 2.2–2.6x both the value known at the time
and the finalized value, in 41 of 49 locations; h1–h3 are off by a similar
factor.

Cause: NHSN's 2024-11-20 release inflated week 2024-11-16 in many states
(covid: PA 6124, MI 3732, US 17593; corrected on 2024-11-21 to 1005, 600 and
7437). Flu had the same bad release (PA 729 → 111). The first run for this
round (2024-11-20) used that data; the AR models are pooled across states,
so a jump in a few large states raised every state's forecast. On 2024-11-21
we re-ran on the corrected data and re-submitted flu (FluSight hub commit
`c160da00`, "updated 11-23 forecast for new data"), but the covid file was
never re-submitted, so the hub holds the 2024-11-20 version. A prod report
rendered on 2024-11-21 (`reports/2024-11-21_covid_prod_on_2024-11-21.html`)
shows the forecasts we would have sent (GA h−1 median 64 against the
submitted 339). A May 2025 replay of the store also gives forecasts about
half the submitted ones.

## Covid, 2026-04-25 round: h−1 near zero in many states (candidate)

The h−1 median is far below both the value known at the time and the
finalized value in about half the locations (WI 2 vs 27 known, VA 3 vs 28,
MA 9 vs 44, IN 9 vs 38), and far below the same round's own h0 (WI 10, VA 25).
h0–h3 look normal. The data were fine: the 2026-04-22 release already had
these values for week 2026-04-18, and later revisions barely changed them.

Cause, not confirmed: at the time, covid h−1 came only from the
`climate_linear` ensemble (the AR models were filtered out of negative
aheads), and at short aheads that ensemble is mostly the linear baseline,
which extrapolates the last few reported weeks. The near-zeros are what a
linear trend through 2026-04-04 → 2026-04-11 gives (WI 23 → 14, VA 51 → 26)
if week 2026-04-18 is missing, so the run most likely did not have that
week. That day the cast API moved from v2 to v5; the fix (`6d91648`, "keep
up with cast-api updates") was committed at 16:11 PDT, nine minutes after
the submission (covid hub `a6b94bb`, 16:02), so the submission probably ran
on a stale archive fetch. There is no covid prod store for that date to
check.

## Flu, 2026-04-04 round: h−1 about 1.8x too high (candidate, design issue)

The h−1 median is about 1.6–1.8x both the value known then and the finalized
value in 22 of 35 locations (OH 204 vs 60, CO 90 vs 34, MN 92 vs 36). h0 is
fine. The prod store replay reproduces the submission exactly, so this is
what the pipeline does, not a one-off failure.

Cause: week 2026-03-28 fell abruptly (OH 257 → 60, US about 5900 → 3300; the
finalized data agree). At h−1 the AR models are run through
`filter_minus_one_ahead()`, which drops the week being nowcast, so they
extrapolate from 2026-03-21 and never see the drop, even though it was
already reported. The same mechanism puts 2025-26 h−1 above the reported
value by 10–30% in other weeks of the decline. This is a design choice in
how h−1 is built, not a bug in one round, so excluding only this round would
leave the milder cases in.

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

## To do

- Run the same sweep on `windowed_seasonal` (the main ensemble component,
  used as a proxy for the ensemble in the calibration harness) to check it
  does not hit similar pathologies or data errors. Needs a variant of the
  script that reads the harness forecasts instead of the hub files.
