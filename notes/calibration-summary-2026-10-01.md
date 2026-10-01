# Calibration: where the experiments stand (2026-10-01)

A summary of E01, E02, E07 and E11–E17. Each notebook is in
`reports/calibration_experiments/` (`index.html` lists them all).

## How these were run

All of these are flu only, except E12 and E13, which cover covid as well.

- **Forecaster.** Every experiment except E11 calibrates one fixed forecaster,
  `windowed_seasonal`. It was replayed weekly from 2024-11-20 with none of
  the hand edits we make to prod data (the "clean replay"). The submitted
  ensemble changes weights and inputs by hand week to week, and some of its
  rounds are known to be spoiled. A fixed forecaster removes both from the
  comparison. E11 used the submitted ensemble, as E00–E10 did.
- **Learning truth.** The tracker learns from data as it was published each
  week, and is re-run from scratch each week, as prod would run it. Scores
  use finalized data.
- **Rounds and seasons.** The 55 hub rounds, 27 in 2024-25 and 28 in
  2025-26. Results are shown per season, because a setting can look good
  pooled and still fail in one season.
- **Horizons.** h0–h3 only. h−1 is left to the revision-aware methods.
- **States only.** US is excluded from the headline numbers. WIS is in
  counts, so US alone is about 45% of the all-locations total. The
  all-locations numbers point the same way throughout.
- **Cold start.** The tracker starts from zero offsets in November 2024
  unless a burn-in is stated.

"WIS change" is % against the uncalibrated forecast, positive is better.
"Coverage bias" is the mean |coverage − nominal| over quantile levels.

## Findings

**1. A constant learning rate on the sqrt scale is the best simple
tracker so far (E14, E16).**

On the rate scale (per 100k), a constant rate of 0.018–0.032 improves WIS
at every horizon in both seasons (E14). E11's 0.1 is past that range: WIS
gets worse at h1–h3 in both seasons. The sqrt scale does better than the
rate scale (E16). Every sqrt rate from 0.001 to 0.018 improves WIS at
every horizon in both seasons, and 0.018 gains the most.

| config | 2024-25 h0 | 2024-25 h2 | 2025-26 h0 | 2025-26 h2 |
|---|---|---|---|---|
| sqrt, constant 0.018 | +1.7 | +0.3 | +3.0 | +1.1 |
| rate, constant 0.032 | +1.0 | +0.3 | +1.9 | +0.8 |
| REF-op (current, cold) | +2.2 | 0.0 | +1.8 | +0.3 |

REF-op gains more at h0–h1 in 2024-25 but loses at h3 there (−0.8). It also barely
moves 2024-25 coverage (bias 0.106–0.124, base 0.127–0.169). Neither REF-op
nor the adaptive sqrt tracker is good in both seasons at every horizon.

**2. The calibration is a small upward shift, which is why the plots barely
change (E15).**

- **The offsets are small.** The typical offset is 2–5% of the 90%
  interval's width, and calibrated intervals are 1.00–1.04× the base width.
- **They point up.** The base under-predicts, and the tracker moves the
  whole forecast up a little.
- **The gain is a median shift.** For REF-op, shifting every quantile by
  the median offset alone gives more WIS than the full calibration. The
  width and shape changes cost WIS and only buy coverage.
- **The gain is in the center and shoulders, not the tails.** The six tail
  levels contribute at most 0.1 point.
- **Why a rate-scale constant step hurts:** a fixed step per 100k is large
  for a forecast with a narrow interval, such as a small state or an
  off-season week. For 1 forecast in 10 under E11, the offset exceeds
  0.26–0.55 of the 90% width. The sqrt scale shrinks this tail (E16).

**3. Offsets go stale after the peak, and March is where it costs (E16,
E17, E02).**

- **The March loss.** Every tracker loses WIS at h1–h3 in March of both
  seasons. After the peak the base already over-predicts, and the offsets
  are still positive. March is 4–8% of a season's WIS, so this costs about
  a point over the season.
- **Season-level coverage hides it.** Under-prediction on the ramp and
  over-prediction after the peak partly cancel in the season total.
- **Decaying the offsets toward zero from 1 March fixes March but loses
  overall (E17).** The offsets carry into the next season, so the decay
  also hands that season a near-zero start.
- **Switching calibration off from 1 March (or 15 February) is the one
  variant with a large WIS gain (E02, E17).** It is positive at every
  horizon in both seasons, but it gives up most of the coverage gain:
  - **sqrt 0.018, pooled WIS:** +2.3/+1.2/+1.2/+1.3 switched off from
    March, against +2.2/+0.8/+0.6/+0.6 left on;
  - **sqrt 0.018, coverage bias:** 0.094–0.116 switched off, 0.059–0.078
    left on, and 0.105–0.127 for the base.

**4. On the adaptive tracker, the multiplier is the only setting that
matters (E01, E02).**

| setting | effect on pooled WIS |
|---|---|
| multiplier | 16–81 points |
| window | 1–2 points |
| carry vs reset | 0.3–1.7 points |

- **On the sqrt scale, multipliers of 0.1 and 0.3 are too large.**
- **0.03** gains at h0 and loses a little at h1–h3.
- **0.01** never loses more than 0.2 points, but from a cold start it
  barely fixes 2024-25 coverage.
- **Carry beats reset.**
- **Per-level eta** gives a small gain at h1–h3; the seasonal window gives
  none.
- These agree with the original (pre-fix, finalized-truth) versions of
  E01 and E02.

**5. The gains are not specific to the ensemble (E07).**

On identical rows, the trackers rank the same way on the submitted ensemble
and on `windowed_seasonal`. The ensemble gains about a point more with
REF-op (pooled +3.9/+1.9/+1.5/+2.3 vs +2.8/+1.1/+0.8/+1.2). The two are
equally miscalibrated in 2024-25. In 2025-26 the ensemble is better
calibrated. The original E07's explanation for the gap, that
`windowed_seasonal` was better calibrated to begin with, does not hold at
h0–h3.

**6. A 2023-24 burn-in helps the adaptive trackers on flu a little, and
doesn't help a constant rate (E13).**

- **Burn-in data.** 2023-24 comes from HHS hospital data at its real
  weekly versions. It runs about 3% below NHSN for flu and 6% for covid.
- **Flu.** A warm start adds 0.5–1.6 points for REF-op and E05's single
  term. The warm configs under-cover the 50% interval (0.40–0.44).
- **Covid.** A warm start is wrong-signed: 2023-24 over-predicted, while
  the live seasons under-predict at h0.
- **Constant rate.** A warm start gives no gain on flu, and is badly
  harmful on covid (−18% at h3).

## Where this leaves the operating point

| candidate | for | against |
|---|---|---|
| sqrt, constant 0.018 | WIS-positive at every horizon in both seasons; small offsets; no learning-rate adaptation to tune | small gains at h2–h3 in 2024-25; loses in March |
| REF-op (current) | largest h0 gain in 2024-25; best with a flu burn-in | loses at h3 in 2024-25; barely improves 2024-25 coverage when cold |
| either, switched off from March | more WIS at h1–h3 | gives up most of the coverage gain |

The gains are 1–3% of WIS and come from a small upward shift. Whether that's
worth running in prod depends on how much weight goes on coverage against
WIS.

## Open questions

- **A decay that keeps the next season warm.** Shrink only the offset
  that's applied to the forecast after the peak, and keep the tracker's
  learned state, so the next season doesn't start cold.
- **Covid.** Every result here is flu except E12 and E13. Covid under the
  sqrt constant rate is untested. The covid results so far (E10, E12, E13)
  point to calibrating h0 only.
- **Month-level coverage as a headline metric**, so cancellation within a
  season stops flattering the season totals.
