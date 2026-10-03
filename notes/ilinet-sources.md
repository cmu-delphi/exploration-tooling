# ILINet sources: epidata v4 `fluview` vs v5 `fluview_ilinet`

Checks whether v5 can replace the v4 `fluview` endpoint as the issue-versioned ILINet archive for flu
backtests. Script: `scripts/one_offs/compare_ilinet_sources.py` (`fetch`, then `analyze`; the full table
dump is in `cache/ilinet/analysis_output.md`). Data pulled 2026-10-02. Latest issue in both: 202637 (report 2026-09-25).

## The two sources

**v4** `https://api.delphi.cmu.edu/epidata/fluview/` (`epidatr::pub_fluview`). It stores one row per
`(region, epiweek, issue)`, so every issue repeats the whole window of weeks that CDC re-published. Columns:
`wili, ili, num_ili, num_patients, num_providers, num_age_0..5, release_date, lag`. Regions: `nat`, `hhs1-10`,
`cen1-9`, states, `jfk` (NYC), `ny` (all of NY, which v4 builds itself). One call with `regions=ca`,
`epiweeks=199701-203053`, `issues=199701-203053` returns everything for a region (no row cap was hit at 93k rows).

**v5** `https://delphi.cmu.edu/epidata/v5`, source `fluview_ilinet`.
- Signals: `ili, wili, num_ili, num_patients, num_providers`. Geo types: `nation` (`us`), `state`, `hhs`, `census_division`.
- Extra key `age_group` (`all`, plus `0-4, 5-24, 25-49, 25-64, 50-64, 65+` on `num_ili`). Filter on `age_group = all`.
- `fill_method` is `source`, except v5 `ny` (all of NY), which has fill_method `nyc_plus_ny_minus_nyc` because it is derived.
- `/archive/` stores changes only: you get a row when a `(signal, geo, reference_time, age_group)` value changes.
  To rebuild an issue, carry the last change forward (as-of join). `/snapshot/?snapshot_date=D` does that on the server.
- `reference_time` is the Saturday that ends the MMWR week. `report_time` is a UTC timestamp, midnight except for the newest row.
- The deployed `/archive/` takes only `source, signal, geo_type, fill_method, extra_keys, limit, use_pagination,
  report_time_query, columns, format, header`. The `geo_value` and `reference_times` filters in `../cast-api` are
  not deployed yet, so pull a whole `(signal, geo_type)` at once. That's 10 requests and 36 MB of CSV for state plus nation.
- Auth: send `token: $DELPHI_EPIDATA_KEY` as a header (same key as v4). Anonymous archive calls are limited to 5/min and 60/hour.

```
GET /epidata/v5/archive/?source=fluview_ilinet&signal=ili&geo_type=state     (header token: …)
GET /epidata/v5/metadata/?source=fluview_ilinet
```

### Geo mapping

| v4 | v5 | note |
|---|---|---|
| `nat` | `us` (geo_type `nation`) | |
| `jfk` | `nyc` | NYC; v5 stops at week 2025-09-27 |
| `ny` | `ny` with fill_method `nyc_plus_ny_minus_nyc` | all of NY; v5 stops at week 2025-09-27 |
| none | `ny_minus_nyc` | upstate only; v5 continues past 2025-09 |
| other states, `dc pr vi` | same codes | |

v4 `ny` and v5 `ny` are both sums of NYC and upstate, but they disagree slightly. Only 32% of `ili` and `num_patients`
values match exactly, with a median relative difference of 2e-5. The NYC component matches exactly, so v4's upstate
component (or the revision it summed) is a few patients off. Both sources stop publishing NYC and the NY total
after week 2025-39. Only v5 `ny_minus_nyc` continues, and I did not check whether it switches to the whole state.

## Coverage

| | v4 nat | v5 nat | v4 states | v5 states |
|---|---|---|---|---|
| geos | 1 | 1 | 54 | 54 + `ny_minus_nyc` |
| weeks | 1997-10-04 – 2026-09-19 | same | 2010-10-09 – 2026-09-19 | same |
| issues / reports | 200347 – 202637 | 2003-11-28 – 2026-09-25 | 201740 – 202637 | 2017-10-20 – 2026-09-25 |
| rows (ili) | 56,898 | 20,172 (changes) | 1,966,481 | 93,498 (changes) |
| state `wili` | n/a | n/a | yes, always equal to `ili` (100%) | absent |

- Both sources have state issues only from 2017w40. Before that, state values exist only as the 2017w40 backfill.
  "CA history back to 2010" therefore means weeks back to 2010, not issues.
- Both skip the same releases: 202539–202544 (October 2025, when CDC published nothing) and 202350 for every state except `pr`.
  In 202350 (released 2023-12-22), both sources have only nat and `pr`.
- Value changes since 2019w40 are almost equal (state ili: 55,928 in v4 vs 56,915 in v5). v5 has slightly more
  because of the extra `ny_minus_nyc` geo and a few off-cycle reports.
- In the latest version, these weeks exist in v4 only:
  - `fl`: 574 weeks from 2010-10-09 to 2021-10-02 (v5 Florida starts at week 2021-10-09).
  - `la`, `vi`, `pr`: 313, 261 and 156 old weeks (2010 to 2013–2016) that appear only in v4's 2017w40 backfill.
  - `dc`: one week, 2022-02-26.
- `ny_minus_nyc` is in v5 only (833 weeks).

## Version alignment

**The v5 `report_time` date is the v4 `release_date`: the Friday CDC publishes FluView, normally the issue
week's Saturday plus 6 days.** 1,062 of 1,071 distinct v5 report dates equal a v4 national release date. In the
remaining 9, v4 stamped a release a day or more late for some geos (August 2025, 2017-10-24, 2014). Holiday-delayed
releases (9–10 days after week end, 34 issues) fall on the same date in both sources.

Shift test (state + nat `ili`, issues ≥ 2019w40): agreement between v4 issue W and v5 read as of `release_date + k`:

| k (days) | -7 | -1 | 0 | +1 | +6 | +7 |
|---|---|---|---|---|---|---|
| % within 5e-6 | 93.68 | 93.87 | **97.74** | 97.74 | 97.54 | 95.27 |

(The ceiling of 97.7 comes from the NY sum difference. Without NY it is 99.99.) There is no off-by-one-week
offset. To reproduce v4 issue W from v5, take the last change with `report_time::date <= ew_end(W) + 6`, or
better, v4's own `release_date`, which covers delayed releases. In a backtest with a forecast date D, the v5 as-of-D
snapshot is the v4 data available on D, so no translation to issues is needed.

## Value agreement

Each v4 `(geo, week, issue)` row is compared with the v5 value as of that issue's release date.

| geo | signal | n | % v5 missing | % exact | % within 5e-6 | max abs diff |
|---|---|---|---|---|---|---|
| nat | ili | 56,898 | 0 | 99.88 | 99.97 | 0.029 |
| nat | wili | 56,898 | 0 | 99.79 | 99.97 | 0.103 |
| nat | counts (each) | 56,898 | 0 | 99.97–99.98 | 99.97–99.98 | — |
| state | ili | 1,966,481 | 1.11 | 97.49 | 97.68 | 8.18 |
| state | num_ili | 1,966,481 | 1.11 | 98.86 | 98.86 | 1430 |
| state | num_patients | 1,966,481 | 1.11 | 97.67 | 97.67 | 104,269 |

Excluding NY, the 2017w40–41 backfill issues and rows that v5 doesn't have:

| geo | signal | n | % exact | max abs diff |
|---|---|---|---|---|
| nat | all 5 | 36,306 | 100 | 0 |
| state | ili | 1,888,666 | 99.99 | 8.18 (VT, below) |
| state | num_ili / num_patients / num_providers | 1,888,762 | 99.99 | 92 / 6886 / 14 |

The two sources use the same units: ILI is a percent (e.g. 6.47), not a proportion. Neither rounds differently.
The national "exact vs within 5e-6" gap in 2009–2012 issues is float noise (≤1.3e-8).

The residual mismatches, by cause:
1. **NY sum** (about 23.6k ili rows, median diff 7e-5): described in the geo mapping section.
2. **2017w40/41 backfill** (about 90 rows in 37 states and 14 nat weeks, weeks 2017-05 to 2017-10): v4's first state
   issue (release_date 2017-10-24, filled in later) and nat 201741 differ by 0.002–1.2 ILI points from v5.
3. **VT, issues 202502–202513**: v4 has `num_patients = 10/11` for weeks 202440–202501, which pushes ili up to about
   8 points high. v5 never records those values and keeps 6375 until the zero in 202514, which both sources have.
   v4 looks like it captured a corrupt CDC publication that v5 missed or rejected.
4. **DC week 2022-02-26**: v4 has `num_patients = 10` from 202208 to 202339, while v5 has 0 throughout.
5. **v5 missing** (1.1% of state rows): FL before 2021-10 (19.5k rows), and LA/PR/VI old weeks in v4's 201740 backfill.
   A few hundred more rows come from 201740 issues released before v5's first report.

## Revision spot checks

The as-of trajectories of CA, TX and nat (wili) for weeks 201952, 202250 and 202350 match v4 exactly at every
issue: the same values, revised on the same Fridays. Examples:
- CA 201952: 3.803 → 4.924 → … → 4.699, with the last change in the 202026 release.
- CA 202350: first value in 202351 in both sources (202350 missing in both), last revision on 2024-10-04.
- nat wili 202350: 24 revisions through 202531, all identical.

NY has the same revision timing with a constant offset of about 2e-4. The full trajectories are in `cache/ilinet/analysis_output.md`.

## Finalized values (latest version, all weeks)

| geo | signal | geo-weeks | v4 only | v5 only | % exact | max abs diff |
|---|---|---|---|---|---|---|
| nat | ili / wili / counts | 1417 | 0 | 0 | 99.7–100 (100 within 1e-8) | 1e-8 |
| state | ili | 45,356 | 1305 | 833 | 98.94 (99.3 within 5e-6) | 0.00096 (NY) |
| state | num_ili / num_patients / num_providers | 45,356 | 1304 | 833 | 99.99 / 99.3 / 100 | 2 / 56 / 0 |

All finalized state differences are in NY, plus DC 2022-02-26.

## Recommendation

v5 can replace v4 for state and national ILINet backtests. Exact agreement is 99.99% outside NY, and version
dates and revision trajectories match one for one. Read v5 as of the forecast date, or as of the v4 `release_date`
to reproduce an issue. Caveats:
- **State wILI**: v5 has no state `wili`. In v4 it is a copy of `ili`, so use `ili` for states.
- **NY**: use v5 `ny` with fill_method `nyc_plus_ny_minus_nyc`. Small differences from v4 `ny` (≈2e-5 relative)
  shift historical scores slightly. After 2025-09 there is no NYC or NY total in either source, so define what "NY" means from 2025-26 on.
- **Florida before 2021-10**: v4 has 574 weeks that v5 lacks. This only matters if a training window reaches before
  2021-22 for FL (it has no issue history before 2017w40 anyway).
- **State issue history starts 2017w40 in both sources.** For a state as-of before October 2017, neither source helps.
- **VT 2025 and DC 2022-02-26**: v4 contains bad values (num_patients = 10) that v5 doesn't have. v5 is the
  cleaner archive here, but a backtest replayed on v5 won't reproduce whatever a 2025 forecast saw from v4 for VT
  in January–March 2025.
- **API**: until `geo_value` and `reference_times` filters are deployed, pull whole `(signal, geo_type)` archives with
  the token header (cheap: 36 MB) and do the as-of join locally (DuckDB `ASOF JOIN`). Don't use `/snapshot/` per
  date (3/min anonymous). Not compared: HHS and census regions, and age-group counts (v5 has them only for national `num_ili`).
