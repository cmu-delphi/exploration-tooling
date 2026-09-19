# R/data/

Data acquisition and preparation utilities shared across all pipeline projects.

## Files

- **aux_data.R** — External source fetching: NHSN admissions, NHSN beds, NSSP ED visits, NWSS wastewater, FluSurv surveillance, ILI+ composite. Also owns `build_cast_api_query` / `get_cast_api_data` (the Epidata v5 archive API client).
- **geo_utils.R** — Geographic data: census population, state/HHS/national aggregation levels (`add_agg_level`, `add_hhs_region_sum`, `append_us_aggregate`), pop+density joining (`add_pop_and_density`), forecast geo filtering (`filter_forecast_geos`).
- **time_utils.R** — Epiweek/season conversion (`convert_epiweek_to_season`, `add_season_info`, `convert_epiweek_to_season_week`), weekly aggregation (`daily_to_weekly`, `daily_to_weekly_archive`), seasonal step (`step_season_week_sine`), training filter (`drop_non_seasons`).
- **data_freshness.R** — Source freshness checks: local file mtime, S3 object mtime, Socrata update time, `check_data_freshness` (gates production runs on data age).

## Key dependencies

`aux_data.R` → uses `convert_epiweek_to_season` / `convert_epiweek_to_season_week` from `time_utils.R` and `append_us_aggregate` from `geo_utils.R`.
