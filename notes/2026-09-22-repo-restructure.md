# Repo Structure Migration

Temp tracking doc — delete when done.

## Decisions

- Pipeline definitions move to **top-level `pipelines/`** (not scripts). They are declarative
  config, not imperative scripts.
- `scripts/reports/` moves to top-level `reports/`; pipeline templates go under `pipelines/templates/`.
- Root `reports/` renamed to `rendered_reports/` (Netlify output).
- `scripts/one_offs/` render artifacts (PDFs, PNGs, cache dirs) get gitignored, not the sources.

---

## 1. Create `pipelines/` at root

Move from `scripts/` → `pipelines/`:

```
covid_hosp_prod.R
covid_hosp_explore.R
flu_hosp_prod.R
flu_hosp_explore.R
rsv_hosp_prod.R           (untracked — move here)
covid_data_substitutions.csv
covid_geo_exclusions.csv
covid_nssp_geo_exclusions.csv
flu_data_substitutions.csv
flu_geo_exclusions.csv
flu_nssp_geo_exclusions.csv
rsv_data_substitutions.csv   (untracked — move here)
rsv_geo_exclusions.csv       (untracked — move here)
rsv_nssp_geo_exclusions.csv  (untracked — move here)
```

Move from `scripts/reports` → `pipelines/`:

```
templates/                ← rendered by targets pipeline, parameterized
    forecast_report.Rmd
    ongoing_score_report.Rmd
    score_report.Rmd
    comparison-notebook.Rmd
    overall-comparison-notebook.Rmd
    new_data.Rmd
```

**References to update after move:**
- [ ] `_targets.yaml` — `script:` paths
- [ ] `Makefile` — prod/explore recipe paths
- [ ] `deploy/systemd/` — if any scripts reference paths directly
- [ ] the scripts here to point at the templates

---

## 2. Reorganize `scripts/`

### 2a. Create `scripts/data/`

Move from `scripts/`:
- `build_nhsn_archive.R`
- `build_nssp_archive.R`
- `get_forecast_data.R`

**References to update:**
- [ ] `Makefile` — archive build recipes
- [ ] `deploy/systemd/` — systemd timer unit paths (`ExecStart=`)

### 2b. Create `scripts/calibration/`

Move from `scripts/`:
- `calibration_harness.R`
- `calibration_ili_backfill.R`
- `calibration_ws_backfill.R`
- `calibration_ws_experiments.R`
- `calibration_export_series.R`

### 2c. Create `scripts/app/`

Move from `scripts/`:
- `app.R`
- `dashboard.R`

### 2d. Move to `scripts/one_offs/`

From `scripts/` root (currently untracked):
- `compare_flusight_populations.py`
- `covid_archive_explore.R`
- `render_prod_explore_comparison.R`
- `covid_hosp_prod_2.R` → **DELETE**
- `covid_hosp_prod_2.py` → **DELETE**
- `flu_hosp_prod_2.R` → **DELETE**

From root:
- `nm-flu-revisions.R` → one_offs or _local (clarify intent)
- `step-through-smoothed-scaled.R` → one_offs or _local (clarify intent)

### 2e. `scripts/` root — stays

- `run.R`
- `run_prod_if_fresh.R`
- `run_garbage.R`
- `prune.R`
- `commit-script.sh`

### 2f. `scripts/` subdirs — stays as-is

- `scripts/one_offs/` — one-off analyses
- `scripts/oracle/` — oracle capture/compare
- `scripts/va_explore/` — VA data exploration

---

## 3. Move `scripts/reports/` → top-level `reports/`

Report sources should not be buried in `scripts/`. Move to top-level and organize into subdirs.

### 3a. Gitignore render artifacts inside `scripts/reports/`

```
reports/*_cache/
reports/*_files/
reports/**/*.html
reports/**/*.png
```

(These are already rendering into `rendered_reports/` — no need to track them in sources.)

### 3b. Subdirectory layout

```
reports/
  setup.qmd                          ← shared setup, referenced by other reports
  writeups/                          ← manually rendered, one-per-topic
    calibration/                     ← coherent cluster
      calibration_qt_covid.Rmd
      calibration_qt_flu.Rmd
      calibration_qt_gallery_covid.Rmd
      calibration_qt_gallery_flu.Rmd
      calibration_qt_seasons_covid.Rmd
      calibration_qt_seasons_flu.Rmd
    nowcasting/                      ← coherent cluster
      baseline_linear_model.Rmd
      baseline_nowcast_covid.qmd
      baseline_nowcast_covid_nssp.qmd
      baseline_nowcast_flu_nhsn.qmd
      baseline_nowcast_flu_nssp.qmd
      nowcasting.qmd
      nowcasting_pipeline.md
    revision/                        ← coherent cluster
      revision_outlier_calibration.qmd
      revision_summary_report_2025.Rmd
      revision_synth_test.R
      revision_target_staleness.R
    presentations/                   ← coherent cluster
      season_2025_talk/
      delphi_forecasting_2025/
    climatological_model.Rmd         ← everything else flat
    climatological_model_covid.Rmd
    decreasing_forecasters.Rmd
    eval_so_far.Rmd
    first_day_wrong.Rmd
    forecast_dashboard.Rmd
    growth_rate_window.R
    growth_rate_window.Rmd
    growth_rate_window.qmd
    phase_selection_2025.Rmd
    prod-explore-comparison.Rmd
    season_summary_2025.Rmd
```

**References to update after move:**
- [ ] `Makefile` — all `scripts/reports/` → `reports/` in render source paths

---

## 4. Rename `reports/` → `rendered_reports/`

`reports/` is the Netlify-deployed rendered output. Renaming makes the distinction obvious.

**References to update:**
- [ ] `Makefile` — `reports/` → `rendered_reports/` in S3 sync, render `output_file=`, `--output-dir`, `cp` commands
- [ ] `R/submission.R` (`update_site()`) — `reports_dir <- "reports"` → `"rendered_reports"`
- [ ] `netlify.toml` or Netlify site config — publish directory

---

## 5. Untracked files

- [ ] `tests/testthat/test-aux-data-utils.R` delete
- [ ] `tests/testthat/test-targets.R` delete
- [ ] `scripts/reports/season_2025_talk/` — clarify: keep in repo?
- [ ] `scripts/one_offs/` 
  - [ ] ensemble_weight_scheme.Rmd delete
  - [ ] ensembling_linear_climate.Rmd delete
  - [ ] 2024-02-17-UMass-flusion.csv delete

### Gitignore

Add to `.gitignore`:

```
# Generated data / archives
nhsn_archive_data
nhsn_archive_data.parquet
raw_nhsn_data.zip
ca_nhsn_version_history.csv
tx_nhsn_version_history.csv
seasonal_features/

# Regression test artifacts
*_regr/
*_regr_prod/

# Rendered report artifacts in reports/
reports/*_cache/
reports/**/*_cache/
reports/*_files/
reports/**/*_files/
reports/**/*.html
reports/**/*.png

# one_offs render artifacts
scripts/one_offs/*.pdf
scripts/one_offs/*.png
scripts/one_offs/*_cache/
scripts/one_offs/*_files/

# Scratch
temp.R
tmp2.R
slide_forecaster_error.rds
reportsBackup2/
notes.org
```

---

### decide later
- [ ] `scripts/reports/` untracked Rmd/qmd sources (eval_so_far, forecast_dashboard,
      growth_rate_window, nowcasting_pipeline.md, phase_selection_2025, prod-explore-comparison,
      revision_outlier_calibration.qmd, revision_synth_test.R, revision_target_staleness.R,
      setup.qmd)
- [ ] `pyproject.toml`, `uv.lock`, `.python-version` — python project
- [ ] `main.py` — clarify what this is first
  - [ ] flusion_model_location_naming.csv
## Status

- [ ] 1. Create `pipelines/` and move pipeline scripts + CSVs
- [ ] 2a. Create `scripts/data/` and move archive builders
- [ ] 2b. Create `scripts/calibration/`
- [ ] 2c. Create `scripts/app/`
- [ ] 2d. Move/delete one_offs and root scratch
- [ ] 3. Move `scripts/reports/` → `reports/` with subdirs
- [ ] 4. Rename `reports/` → `rendered_reports/`
- [ ] 5. Update `.gitignore`
- [ ] 6. Commit untracked files
- [ ] 7. Update `_targets.yaml`, `Makefile`, `deploy/systemd/`, `netlify.toml` for moved paths
- [ ] Clarify: `nm-flu-revisions.R`, `step-through-smoothed-scaled.R`, `main.py`, `season_2025_talk/`
- [ ] Delete this file
