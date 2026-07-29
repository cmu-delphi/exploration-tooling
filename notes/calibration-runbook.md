# Calibration runbook

Every command needed to reproduce the online-calibration work from scratch.
Three repos are involved:

| path | role |
|---|---|
| `~/repos/delphi/exploration-tooling` | the R implementation, tests, driver, notebook |
| `~/repos/delphi/multiQT` | the authors' code plus our upstreamed bugfixes (branch `delphi-fixes`) — the oracle for the R tests |
| `~/repos/delphi/FluSight-forecast-hub` | submitted forecasts (sparse checkout: CMU model-output only) |

The multiQT clone must be on `delphi-fixes`. The as-published code had a
handful of defects (a learning-rate window that sliced the level axis instead
of time, an unpopulated first played column, a default `lr='adaptive++'` with
no implementation, a numpy read-only-view crash), all reported to the authors
and fixed in that branch's single commit — see its message for the details.
The R port implements the corrected semantics only; there are no
compatibility knobs for the as-published behavior.

R is not on `$PATH`; it lives in the rocker container. Every R command below is
shown with the `distrobox` prefix. Drop it if you are already inside the
container.

---

## 0. One-time setup

```sh
# Python deps are supplied per-invocation by uv; nothing to install.
# The multiQT oracle must be on the bugfix branch:
cd ~/repos/delphi/multiQT && git checkout delphi-fixes

# Refresh the hub submissions (83 CMU-TimeSeries rounds, 2023-10-14 .. 2026-05-30):
cd ~/repos/delphi/FluSight-forecast-hub && git pull --ff-only

# The hub's truth file is downloaded and cached on first use by hub_read_truth();
# to force a refresh:
cd ~/repos/delphi/exploration-tooling
rm -f cache/calibration/target-hospital-admissions.csv
```

The hub checkout is sparse (`hub-config`, `model-output/CMU-TimeSeries`,
`model-output/CMU-climate_baseline`). Leave it that way — `target-data/time-series.csv`
alone is 84 MB, and we fetch the 570 KB truth file over HTTP instead.

---

## 1. R implementation and its oracle tests

```sh
cd ~/repos/delphi/exploration-tooling

# just the tracker's tests (bit-exact oracle fixtures + properties)
distrobox enter rocker -- Rscript -e \
  'testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-qt.R")'

# the whole suite, to confirm nothing else moved (expect 183 pass / 0 fail / 9 skip)
distrobox enter rocker -- Rscript -e 'testthat::test_dir("tests/testthat")'
```

### Regenerating the oracle fixtures

`tests/testthat/fixtures/qt/` is the output of `projectedQT` on the
`delphi-fixes` branch, generated with `lr_window = 50` — the same default the
R side uses, so `test-qt.R` calls `qt_track()` with its defaults (plus
`lr_window = 50` spelled out). If you change the fixture set, regenerate and
copy:

```sh
cd ~/repos/delphi/multiQT      # on delphi-fixes
uv run --with numpy --with scikit-learn --with matplotlib \
    python make_r_fixtures.py
cp r_fixtures/*.csv ~/repos/delphi/exploration-tooling/tests/testthat/fixtures/qt/
```

---

## 2. Cross-check the R port against Python on **real** hub series

The fixtures are synthetic. This runs the same comparison on real submitted
forecasts, real hub truth, and the real `horizon + 2` delay.

```sh
cd ~/repos/delphi/exploration-tooling
distrobox enter rocker -- Rscript scripts/calibration_export_series.R
# optional: Rscript scripts/calibration_export_series.R <outdir> <n_series>

cd ~/repos/delphi/multiQT
uv run --with numpy --with scikit-learn --with matplotlib \
    python check_r_port_real_series.py
```

Expect `ALL MATCH`, max abs diff ~1e-12 (CSV round-trip noise) against a
tolerance of 1e-8. Only series complete across all 83 rounds are exported (52 of
265), because the reference Python has no notion of an inactive round.

---

## 3. Run the calibration

```sh
cd ~/repos/delphi/exploration-tooling
distrobox enter rocker -- Rscript -e '
suppressPackageStartupMessages(source("R/load_all.R"))
fc  <- hub_read_forecasts()
tr  <- hub_read_truth()
cal <- calibrate_hub_forecasts(fc, tr,
         burn_in_seasons = "2023-2024",
         season_policy   = "carry",     # or "reset" / "shrink"
         lr_window       = 50,          # Inf is an expanding window
         lr_args         = list(mult = 0.1))
print(as.data.frame(hub_coverage_summary(cal)))
print(as.data.frame(hub_quantile_loss(cal)))
saveRDS(cal, "cache/calibration/cal.rds")
'
```

Takes ~5 s for all 265 (location, horizon) series. Other entry points:

- `hub_coverage(cal, by = "location")` — per-location coverage, for the fan plot
- `hub_rolling_tradeoff(cal, window = 20)` — rolling calibration error vs quantile loss
- `cal$series` — per-series learning-rate trajectory and reveal counts
- `cal$rounds` — the global round axis with season labels, burn-in flags, `hidden_scale`

## 4. The EDA notebook

```sh
cd ~/repos/delphi/exploration-tooling
distrobox enter rocker -- Rscript -e \
  'rmarkdown::render("scripts/reports/calibration_qt.Rmd", output_file = "calibration_qt.html")'
```

Output: `scripts/reports/calibration_qt.html`. Parameters (`burn_in_seasons`,
`season_policy`, `lr_window`, `lr_mult`, `roll_window`) are in the YAML header;
override per-render with:

```sh
distrobox enter rocker -- Rscript -e '
rmarkdown::render("scripts/reports/calibration_qt.Rmd",
  output_file = "calibration_qt_mult003.html",
  params = list(lr_mult = 0.03, season_policy = "reset"))'
```

It renders the settings sweep itself, so it takes ~5 min (32 full calibration runs).

## 5. The gallery notebook

Fixed operating point (`lr_mult = 0.03`, `lr_window = 20`, carry) plus a
ranked per-forecast panel gallery; see `notes/CALIBRATION.md` for the design.

```sh
cd ~/repos/delphi/exploration-tooling
distrobox enter rocker -- Rscript -e \
  'rmarkdown::render("scripts/reports/calibration_qt_gallery.Rmd", output_file = "calibration_qt_gallery.html")'
```

As-of vintages come from `cache/calibration/nhsn_archive_flu.parquet` (a copy
of an oracle-capture NHSN archive; provenance and staleness caveats in
`notes/CALIBRATION.md`). `n_panels` is a YAML param (default 150).

---

## Porting gotcha worth knowing about

The one bug hit in the port itself, invisible by construction: numpy
broadcasts `Y - Yhat` along the **last** axis, R recycles a vector
**column-major down** the matrix. Getting it wrong transposes the outcome onto
the level axis and silently corrupts every learning rate while leaving the
algorithm's shape intact. `test-qt.R` pins `eta` against a hand-rolled residual
matrix to catch it — see "the adaptive lr is the 90th percentile of |Y - Yhat|".
