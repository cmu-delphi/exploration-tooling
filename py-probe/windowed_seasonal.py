# /// script
# requires-python = ">=3.11"
# dependencies = ["polars>=1.0", "scikit-learn>=1.4", "numpy", "epidatpy", "pandas", "pyarrow"]
# ///
"""
Cheap probe: `windowed_seasonal` (+ `_extra_sources`) reimplemented in thin
Python, no pipeline framework. Compare against:
  R/forecasters/forecaster_scaled_pop_seasonal.R   (scaled_pop_seasonal, method="window")
  R/forecasters/data_transforms.R                  (whitening / coloring)
  R/new_epipredict_steps/step_training_window.R    (seasonal training window)
  R/flu_prod_forecasters.R                         (the two wrappers)

The point of the probe is to see whether the *essential* complexity the
maintainers named -- "a forecaster IS its data shaping" and "pooled panel fit,
per-key prediction" -- reads more honestly as plain dataframe code than as a
recipe/frosting DSL with whitening bolted on either side.

Data comes live from cast-api via epidatpy (cmu-delphi/epidatpy):
  outcome     = nhsn / confirmed_admissions_flu_ew   (R: get_nhsn_data_archive)
  extra source= nssp / pct_ed_visits_influenza       (R: up_to_date_nssp_state_archive)
We use latest snapshots (epidata_snapshot) rather than the full version archive --
a real backtest would pull epidata_archive and slice as_of, but for a single
forecast date the latest snapshot is the right input.

Run:  uv run _local/py-probe/windowed_seasonal.py
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date, timedelta

import numpy as np
import polars as pl
from epidatpy import EpiDataContext
from sklearn.linear_model import QuantileRegressor

from common import QUANTILE_LEVELS, add_season_info, fetch


# ----------------------------------------------------------------------------
# Whitening. Per (source, geo_value): quarter-root, then center on the median,
# then scale so the 5th-95th quantile span is 1. `WhitenParams` is just the
# fitted state; `color` is its exact inverse. This is the transform epipredict
# can't express, which is why the R code reaches around the recipe to do it.
# ----------------------------------------------------------------------------
@dataclass
class WhitenParams:
    cols: list[str]
    table: pl.DataFrame  # (source, geo_value, {col}_center, {col}_scale) per col


def fit_whiten(df: pl.DataFrame, cols: list[str]) -> WhitenParams:
    rooted = df.with_columns([(pl.col(c) + 0.01).pow(0.25) for c in cols])
    aggs = []
    for c in cols:
        aggs.append(pl.col(c).median().alias(f"{c}_center"))
        aggs.append(
            (pl.col(c).quantile(0.95) - pl.col(c).quantile(0.05) + 0.01).alias(f"{c}_scale")
        )
    table = rooted.group_by("source", "geo_value").agg(aggs)
    return WhitenParams(cols, table)


def whiten(df: pl.DataFrame, p: WhitenParams) -> pl.DataFrame:
    out = df.join(p.table, on=["source", "geo_value"], how="left")
    out = out.with_columns([(pl.col(c) + 0.01).pow(0.25) for c in p.cols])
    out = out.with_columns([((pl.col(c) - pl.col(f"{c}_center")) / pl.col(f"{c}_scale")) for c in p.cols])
    return out.drop([f"{c}_center" for c in p.cols] + [f"{c}_scale" for c in p.cols])


def color(df: pl.DataFrame, p: WhitenParams, col: str) -> pl.DataFrame:
    """Invert whitening for a single predicted column, joining params by geo."""
    tbl = p.table.filter(pl.col("source") == "nhsn").select("geo_value", f"{col}_center", f"{col}_scale")
    out = df.join(tbl, on="geo_value", how="left")
    out = out.with_columns((pl.col(col) * pl.col(f"{col}_scale") + pl.col(f"{col}_center")))
    out = out.with_columns((pl.col(col).pow(4) - 0.01))
    return out.drop(f"{col}_center", f"{col}_scale")


# ----------------------------------------------------------------------------
# Design matrix. step_epi_lag/step_epi_ahead, but as explicit date-joins so gaps
# in the weekly series don't silently shift a lag. `ahead` and `lags` are in
# DAYS (the R wrappers pass ahead*7 and lags of c(0,7)) -- the data is weekly so
# a 7-day lag is "last week".
# ----------------------------------------------------------------------------
def build_design(df: pl.DataFrame, outcome: str, predictors: dict[str, list[int]], ahead: int) -> pl.DataFrame:
    keys = ["geo_value", "source", "time_value"]
    out = df.select(keys)
    feature_cols: list[str] = []
    for col, lags in predictors.items():
        for lag in lags:
            name = f"{col}_lag{lag}"
            feature_cols.append(name)
            shifted = df.select(
                pl.col("geo_value"),
                pl.col("source"),
                (pl.col("time_value") + timedelta(days=lag)).alias("time_value"),
                pl.col(col).alias(name),
            )
            out = out.join(shifted, on=keys, how="left")
    # target = outcome `ahead` days in the future
    target = df.select(
        pl.col("geo_value"),
        pl.col("source"),
        (pl.col("time_value") - timedelta(days=ahead)).alias("time_value"),
        pl.col(outcome).alias("target"),
    )
    out = out.join(target, on=keys, how="left")
    return out, feature_cols


# ----------------------------------------------------------------------------
# Seasonal training window (R: step_epi_training_window, seasonal=TRUE).
# Anchor on the season_week of the as_of date; collect every calendar date in
# any year that lands on that season_week; keep rows within
# [-backward, +forward] days of those anchors. This is how the model borrows
# the *same phase* of previous seasons.
# ----------------------------------------------------------------------------
def seasonal_window(df: pl.DataFrame, as_of: date, backward_days: int, forward_days: int) -> pl.DataFrame:
    df = add_season_info(df)
    iso_week = as_of.isocalendar().week
    current_sw = iso_week - 39 if iso_week >= 40 else iso_week + 52 - 39
    anchors = df.filter(pl.col("season_week") == current_sw)["time_value"].unique().to_list()
    keep = set()
    for a in anchors:
        a = a if isinstance(a, date) else a.date()
        for d in range(-backward_days, forward_days + 1):
            keep.add(a + timedelta(days=d))
    return df.filter(pl.col("time_value").is_in(list(keep)))


# ----------------------------------------------------------------------------
# Pooled quantile regression: ONE model across all geos (the panel fit), one
# coefficient set per quantile level, then predict each geo's latest row. This
# pooled-fit / per-key-predict shape is exactly what tidymodels can't do and
# epipredict was built to provide -- here it's a groupby and a loop.
# ----------------------------------------------------------------------------
def fit_predict_quantiles(train: pl.DataFrame, test: pl.DataFrame, features: list[str]) -> pl.DataFrame:
    tr = train.drop_nulls(features + ["target"])
    X, y = tr.select(features).to_numpy(), tr["target"].to_numpy()
    Xt = test.select(features).to_numpy()
    preds = np.empty((len(test), len(QUANTILE_LEVELS)))
    for j, tau in enumerate(QUANTILE_LEVELS):
        model = QuantileRegressor(quantile=tau, alpha=0.0, solver="highs")
        model.fit(X, y)
        preds[:, j] = model.predict(Xt)
    preds.sort(axis=1)  # enforce monotone quantiles (no crossing)
    long = []
    base = test.select("geo_value", "source", "time_value")
    for j, tau in enumerate(QUANTILE_LEVELS):
        long.append(base.with_columns(quantile=pl.lit(tau), value=pl.Series(preds[:, j])))
    return pl.concat(long)


# ----------------------------------------------------------------------------
# The forecaster. Everything above is shared; this is the whole "model": shape,
# whiten, window, fit, color, label. ~20 lines, and the data-shaping IS visible
# inline rather than smeared across recipe steps + pre/post hooks.
# ----------------------------------------------------------------------------
# NOTE (bake-off finding): this port does NOT implement epipredict's
# step_adjust_latency. Production runs scaled_pop_seasonal with the default
# adjust_latency="extend_lags", which extends the lags by the data latency. With
# normal ~1wk latency the effect is small, but the bake-off (compare.py) showed
# it dominates when latency is large. Ports match R to ~1% only with
# adjust_latency="none"; faithfully porting extend_lags is the open work item.
def windowed_seasonal(
    df: pl.DataFrame,
    ahead_weeks: int,
    as_of: date,
    extra_source: str | None = None,
    backward_days: int = 5 * 7,
    forward_days: int = 3 * 7,
) -> pl.DataFrame:
    ahead = ahead_weeks * 7
    predictors = {"value": [0, 7]}
    cols_to_whiten = ["value"]
    if extra_source is not None:
        predictors[extra_source] = [0, 7]
        cols_to_whiten.append(extra_source)

    params = fit_whiten(df, cols_to_whiten)
    whitened = whiten(df, params)

    design, features = build_design(whitened, "value", predictors, ahead)
    design = seasonal_window(design, as_of, backward_days, forward_days + ahead)

    # test rows = latest available row per geo (what we forecast from)
    latest = design.group_by("geo_value").agg(pl.col("time_value").max().alias("time_value"))
    test = design.join(latest, on=["geo_value", "time_value"], how="semi").drop_nulls(features)

    pred = fit_predict_quantiles(design, test, features)
    pred = color(pred, params, "value")
    return (
        pred.with_columns(
            forecast_date=pl.lit(as_of),
            target_end_date=pl.col("time_value") + timedelta(days=ahead),
            value=pl.col("value").clip(lower_bound=0),
        )
        .select("geo_value", "forecast_date", "target_end_date", "quantile", "value")
    )


def windowed_seasonal_extra_sources(nhsn: pl.DataFrame, extra: pl.DataFrame, ahead_weeks: int, as_of: date):
    """R wrapper: left_join the extra source onto nhsn, then run the window model
    with it as a second predictor. The join is the forecaster-defining step."""
    joined = nhsn.join(extra.select("geo_value", "time_value", "nssp"), on=["geo_value", "time_value"], how="left")
    return windowed_seasonal(joined, ahead_weeks, as_of, extra_source="nssp")


def main() -> None:
    ctx = EpiDataContext()
    nhsn = fetch(ctx, "nhsn", "confirmed_admissions_flu_ew", "value", "2022-01-01", "2026-06-01")
    nssp = fetch(ctx, "nssp", "pct_ed_visits_influenza", "nssp", "2022-01-01", "2026-06-01")

    as_of = nhsn["time_value"].max()
    print(f"NHSN flu adm: {nhsn.shape[0]} rows, {nhsn['geo_value'].n_unique()} geos, as_of={as_of}")
    print(f"NSSP %ed flu: {nssp.shape[0]} rows, {nssp['geo_value'].n_unique()} geos\n")

    for ahead in range(0, 4):
        fc = windowed_seasonal(nhsn, ahead, as_of)
        med = fc.filter((pl.col("quantile") == 0.5) & (pl.col("geo_value").is_in(["ca", "ny", "tx"])))
        print(f"ahead={ahead}w  target={fc['target_end_date'][0]}  median forecast (ca/ny/tx):")
        print("   " + "  ".join(f"{r['geo_value']}={r['value']:.1f}" for r in med.sort("geo_value").to_dicts()))

    print("\n--- extra_sources path (real nssp as second predictor) ---")
    for ahead in range(0, 4):
        fc = windowed_seasonal_extra_sources(nhsn, nssp, ahead_weeks=ahead, as_of=as_of)
        med = fc.filter((pl.col("quantile") == 0.5) & (pl.col("geo_value").is_in(["ca", "ny", "tx"])))
        print(f"ahead={ahead}w  target={fc['target_end_date'][0]}  median (ca/ny/tx):")
        print("   " + "  ".join(f"{r['geo_value']}={r['value']:.1f}" for r in med.sort("geo_value").to_dicts()))


if __name__ == "__main__":
    main()
