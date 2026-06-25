# /// script
# requires-python = ">=3.11"
# dependencies = ["polars>=1.0", "numpy", "epidatpy", "pandas", "pyarrow"]
# ///
"""
Cheap probe, part 2: the `climate_linear` ENSEMBLE -- the meta-forecaster case.
Compare against:
  R/forecasters/ensemble_linear_climate.R   (ensemble_climate_linear + weight fns)
  R/forecasters/climatological_model.R      (climate_base / climate_geo_agged)
  R/forecasters/forecaster_baseline_linear.R(linear)
  scripts/flu_hosp_prod.R                    (ensemble_clim_lin target)

The question this probe answers: does the "forecaster that takes forecasts as
input" case stay clean as a plain function, or does it need special machinery?
Answer (see ensemble_climate_linear below): it's just a frame of component
forecasts in, a frame out -- the only real content is the weight tables. No
framework, no different abstraction from the leaf forecasters.

Run:  uv run _local/py-probe/climate_linear_ensemble.py
"""

from __future__ import annotations

from datetime import date, timedelta

import numpy as np
import polars as pl
from epidatpy import EpiDataContext

from common import (
    QUANTILE_LEVELS,
    add_season_info,
    epiweek,
    fetch,
    reference_saturday,
    sort_by_quantile,
)

OUT_COLS = ["geo_value", "forecast_date", "target_end_date", "quantile", "value"]


def _emp_quantiles(values: np.ndarray) -> np.ndarray:
    # R uses type=8; numpy's "median_unbiased" is the equivalent.
    return np.quantile(values, QUANTILE_LEVELS, method="median_unbiased")


# ----------------------------------------------------------------------------
# Component 1: climatological model (R: climatological_model). Forecast = the
# empirical quantiles of historical values near this epiweek (a seasonal
# window), either per-geo or pooled across geos. No regression at all.
# ----------------------------------------------------------------------------
def climatological_model(
    df: pl.DataFrame, ahead_weeks: int, as_of: date,
    geo_agg: bool = False, window_size: int = 3, recent_window: int = 3, floor_value: float = 0.0,
) -> pl.DataFrame:
    df = add_season_info(df)
    target_week = epiweek(as_of) + ahead_weeks
    last_date = df["time_value"].max()
    near = (
        df.filter(
            pl.col("value").is_not_null(),
            ~pl.col("season").is_in(["2020/21", "2021/22"]),
            ((pl.col("epiweek") - target_week).abs() <= window_size)
            | ((pl.lit(last_date) - pl.col("time_value")).dt.total_days() <= recent_window * 7),
        )
    )
    ted = reference_saturday(as_of) + timedelta(days=ahead_weeks * 7)

    if geo_agg:  # one pooled quantile set, broadcast to every geo
        qs = np.maximum(floor_value, _emp_quantiles(near["value"].to_numpy()))
        geos = near["geo_value"].unique().to_list()
        out = pl.DataFrame({"geo_value": geos}).join(
            pl.DataFrame({"quantile": QUANTILE_LEVELS, "value": qs}), how="cross"
        )
    else:  # per-geo empirical quantiles
        parts = []
        for (g,), sub in near.group_by("geo_value"):
            qs = np.maximum(floor_value, _emp_quantiles(sub["value"].to_numpy()))
            parts.append(pl.DataFrame({"geo_value": g, "quantile": QUANTILE_LEVELS, "value": qs}))
        out = pl.concat(parts)
    return out.with_columns(forecast_date=pl.lit(as_of), target_end_date=pl.lit(ted)).select(OUT_COLS)


# ----------------------------------------------------------------------------
# Component 2: linear baseline (R: forecaster_baseline_linear, no_intercept).
# Per geo, regress the last 30 days through the last observed value, extrapolate
# `ahead+latency` weeks; spread = pooled, trimmed, symmetrized residuals
# propagated as a random walk of (ahead+2) steps (R uses epipredict's
# propagate_samples; this is a faithful Monte-Carlo stand-in).
# ----------------------------------------------------------------------------
def linear_baseline(
    df: pl.DataFrame, ahead_weeks: int, as_of: date,
    n_train_days: int = 30, residual_center: float = 0.085, residual_tail: float = 0.85, nsim: int = 10_000,
) -> pl.DataFrame:
    max_time = df["time_value"].max()
    latency = int(np.ceil((as_of - max_time).days / 7))
    train = df.filter(pl.col("time_value") >= max_time - timedelta(days=n_train_days), pl.col("value").is_not_null())

    points, resids = [], []
    for (g,), sub in train.group_by("geo_value"):
        sub = sub.sort("time_value")
        if sub.height < 2:
            continue
        last_val = sub["value"][-1]
        weeks_back = (sub["time_value"] - max_time).dt.total_days().to_numpy() / 7.0
        y = sub["value"].to_numpy() - last_val  # regress (value - last) through origin
        denom = np.sum(weeks_back**2)
        slope = np.sum(weeks_back * y) / denom if denom > 0 else 0.0
        resids.append(y - slope * weeks_back)
        points.append((g, last_val + slope * (ahead_weeks + latency)))

    resid = np.concatenate(resids)
    resid = np.concatenate([resid, -resid])
    a = np.abs(resid)
    lo, hi = np.quantile(a, residual_center), np.quantile(a, residual_tail)
    a = a[(a > lo) & (a < hi)]
    resid = np.concatenate([a, -a])

    rng = np.random.default_rng(0)
    walk = rng.choice(resid, size=(nsim, ahead_weeks + 2)).sum(axis=1)
    ted = reference_saturday(as_of) + timedelta(days=ahead_weeks * 7)
    parts = []
    for g, point in points:
        qs = np.maximum(0.0, _emp_quantiles(point + walk))
        parts.append(pl.DataFrame({"geo_value": g, "quantile": QUANTILE_LEVELS, "value": qs}))
    return pl.concat(parts).with_columns(forecast_date=pl.lit(as_of), target_end_date=pl.lit(ted)).select(OUT_COLS)


# ----------------------------------------------------------------------------
# Weight tables. Climate weight rises with horizon (linear at short aheads,
# climate at long) and rises into the tails (linear near the median, climate in
# the extremes). R: make_ahead_weights / make_quantile_weights.
# ----------------------------------------------------------------------------
def _ahead_weights(aheads: list[int], min_w: float, max_w: float) -> pl.DataFrame:
    aheads = sorted(aheads)
    vals = np.linspace(min_w, max_w, len(aheads))
    return pl.concat([
        pl.DataFrame({"family": "climate", "ahead": aheads, "w_ahead": vals}),
        pl.DataFrame({"family": "linear", "ahead": aheads, "w_ahead": 1 - vals}),
    ])


def _quantile_weights(probs: list[float], min_w: float, max_w: float) -> pl.DataFrame:
    vals = [min_w + (max_w - min_w) * abs(q - 0.5) * 2 for q in probs]
    return pl.concat([
        pl.DataFrame({"family": "climate", "quantile": probs, "w_q": vals}),
        pl.DataFrame({"family": "linear", "quantile": probs, "w_q": [1 - v for v in vals]}),
    ])


# ----------------------------------------------------------------------------
# The meta-forecaster (R: ensemble_climate_linear). Frame of component forecasts
# in, ensembled frame out. The whole body is: build per-(family,ahead,quantile)
# weights, map family->forecaster, renormalize, weighted-sum. Same shape as a
# leaf forecaster -- nothing special about consuming forecasts as input.
# ----------------------------------------------------------------------------
def ensemble_climate_linear(
    forecasts: pl.DataFrame, aheads: list[int], probs: list[float] = QUANTILE_LEVELS,
    min_climate_ahead: float = 0.05, max_climate_ahead: float = 0.90,
    min_climate_quantile: float = 0.1, max_climate_quantile: float = 1.0,
) -> pl.DataFrame:
    last_data = forecasts["target_end_date"].min()
    forecast_date = forecasts["forecast_date"].min()
    latency = int(np.ceil((forecast_date - last_data).days / 7))
    aheads = list(range(min(aheads), max(aheads) + latency + 1))

    fc = forecasts.filter(pl.col("forecaster").str.contains("climate|linear")).with_columns(
        forecast_date=pl.lit(last_data)
    )
    forecasters = fc["forecaster"].unique().to_list()
    fam_map = pl.DataFrame(
        {"forecaster": forecasters, "family": ["climate" if "climate" in f else "linear" for f in forecasters]}
    )

    weights = (
        _ahead_weights(aheads, min_climate_ahead, max_climate_ahead)
        .join(_quantile_weights(probs, min_climate_quantile, max_climate_quantile), on="family")
        .with_columns(weight=pl.col("w_ahead") * pl.col("w_q"))
        .join(fam_map, on="family")
        .select("forecaster", "ahead", "quantile", "weight")
        # renormalize so weights sum to 1 within each (ahead, quantile)
        .with_columns(weight=pl.col("weight") / pl.col("weight").sum().over(["ahead", "quantile"]))
    )

    return (
        fc.with_columns(
            quantile=pl.col("quantile").round(3),
            ahead=((pl.col("target_end_date") - pl.col("forecast_date")).dt.total_days() // 7).cast(pl.Int64),
        )
        .join(weights, on=["forecaster", "ahead", "quantile"])
        .with_columns(value=pl.col("weight") * pl.col("value"))
        .group_by("geo_value", "forecast_date", "target_end_date", "quantile")
        .agg(value=pl.col("value").sum())
        .pipe(sort_by_quantile)
        .with_columns(forecaster=pl.lit("climate_linear"))
    )


# ----------------------------------------------------------------------------
def _median(fc: pl.DataFrame, geos=("ca", "ny", "tx")) -> str:
    m = fc.filter((pl.col("quantile") == 0.5) & pl.col("geo_value").is_in(list(geos))).sort("geo_value")
    return "  ".join(f"{r['geo_value']}={r['value']:.1f}" for r in m.to_dicts())


def main() -> None:
    ctx = EpiDataContext()
    nhsn = fetch(ctx, "nhsn", "confirmed_admissions_flu_ew", "value", "2022-01-01", "2026-06-01")
    as_of = nhsn["time_value"].max()
    print(f"NHSN flu adm: {nhsn.shape[0]} rows, {nhsn['geo_value'].n_unique()} geos, as_of={as_of}\n")

    aheads = [0, 1, 2, 3]
    components = pl.concat([
        f(nhsn, a, as_of).with_columns(forecaster=pl.lit(name))
        for a in aheads
        for name, f in [
            ("climate_base", climatological_model),
            ("climate_geo_agged", lambda d, a, o: climatological_model(d, a, o, geo_agg=True)),
            ("linear", linear_baseline),
        ]
    ])
    ensemble = ensemble_climate_linear(components, aheads)

    # Show the ensemble sitting between its components (median, ca/ny/tx).
    for a in aheads:
        ted = reference_saturday(as_of) + timedelta(days=a * 7)
        sel = lambda name: components.filter(pl.col("forecaster") == name, pl.col("target_end_date") == ted)
        print(f"ahead={a}w  target={ted}")
        print(f"   climate_base : {_median(sel('climate_base'))}")
        print(f"   linear       : {_median(sel('linear'))}")
        print(f"   => ensemble  : {_median(ensemble.filter(pl.col('target_end_date') == ted))}")

    # Confirm the tails widen as designed: climate weight grows into the tails.
    print("\n90% interval width (ca, ahead=3w): climate vs linear vs ensemble")
    ted = reference_saturday(as_of) + timedelta(days=3 * 7)
    for label, fc in [
        ("climate_base", components.filter(pl.col("forecaster") == "climate_base")),
        ("linear", components.filter(pl.col("forecaster") == "linear")),
        ("ensemble", ensemble),
    ]:
        ca = fc.filter(pl.col("geo_value") == "ca", pl.col("target_end_date") == ted)
        width = (
            ca.filter(pl.col("quantile") == 0.95)["value"][0] - ca.filter(pl.col("quantile") == 0.05)["value"][0]
        )
        print(f"   {label:13s}: {width:.1f}")


if __name__ == "__main__":
    main()
