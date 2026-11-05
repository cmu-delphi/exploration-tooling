"""Shared toolkit for the forecaster probes. Imported, not run directly --
its dependencies are supplied by whichever script's PEP-723 header runs it
(see windowed_seasonal.py / climate_linear_ensemble.py).

The bet behind the rewrite is that a forecaster repo needs a *small* shared
toolkit (season bookkeeping, a data fetch, quantile conventions) and then each
forecaster is a plain function on top of it -- no recipe DSL, no metaprogramming.
This file is that toolkit.
"""

from __future__ import annotations

from datetime import date, timedelta

import polars as pl
from epidatpy import EpiDataContext, EpiRange

# The covidhub quantile levels (R: covidhub_probs()).
QUANTILE_LEVELS = [0.01, 0.025, *[round(0.05 * i, 3) for i in range(1, 20)], 0.975, 0.99]


def epiweek(d: date) -> int:
    """MMWR week, approximated by ISO week (agrees within a week)."""
    return d.isocalendar().week


def epiweek_to_season_week(iso_week: int) -> int:
    """Weeks since the season start at epiweek 40 (R: convert_epiweek_to_season_week)."""
    return iso_week - 39 if iso_week >= 40 else iso_week + 52 - 39


def reference_saturday(d: date) -> date:
    """The Saturday ending d's epiweek (R: get_forecast_reference_date)."""
    return d + timedelta(days=(5 - d.weekday()) % 7)


def add_season_info(df: pl.DataFrame) -> pl.DataFrame:
    """Add epiyear/epiweek/season/season_week (R: add_season_info)."""
    iso = df["time_value"].dt.iso_year().rename("epiyear")
    wk = df["time_value"].dt.week().rename("epiweek")
    df = df.with_columns(iso, wk)
    season_start_year = pl.when(pl.col("epiweek") <= 39).then(pl.col("epiyear") - 1).otherwise(pl.col("epiyear"))
    season_week = pl.when(pl.col("epiweek") >= 40).then(pl.col("epiweek") - 39).otherwise(pl.col("epiweek") + 52 - 39)
    return df.with_columns(
        season=season_start_year.cast(pl.Utf8) + "/" + (season_start_year + 1).cast(pl.Utf8).str.slice(2, 2),
        season_week=season_week.cast(pl.Int32),
    )


def sort_by_quantile(forecasts: pl.DataFrame) -> pl.DataFrame:
    """Force monotone quantiles within each (geo, forecast_date, target) cell."""
    return forecasts.sort("quantile").with_columns(
        value=pl.col("value").sort().over(["geo_value", "forecast_date", "target_end_date"])
    )


def fetch(ctx: EpiDataContext, source: str, signal: str, value_name: str, start: str, end: str) -> pl.DataFrame:
    """Latest snapshot of a cast-api signal as a tidy (geo, week, value) frame."""
    pdf = ctx.epidata_snapshot(
        source=source, signals=signal, geo_type="state", geo_values="*",
        reference_time=EpiRange(start, end),
    ).df()
    return (
        pl.from_pandas(pdf[["geo_value", "reference_time", "value"]])
        .rename({"reference_time": "time_value", "value": value_name})
        .with_columns(
            time_value=pl.col("time_value").cast(pl.Date),
            geo_value=pl.col("geo_value").str.to_lowercase(),
            source=pl.lit("nhsn"),  # single nominal source; whitening keys on it
        )
        .drop_nulls(value_name)
        .group_by("geo_value", "time_value", "source").agg(pl.col(value_name).last())
    )
