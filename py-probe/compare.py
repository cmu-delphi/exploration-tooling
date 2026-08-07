# /// script
# requires-python = ">=3.11"
# dependencies = ["polars>=1.0", "scikit-learn>=1.4", "numpy", "epidatpy", "pandas", "pyarrow"]
# ///
"""
Bake-off comparison. Runs the Python forecasters on the SAME canonical snapshot
the R script used (py-probe/nhsn_input.csv), loads the R outputs, and reports
per-forecaster agreement.

Pipeline:
  1. uv run py-probe/dump_inputs.py            # write canonical csv
  2. distrobox enter rocker -- Rscript py-probe/bakeoff.R   # R forecasts -> r_*.csv
  3. uv run py-probe/compare.py                # this script

Aligns by (forecaster, geo_value, quantile, horizon) where horizon is the rank
of target_end_date within a forecaster -- robust to date-anchoring plumbing.
"""

from __future__ import annotations

import polars as pl

from climate_linear_ensemble import climatological_model, ensemble_climate_linear, linear_baseline
from windowed_seasonal import windowed_seasonal

AHEADS = [0, 1, 2, 3]


def with_horizon(df: pl.DataFrame) -> pl.DataFrame:
    """Map each (forecaster, geo) series' target_end_dates to a 0-based horizon.
    Per-geo because windowed_seasonal anchors on each geo's latest in-window row,
    so target dates can differ across geos."""
    return df.with_columns(
        horizon=pl.col("target_end_date").rank("dense").over("forecaster", "geo_value").cast(pl.Int64) - 1
    )


def run_python(nhsn: pl.DataFrame) -> pl.DataFrame:
    as_of = nhsn["time_value"].max()
    frames = []
    for a in AHEADS:
        frames.append(windowed_seasonal(nhsn, a, as_of).with_columns(forecaster=pl.lit("windowed_seasonal")))
        frames.append(climatological_model(nhsn, a, as_of).with_columns(forecaster=pl.lit("climate_base")))
        frames.append(
            climatological_model(nhsn, a, as_of, geo_agg=True).with_columns(forecaster=pl.lit("climate_geo_agged"))
        )
        frames.append(linear_baseline(nhsn, a, as_of).with_columns(forecaster=pl.lit("linear")))
    components = pl.concat([f.select("forecaster", "geo_value", "target_end_date", "quantile", "value") for f in frames])
    ens = ensemble_climate_linear(
        components.filter(pl.col("forecaster").str.contains("climate|linear")).with_columns(
            forecast_date=pl.lit(as_of)
        ),
        AHEADS,
    ).select("forecaster", "geo_value", "target_end_date", "quantile", "value")
    return with_horizon(pl.concat([components, ens]))


def main() -> None:
    nhsn = (
        pl.read_csv("py-probe/nhsn_input.csv", try_parse_dates=True)
        .select("geo_value", "time_value", "value")
        .with_columns(source=pl.lit("nhsn"))
    )

    py = run_python(nhsn)
    r = with_horizon(
        pl.concat([
            pl.read_csv("py-probe/r_windowed_seasonal.csv", try_parse_dates=True),
            pl.read_csv("py-probe/r_climate_linear.csv", try_parse_dates=True),
        ]).select("forecaster", "geo_value", "target_end_date", "quantile", "value")
    )

    joined = py.join(
        r, on=["forecaster", "geo_value", "quantile", "horizon"], how="inner", suffix="_r"
    ).with_columns(
        abs_err=(pl.col("value") - pl.col("value_r")).abs(),
        rel_err=(pl.col("value") - pl.col("value_r")).abs()
        / (0.5 * (pl.col("value").abs() + pl.col("value_r").abs()) + 1.0),
    )

    print(f"matched rows: {joined.height}  (py={py.height}, r={r.height})\n")
    print(f"{'forecaster':18s} {'n':>5s} {'med|Δ|':>8s} {'p90|Δ|':>8s} {'max|Δ|':>9s} {'med rel':>8s}")
    summary = (
        joined.group_by("forecaster")
        .agg(
            n=pl.len(),
            med_abs=pl.col("abs_err").median(),
            p90_abs=pl.col("abs_err").quantile(0.90),
            max_abs=pl.col("abs_err").max(),
            med_rel=pl.col("rel_err").median(),
        )
        .sort("forecaster")
    )
    for row in summary.to_dicts():
        print(
            f"{row['forecaster']:18s} {row['n']:5d} {row['med_abs']:8.2f} "
            f"{row['p90_abs']:8.2f} {row['max_abs']:9.2f} {row['med_rel']:8.3f}"
        )

    # Concrete side-by-side: ca median across horizons.
    print("\nca median, python vs R (value | value_r):")
    ca = (
        joined.filter((pl.col("geo_value") == "ca") & (pl.col("quantile") == 0.5))
        .sort("forecaster", "horizon")
        .select("forecaster", "horizon", "value", "value_r")
    )
    for row in ca.to_dicts():
        print(f"   {row['forecaster']:18s} h{row['horizon']}  {row['value']:8.1f} | {row['value_r']:8.1f}")


if __name__ == "__main__":
    main()
