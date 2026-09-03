# /// script
# requires-python = ">=3.11"
# dependencies = ["timesfm[torch]", "torch", "numpy", "pandas", "pyarrow", "duckdb", "typer"]
#
# [tool.uv.sources]
# timesfm = { path = "../../../../third-party/timesfm" }
# ///
"""Backtest TimesFM 3.0 on the weekly flu data pool dumped by dump_data_pool.R.

Builds, per scheduled forecast date, an as-of snapshot of the archive and feeds
it to TimesFM3 as one or more contexts (univariate / geo_multivariate / panel),
then reshapes the model's 9 quantiles into the 23-level output schema used by
the R forecasters (r_forecasts.parquet).
"""

from __future__ import annotations

import statistics
from datetime import date, timedelta
from pathlib import Path

import duckdb
import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import typer

app = typer.Typer(add_completion=False)

QUANTILE_LEVELS = (
    [0.01, 0.025] + [round(0.05 * i, 3) for i in range(1, 20)] + [0.975, 0.99]
)
MODEL_QUANTILES = [round(0.1 * i, 1) for i in range(1, 10)]  # 0.1 .. 0.9


def expand_quantiles(q9: np.ndarray) -> np.ndarray:
    """Expand 9 model quantiles (levels 0.1..0.9) into the 23 required output levels.

    Interior levels are linear interpolation in probability; tail levels (below
    0.1 and above 0.9) are normal-tail extrapolation anchored on the median and
    the outer decile. Input shape (..., 9) -> output shape (..., 23).
    """
    q9 = np.asarray(q9, dtype=np.float64)
    nd = statistics.NormalDist()
    z90 = nd.inv_cdf(0.9)
    z10 = nd.inv_cdf(0.1)

    flat = q9.reshape(-1, 9)
    out = np.empty((flat.shape[0], len(QUANTILE_LEVELS)), dtype=np.float64)
    for i, row in enumerate(flat):
        q50 = np.interp(0.5, MODEL_QUANTILES, row)
        q90 = row[-1]
        q10 = row[0]
        for j, p in enumerate(QUANTILE_LEVELS):
            if p > 0.9:
                zp = nd.inv_cdf(p)
                out[i, j] = q50 + (q90 - q50) * zp / z90
            elif p < 0.1:
                zp = nd.inv_cdf(p)
                out[i, j] = q50 + (q10 - q50) * zp / z10
            else:
                out[i, j] = np.interp(p, MODEL_QUANTILES, row)
    out = np.clip(out, 0, None)
    out.sort(axis=1)
    return out.reshape(q9.shape[:-1] + (len(QUANTILE_LEVELS),))


def wednesday_of_week(d: date) -> date:
    """Floor a date to the Sunday that starts its week, then shift to Wednesday."""
    sunday = d - timedelta(days=(d.weekday() + 1) % 7)
    return sunday + timedelta(days=3)


def load_substitutions(path: Path) -> pd.DataFrame:
    """Read the manual nhsn data-correction csv, stripping whitespace from cells."""
    if not path.exists():
        return pd.DataFrame(columns=["geo_value", "forecast_date", "time_value", "value"])
    df = pd.read_csv(path, comment="#", skipinitialspace=True)
    df.columns = [c.strip() for c in df.columns]
    for c in ["geo_value"]:
        df[c] = df[c].astype(str).str.strip()
    df["forecast_date"] = pd.to_datetime(df["forecast_date"]).dt.date
    df["time_value"] = pd.to_datetime(df["time_value"]).dt.date
    df["time_value"] = df["time_value"].map(wednesday_of_week)
    return df.dropna(subset=["value"])


def snapshot_as_of(archive_path: Path, sources: list[str], gen_date: date) -> pd.DataFrame:
    """Pull the as-of snapshot (latest value per key as of gen_date) via DuckDB."""
    con = duckdb.connect()
    df = con.execute(
        """
        SELECT geo_value, source, time_value, arg_max(value, version) AS value
        FROM read_parquet(?)
        WHERE source IN (SELECT UNNEST(?)) AND version <= ? AND time_value <= ?
        GROUP BY 1, 2, 3
        """,
        [str(archive_path), sources, gen_date, gen_date],
    ).df()
    con.close()
    return df.dropna(subset=["value"])


def apply_substitutions(snap: pd.DataFrame, subs: pd.DataFrame, gen_date: date) -> pd.DataFrame:
    """Inner-join replace nhsn values for the rows the csv corrects at this generation_date."""
    rows = subs[subs["forecast_date"] == gen_date]
    if rows.empty:
        return snap
    snap = snap.copy()
    key = ["geo_value", "time_value"]
    is_nhsn = snap["source"] == "nhsn"
    idx = snap[is_nhsn].set_index(key).index
    sub_idx = rows.set_index(key)["value"]
    common = idx.intersection(sub_idx.index)
    if len(common) == 0:
        return snap
    lookup = sub_idx.loc[common]
    snap = snap.set_index(key)
    for k, v in lookup.items():
        # only touch nhsn rows at this key
        mask = (snap.index == k) & (snap["source"] == "nhsn")
        snap.loc[mask, "value"] = v
    snap = snap.reset_index()
    return snap


def build_series(snap: pd.DataFrame, context_weeks: int) -> dict[tuple[str, str], dict]:
    """Build a regular weekly (NaN-filled) grid per (geo, source), trimmed to context_weeks."""
    series = {}
    snap = snap.copy()
    snap["time_value"] = pd.to_datetime(snap["time_value"]).dt.date
    for (geo, src), g in snap.groupby(["geo_value", "source"]):
        g = g.sort_values("time_value")
        first, last = g["time_value"].iloc[0], g["time_value"].iloc[-1]
        grid = pd.date_range(first, last, freq="7D").date
        vals = pd.Series(np.nan, index=grid)
        vals.loc[g["time_value"].values] = g["value"].values
        grid = grid[-context_weeks:]
        vals = vals.loc[grid]
        series[(geo, src)] = {"grid": list(grid), "values": vals.to_numpy(dtype=np.float64), "t_last": last}
    return series


def steps_ahead(t_last: date, forecast_date: date, ahead: int) -> tuple[date, date, int]:
    """Return (target_wed, target_end_date, steps) for one ahead value."""
    target_wed = forecast_date + timedelta(weeks=ahead)
    target_end = target_wed + timedelta(days=3)
    steps = (target_wed - t_last).days // 7
    return target_wed, target_end, steps


def emit_rows(target: str, forecaster_id: str, geo: str, forecast_date: date,
              aheads: list[int], t_last: date, last_value: float,
              q23_by_step: dict[int, np.ndarray]) -> list[dict]:
    """Build output rows for one (geo, source) series across all aheads."""
    rows = []
    for a in aheads:
        target_wed, target_end, steps = steps_ahead(t_last, forecast_date, a)
        if steps >= 1:
            q23 = q23_by_step[steps]
        else:
            # already-observed week: emit the observed value as a degenerate point forecast
            # (the -1 ahead in this repo is a nowcast of the last reported week)
            q23 = np.full(len(QUANTILE_LEVELS), last_value)
        for lvl, val in zip(QUANTILE_LEVELS, q23):
            rows.append(
                dict(
                    target=target,
                    forecaster=forecaster_id,
                    geo_value=geo,
                    forecast_date=forecast_date,
                    target_end_date=target_end,
                    quantile=lvl,
                    value=float(val),
                )
            )
    return rows


def build_contexts_for_date(
    snap: pd.DataFrame, sources: list[str], mode: str, context_weeks: int,
    forecast_date: date, max_latency_weeks: int,
) -> tuple[list[np.ndarray], list[list[tuple[str, str]]], dict]:
    """Group per-(geo,source) series into model contexts per the chosen mode.

    Returns (contexts, groups, series_by_key) where groups[i] lists the
    (geo, source) keys stacked (in row order) inside contexts[i].
    """
    series = build_series(snap, context_weeks)
    # A series lagging far behind the freshest one (e.g. wy nssp, which stopped
    # reporting) is skipped rather than forecast from stale data or allowed to
    # drag a geo's other series back. Measured against the freshest series, not
    # the forecast date, so a reporting outage that stalls every source (fall
    # 2025) still forecasts from whatever was last published, like prod did.
    if series:
        freshest = max(s["t_last"] for s in series.values())
        stale_cutoff = freshest - timedelta(weeks=max_latency_weeks)
        series = {k: s for k, s in series.items() if s["t_last"] >= stale_cutoff}
    contexts: list[np.ndarray] = []
    groups: list[list[tuple[str, str]]] = []

    if mode == "univariate":
        for key, s in series.items():
            contexts.append(np.asarray(s["values"], dtype=np.float32))
            groups.append([key])
        return contexts, groups, series

    if mode == "geo_multivariate":
        geos = sorted({g for g, _ in series})
        for geo in geos:
            keys = [(geo, src) for src in sources if (geo, src) in series]
            if len(keys) <= 1:
                for key in keys:
                    contexts.append(np.asarray(series[key]["values"], dtype=np.float32))
                    groups.append([key])
                continue
            common_last = min(series[k]["t_last"] for k in keys)
            rows = []
            for k in keys:
                grid = series[k]["grid"]
                vals = series[k]["values"]
                cut = [i for i, d in enumerate(grid) if d <= common_last]
                rows.append(vals[cut[-context_weeks:]] if cut else np.array([np.nan]))
            min_len = min(len(r) for r in rows)
            rows = [r[-min_len:] for r in rows]
            for k in keys:
                series[k]["t_last"] = common_last
            contexts.append(np.stack(rows).astype(np.float32))
            groups.append(keys)
        return contexts, groups, series

    if mode == "panel":
        t_lasts = pd.Series([s["t_last"] for s in series.values()])
        common_last = t_lasts.mode().iloc[0]
        panel_keys, panel_rows = [], []
        for key, s in series.items():
            if s["t_last"] != common_last:
                contexts.append(np.asarray(s["values"], dtype=np.float32))
                groups.append([key])
                continue
            panel_keys.append(key)
            panel_rows.append(s["values"])
        if panel_keys:
            min_len = min(len(r) for r in panel_rows)
            panel_rows = [r[-min_len:] for r in panel_rows]
            contexts.append(np.stack(panel_rows).astype(np.float32))
            groups.append(panel_keys)
        return contexts, groups, series

    raise ValueError(f"unknown mode {mode}")


@app.command()
def main(
    pool: Path = typer.Option(Path("cache/tsfm"), help="Data pool directory"),
    out: Path = typer.Option(Path("cache/tsfm/timesfm_forecasts.parquet")),
    start: str = typer.Option("2025-10-01"),
    end: str = typer.Option("2026-07-08"),
    mode: str = typer.Option("geo_multivariate"),
    sources: str = typer.Option("nhsn,nssp"),
    aheads: str = typer.Option("-1,0,1,2,3"),
    context_weeks: int = typer.Option(200),
    max_latency_weeks: int = typer.Option(6, help="Skip series lagging more than this many weeks behind the freshest one"),
    forecaster_id: str = typer.Option(None),
    device: str = typer.Option("cuda"),
    batch_size: int = typer.Option(32),
    checkpoint: str = typer.Option("google/timesfm-3.0-pytorch"),
    limit_dates: int = typer.Option(0),
    dry_run: bool = typer.Option(False, "--dry-run"),
):
    src_list = [s.strip() for s in sources.split(",")]
    ahead_list = [int(a) for a in aheads.split(",")]
    fid = forecaster_id or f"timesfm3_{mode}"
    start_d, end_d = pd.to_datetime(start).date(), pd.to_datetime(end).date()

    archive_path = pool / "archive.parquet"
    schedule = pd.read_parquet(pool / "forecast_schedule.parquet")
    schedule["forecast_date"] = pd.to_datetime(schedule["forecast_date"]).dt.date
    schedule["generation_date"] = pd.to_datetime(schedule["generation_date"]).dt.date
    schedule = schedule[
        (schedule["forecast_date"] >= start_d) & (schedule["forecast_date"] <= end_d)
    ].sort_values("forecast_date")
    if limit_dates:
        schedule = schedule.head(limit_dates)

    subs = load_substitutions(Path("scripts/flu_data_substitutions.csv"))

    model = None
    if not dry_run:
        # imported lazily so --dry-run needs neither torch nor timesfm3
        import torch  # noqa: F401
        from timesfm3.evaluator import TimesFM3Evaluator
        from timesfm3.timesfm3_forecaster import ModelConfig

        model = TimesFM3Evaluator(
            ModelConfig(checkpoint_path=checkpoint, per_core_batch_size=batch_size, device=device)
        )

    all_rows: list[dict] = []
    for _, row in schedule.iterrows():
        fdate, gdate = row["forecast_date"], row["generation_date"]
        snap = snapshot_as_of(archive_path, src_list, gdate)
        snap = apply_substitutions(snap, subs, gdate)
        if snap.empty:
            typer.echo(f"{fdate}: no data, skipping")
            continue

        contexts, groups, series = build_contexts_for_date(
            snap, src_list, mode, context_weeks, fdate, max_latency_weeks
        )

        if not contexts:
            typer.echo(f"{fdate}: no usable series, skipping")
            continue

        # horizon bookkeeping: max steps needed over all series/aheads in this call
        max_steps = 1
        for keys in groups:
            t_last = series[keys[0]]["t_last"]
            for a in ahead_list:
                _, _, steps = steps_ahead(t_last, fdate, a)
                max_steps = max(max_steps, steps)
        horizon = max_steps

        ctx_lens = [c.shape[-1] for c in contexts]
        last_dates = [series[keys[0]]["t_last"] for keys in groups]
        typer.echo(
            f"{fdate} (gen {gdate}): {len(groups)} context group(s), "
            f"{sum(len(k) for k in groups)} series, "
            f"context len {min(ctx_lens)}-{max(ctx_lens)}, "
            f"last obs {min(last_dates)}..{max(last_dates)}, horizon {horizon}"
        )

        if dry_run:
            continue

        # group contexts by number of variates so each predict_batch call is homogeneous
        by_nvar: dict[int, list[int]] = {}
        for i, c in enumerate(contexts):
            nvar = 1 if c.ndim == 1 else c.shape[0]
            by_nvar.setdefault(nvar, []).append(i)

        q23_by_key: dict[tuple[str, str], dict[int, np.ndarray]] = {}
        for nvar, idxs in by_nvar.items():
            batch_contexts = [contexts[i] for i in idxs]
            outs = list(
                model.predict_batch(
                    batch_contexts,
                    horizon,
                    return_quantiles=True,
                    use_symmetric_averaging=True,
                    make_positive=True,
                    sort_quantiles=True,
                    univariate=False,
                )
            )
            for i, fo in zip(idxs, outs):
                keys = groups[i]
                q9 = fo.quantiles  # (H, 9) or (V, H, 9)
                q23 = expand_quantiles(q9)  # (H, 23) or (V, H, 23)
                if len(keys) == 1:
                    q23_by_key[keys[0]] = {s: q23[s - 1] for s in range(1, horizon + 1)}
                else:
                    for v, key in enumerate(keys):
                        q23_by_key[key] = {s: q23[v, s - 1] for s in range(1, horizon + 1)}

        for (geo, src), s in series.items():
            q_by_step = q23_by_key.get((geo, src))
            if q_by_step is None:
                continue
            last_value = s["values"][~np.isnan(s["values"])][-1] if np.any(~np.isnan(s["values"])) else np.nan
            all_rows.extend(
                emit_rows(src, fid, geo, fdate, ahead_list, s["t_last"], last_value, q_by_step)
            )

    if dry_run:
        typer.echo("dry run complete, nothing written")
        raise typer.Exit()

    out_df = pd.DataFrame(all_rows)
    schema = pa.schema(
        [
            ("target", pa.string()),
            ("forecaster", pa.string()),
            ("geo_value", pa.string()),
            ("forecast_date", pa.date32()),
            ("target_end_date", pa.date32()),
            ("quantile", pa.float64()),
            ("value", pa.float64()),
        ]
    )
    table = pa.Table.from_pandas(out_df, schema=schema, preserve_index=False)
    out.parent.mkdir(parents=True, exist_ok=True)
    pq.write_table(table, out)
    typer.echo(
        f"wrote {len(out_df)} rows, {out_df['forecast_date'].nunique()} dates, "
        f"{out_df['geo_value'].nunique()} geos -> {out}"
    )


if __name__ == "__main__":
    app()
