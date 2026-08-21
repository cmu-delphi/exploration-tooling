# /// script
# requires-python = ">=3.11"
# dependencies = ["polars>=1.0", "epidatpy", "pandas", "pyarrow"]
# ///
"""Fetch the canonical bake-off inputs once and write them to CSV, so the R
script and the Python comparison run on byte-identical data. This isolates the
bake-off to forecaster logic, not data-fetch differences between epidatpy and R.

Run:  uv run py-probe/dump_inputs.py
"""

from epidatpy import EpiDataContext

from common import fetch

ctx = EpiDataContext()
fetch(ctx, "nhsn", "confirmed_admissions_flu_ew", "value", "2022-01-01", "2026-06-01").sort(
    "geo_value", "time_value"
).write_csv("py-probe/nhsn_input.csv")
fetch(ctx, "nssp", "pct_ed_visits_influenza", "nssp", "2022-01-01", "2026-06-01").sort(
    "geo_value", "time_value"
).write_csv("py-probe/nssp_input.csv")
print("wrote py-probe/nhsn_input.csv, py-probe/nssp_input.csv")
