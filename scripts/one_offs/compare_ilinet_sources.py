# /// script
# requires-python = ">=3.11"
# dependencies = ["duckdb", "typer"]
# ///
"""Compare versioned ILINet from epidata v4 `fluview` against v5 `fluview_ilinet`.

Usage:
    uv run scripts/one_offs/compare_ilinet_sources.py fetch    # download into cache/ilinet/
    uv run scripts/one_offs/compare_ilinet_sources.py analyze  # print markdown tables

Needs DELPHI_EPIDATA_KEY in the environment (used for both APIs). Findings are in
notes/ilinet-sources.md.
"""

import json
import os
import time
import urllib.parse
import urllib.request
from pathlib import Path

import duckdb
import typer

ROOT = Path(__file__).parents[2]
CACHE = ROOT / "cache" / "ilinet"
V4 = "https://api.delphi.cmu.edu/epidata/fluview/"
V5 = "https://delphi.cmu.edu/epidata/v5/archive/"
SIGNALS = ["wili", "ili", "num_ili", "num_patients", "num_providers"]
V5_GEO_TYPES = ["state", "nation"]
# v4 region codes for states + national. v4 `jfk` is NYC (v5 `nyc`) and v4 `ny` is all of NY
# (v5 `ny` with fill_method nyc_plus_ny_minus_nyc). v5 `ny_minus_nyc` has no v4 counterpart.
STATES = (
    "ak al ar az ca co ct dc de fl ga hi ia id il in ks ky la ma md me mi mn mo ms mt nc nd ne nh nj "
    "nm nv ny nyc jfk oh ok or pa pr ri sc sd tn tx ut va vi vt wa wi wv wy"
).split()
V4_REGIONS = ["nat", *STATES]

app = typer.Typer(add_completion=False)


def _get(url: str) -> bytes:
    for attempt in range(4):
        try:
            with urllib.request.urlopen(url, timeout=600) as resp:
                return resp.read()
        except Exception as exc:  # noqa: BLE001
            print(f"  retry {attempt + 1} after {exc}")
            time.sleep(10 * (attempt + 1))
    raise RuntimeError(f"failed: {url.split('?')[0]}")


@app.command()
def fetch() -> None:
    """Download both sources into cache/ilinet/, skipping files already present."""
    key = os.environ["DELPHI_EPIDATA_KEY"]
    CACHE.mkdir(parents=True, exist_ok=True)
    for geo_type in V5_GEO_TYPES:
        for signal in SIGNALS:
            out = CACHE / f"v5_{geo_type}_{signal}.csv"
            if out.exists():
                continue
            qs = urllib.parse.urlencode(
                {"source": "fluview_ilinet", "signal": signal, "geo_type": geo_type, "token": key}
            )
            print(f"v5 {geo_type} {signal}")
            out.write_bytes(_get(f"{V5}?{qs}"))
            time.sleep(2)
    for region in V4_REGIONS:
        out = CACHE / f"v4_{region}.json"
        if out.exists():
            continue
        qs = urllib.parse.urlencode(
            {"regions": region, "epiweeks": "199701-203053", "issues": "199701-203053", "api_key": key}
        )
        print(f"v4 {region}")
        body = json.loads(_get(f"{V4}?{qs}"))
        if body["result"] != 1:
            print(f"  {region}: {body['message']}")
        out.write_text(json.dumps(body.get("epidata", [])))
        time.sleep(1)


def connect() -> duckdb.DuckDBPyConnection:
    """Load both sources as long tables keyed by (geo, week_end, signal, version date)."""
    con = duckdb.connect()
    con.execute("SET TimeZone = 'UTC'")
    con.execute(f"""
        CREATE TABLE v5 AS
        SELECT signal,
               CAST(report_time AS TIMESTAMP) AS report_time,
               CAST(report_time AS DATE) AS report_date,
               CASE geo_value WHEN 'us' THEN 'nat' WHEN 'nyc' THEN 'jfk' ELSE geo_value END AS geo,
               geo_type,
               CAST(reference_time AS DATE) AS week_end,
               value
        FROM read_csv('{CACHE}/v5_*.csv', union_by_name = true)
        WHERE age_group = 'all'
    """)
    unpivot = " UNION ALL ".join(
        f"SELECT region AS geo, epiweek, issue, CAST(release_date AS DATE) AS raw_release_date, lag, "
        f"'{s}' AS signal, CAST({s} AS DOUBLE) AS value FROM raw WHERE {s} IS NOT NULL"
        for s in SIGNALS
    )
    con.execute(f"""
        CREATE TABLE raw AS SELECT * FROM read_json('{CACHE}/v4_*.json', format = 'array',
            columns = {{region: 'VARCHAR', epiweek: 'INT', issue: 'INT', release_date: 'VARCHAR', lag: 'INT',
                       wili: 'DOUBLE', ili: 'DOUBLE', num_ili: 'DOUBLE', num_patients: 'DOUBLE',
                       num_providers: 'DOUBLE'}});
        -- Saturday ending MMWR week: week 1 starts on the Sunday on or before Jan 4.
        CREATE MACRO ew_end(ew) AS CAST(
            make_date(CAST(ew // 100 AS INT), 1, 4)
            - to_days(CAST(dayofweek(make_date(CAST(ew // 100 AS INT), 1, 4)) AS INT))
            + to_days(CAST((ew % 100) * 7 - 1 AS INT)) AS DATE);
        CREATE TABLE v4 AS
        -- Some state rows lack release_date; those fall back to the Friday after the issue week.
        SELECT u.*, ew_end(epiweek) AS week_end, ew_end(issue) AS issue_week_end,
               coalesce(raw_release_date, ew_end(issue) + 6) AS release_date
        FROM ({unpivot}) u;
    """)
    return con


def md(con: duckdb.DuckDBPyConnection, title: str, sql: str) -> None:
    rel = con.sql(sql)
    cols = rel.columns
    rows = rel.fetchall()
    print(f"\n### {title}\n")
    print("| " + " | ".join(cols) + " |")
    print("|" + "---|" * len(cols))
    for r in rows:
        print("| " + " | ".join("" if v is None else (f"{v:.6g}" if isinstance(v, float) else str(v)) for v in r) + " |")


@app.command()
def analyze() -> None:
    """Print the coverage, alignment, agreement, revision and finalized-value tables."""
    con = connect()
    geo_class = "CASE WHEN geo = 'nat' THEN 'nat' ELSE 'state' END"

    # MMWR week sanity check: 202350 ends 2023-12-16.
    assert str(con.sql("SELECT ew_end(202350)").fetchone()[0]) == "2023-12-16"

    md(con, "Coverage: v4 fluview (issue rows)", f"""
        SELECT {geo_class} AS geo_class, signal, count(DISTINCT geo) AS n_geo,
               min(week_end) AS first_week, max(week_end) AS last_week,
               min(issue) AS first_issue, max(issue) AS last_issue, count(*) AS n_rows,
               median(n) AS med_issues_per_geo_week
        FROM (SELECT *, count(*) OVER (PARTITION BY geo, signal, epiweek) AS n FROM v4)
        GROUP BY ALL ORDER BY ALL
    """)
    md(con, "Coverage: v5 fluview_ilinet (change rows)", f"""
        SELECT {geo_class} AS geo_class, signal, count(DISTINCT geo) AS n_geo,
               min(week_end) AS first_week, max(week_end) AS last_week,
               min(report_date) AS first_report, max(report_date) AS last_report, count(*) AS n_rows,
               median(n) AS med_versions_per_geo_week
        FROM (SELECT *, count(*) OVER (PARTITION BY geo, signal, week_end) AS n FROM v5)
        GROUP BY ALL ORDER BY ALL
    """)
    md(con, "Geos in one source only", """
        SELECT 'v4 only' AS side, string_agg(DISTINCT geo, ' ') AS geos FROM v4 WHERE geo NOT IN (SELECT geo FROM v5)
        UNION ALL
        SELECT 'v5 only', string_agg(DISTINCT geo, ' ') FROM v5 WHERE geo NOT IN (SELECT geo FROM v4)
    """)

    # Version alignment: weekday of v5 report dates, and their relation to v4 release dates.
    md(con, "v5 report_time weekday/hour distribution", """
        SELECT dayname(report_time) AS weekday, count(DISTINCT report_time) AS n_report_times,
               min(report_date) AS first, max(report_date) AS last
        FROM v5 GROUP BY ALL ORDER BY n_report_times DESC
    """)
    con.execute("""
        CREATE TABLE releases AS
        SELECT issue, issue_week_end, min(release_date) AS release_date, max(release_date) AS max_release_date
        FROM v4 WHERE geo = 'nat' GROUP BY ALL;
        CREATE TABLE v5_reports AS
        SELECT DISTINCT report_time, report_date FROM v5;
    """)
    md(con, "v4 release_date minus issue week end (days)", """
        SELECT release_date - issue_week_end AS days_after_week_end, count(*) AS n_issues,
               min(issue) AS first_issue, max(issue) AS last_issue
        FROM releases GROUP BY ALL ORDER BY n_issues DESC LIMIT 10
    """)
    md(con, "v5 report dates matched to v4 release dates", """
        SELECT CASE WHEN r.release_date IS NOT NULL THEN 'equals a v4 release_date'
                    WHEN r2.release_date IS NOT NULL THEN 'within 6 days after a v4 release_date'
                    ELSE 'no nearby v4 release' END AS match,
               count(*) AS n_report_times, min(v.report_date) AS first, max(v.report_date) AS last
        FROM v5_reports v
        LEFT JOIN releases r ON v.report_date = r.release_date
        LEFT JOIN (SELECT DISTINCT release_date FROM releases) r2
               ON v.report_date > r2.release_date AND v.report_date <= r2.release_date + 6 AND r.release_date IS NULL
        GROUP BY ALL ORDER BY ALL
    """)
    md(con, "v5 report times not on a v4 release date", """
        SELECT v.report_time, count(*) AS n_rows, min(week_end) AS min_week, max(week_end) AS max_week
        FROM v5 v WHERE report_date NOT IN (SELECT release_date FROM releases)
        GROUP BY ALL ORDER BY n_rows DESC LIMIT 15
    """)

    # v5 as of each v4 release date (v5 is change-only, so carry the last change forward).
    con.execute("""
        CREATE TABLE paired AS
        SELECT a.geo, a.signal, a.epiweek, a.week_end, a.issue, a.release_date, a.lag,
               a.value AS v4_value, b.value AS v5_value, b.report_date AS v5_report_date
        FROM v4 a ASOF LEFT JOIN v5 b
          ON a.geo = b.geo AND a.signal = b.signal AND a.week_end = b.week_end
         AND a.release_date >= b.report_date
    """)
    agree = f"""
        SELECT {{grp}}, count(*) AS n,
               round(100 * avg(CASE WHEN v5_value IS NULL THEN 1 ELSE 0 END), 2) AS pct_v5_missing,
               round(100 * avg(CASE WHEN v4_value = v5_value THEN 1 ELSE 0 END), 2) AS pct_exact,
               round(100 * avg(CASE WHEN abs(v4_value - v5_value) <= 5e-6 THEN 1 ELSE 0 END), 2) AS pct_within_5e6,
               round(100 * avg(CASE WHEN abs(v4_value - v5_value) <= 1e-3 * greatest(abs(v4_value), 1) THEN 1 ELSE 0 END), 2) AS pct_within_0p1pct,
               max(abs(v4_value - v5_value)) AS max_abs_diff
        FROM paired {{where}} GROUP BY ALL ORDER BY ALL
    """
    md(con, "Agreement at v4 (geo, week, issue), v5 as of the release date", agree.format(
        grp=f"{geo_class} AS geo_class, signal", where=""))
    md(con, "Agreement by issue season (ili, all geos)", agree.format(
        grp="CASE WHEN issue % 100 >= 40 THEN issue // 100 ELSE issue // 100 - 1 END AS season_start",
        where="WHERE signal = 'ili'"))
    md(con, "Agreement on the lag-0 (first) issue", agree.format(
        grp=f"{geo_class} AS geo_class, signal", where="WHERE lag = 0"))
    md(con, "Agreement restricted to issues >= 2019w40 and v5 present", agree.format(
        grp=f"{geo_class} AS geo_class, signal", where="WHERE issue >= 201940 AND v5_value IS NOT NULL"))
    md(con, "Agreement excluding ny (v4 sums NY itself), the 2017w40-41 backfill issues, and rows v5 lacks", agree.format(
        grp=f"{geo_class} AS geo_class, signal", where="WHERE geo != 'ny' AND issue > 201741 AND v5_value IS NOT NULL"))
    md(con, "Largest mismatches (v5 present, diff > 5e-6)", """
        SELECT geo, signal, epiweek, issue, release_date, v5_report_date, v4_value, v5_value,
               v4_value - v5_value AS diff
        FROM paired WHERE v5_value IS NOT NULL AND abs(v4_value - v5_value) > 5e-6
        ORDER BY abs(v4_value - v5_value) / greatest(abs(v4_value), 1) DESC LIMIT 20
    """)
    md(con, "Mismatch (diff > 5e-6, v5 present) counts by geo, top 15", """
        SELECT geo, count(*) AS n_mismatch, count(DISTINCT issue) AS n_issues, min(issue) AS first_issue,
               max(issue) AS last_issue
        FROM paired WHERE v5_value IS NOT NULL AND abs(v4_value - v5_value) > 5e-6
        GROUP BY ALL ORDER BY n_mismatch DESC LIMIT 15
    """)
    # Shift test: does v5 as of release_date + k days agree better, i.e. is the version off by a week?
    md(con, "Why v5 is missing at a v4 (geo, week, issue)", f"""
        SELECT {geo_class} AS geo_class,
               CASE WHEN p.release_date < f.first_report THEN 'v4 release before first v5 report of the week'
                    WHEN f.first_report IS NULL THEN 'week never in v5'
                    ELSE 'other' END AS reason,
               count(*) AS n, min(p.issue) AS first_issue, max(p.issue) AS last_issue,
               string_agg(DISTINCT p.geo, ' ') AS geos
        FROM paired p LEFT JOIN (SELECT geo, signal, week_end, min(report_date) AS first_report FROM v5 GROUP BY ALL) f
          USING (geo, signal, week_end)
        WHERE p.v5_value IS NULL AND p.signal = 'ili' GROUP BY ALL ORDER BY ALL
    """)
    md(con, "Mismatches (ili, v5 present, diff > 5e-6) by geo", """
        SELECT geo, count(*) AS n_mismatch, count(DISTINCT issue) AS n_issues, min(issue) AS first_issue,
               max(issue) AS last_issue, count(DISTINCT epiweek) AS n_weeks, min(epiweek) AS first_week,
               max(epiweek) AS last_week, median(abs(v4_value - v5_value)) AS median_abs_diff
        FROM paired WHERE signal = 'ili' AND v5_value IS NOT NULL AND abs(v4_value - v5_value) > 5e-6
        GROUP BY ALL ORDER BY n_mismatch DESC LIMIT 20
    """)
    md(con, "Mismatches (ili, v5 present, diff > 5e-6, geo != ny) by issue, top 15", """
        SELECT issue, release_date, count(*) AS n_mismatch, count(DISTINCT geo) AS n_geo,
               min(epiweek) AS first_week, max(epiweek) AS last_week, max(abs(v4_value - v5_value)) AS max_abs_diff
        FROM paired WHERE signal = 'ili' AND geo != 'ny' AND v5_value IS NOT NULL AND abs(v4_value - v5_value) > 5e-6
        GROUP BY ALL ORDER BY n_mismatch DESC LIMIT 15
    """)
    shift = " UNION ALL ".join(
        f"""SELECT {k} AS k, abs(a.value - b.value) <= 5e-6 AS ok FROM
            (SELECT *, release_date + {k} AS t FROM v4 WHERE signal = 'ili' AND issue >= 201940) a
            ASOF LEFT JOIN v5 b ON a.geo = b.geo AND a.signal = b.signal AND a.week_end = b.week_end
            AND a.t >= b.report_date"""
        for k in [-7, -1, 0, 1, 6, 7]
    )
    md(con, "Version shift test (ili, issues >= 2019w40): agreement when v5 is read k days after release", f"""
        SELECT k, count(*) AS n, round(100 * avg(CASE WHEN ok THEN 1 ELSE 0 END), 2) AS pct_within_5e6
        FROM ({shift}) GROUP BY k ORDER BY k
    """)

    # v5 change rows that v4 never shows as a value change on that release.
    con.execute("""
        CREATE TABLE v4_changes AS
        SELECT * FROM (
            SELECT *, lag(value) OVER (PARTITION BY geo, signal, epiweek ORDER BY issue) AS prev
            FROM v4) WHERE prev IS NULL OR prev != value
    """)
    md(con, "Number of value changes per (geo, signal, week), issues >= 2019w40 vs v5 reports >= 2019-10-01", f"""
        WITH a AS (SELECT {geo_class} AS g, signal, epiweek, count(*) AS n FROM v4_changes
                   WHERE issue >= 201940 GROUP BY ALL),
             b AS (SELECT {geo_class} AS g, signal, week_end, count(*) AS n FROM v5
                   WHERE report_date >= DATE '2019-10-01' GROUP BY ALL)
        SELECT 'v4' AS src, g, signal, sum(n) AS n_changes, avg(n) AS mean_per_week FROM a GROUP BY ALL
        UNION ALL SELECT 'v5', g, signal, sum(n), avg(n) FROM b GROUP BY ALL ORDER BY g, signal, src
    """)

    # Finalized: latest version per (geo, signal, week).
    con.execute("""
        CREATE TABLE fin AS
        SELECT coalesce(a.geo, b.geo) AS geo, coalesce(a.signal, b.signal) AS signal,
               coalesce(a.week_end, b.week_end) AS week_end, a.value AS v4_value, b.value AS v5_value
        FROM (SELECT * FROM v4 QUALIFY row_number() OVER (PARTITION BY geo, signal, week_end ORDER BY issue DESC) = 1) a
        FULL JOIN (SELECT * FROM v5 QUALIFY row_number() OVER (PARTITION BY geo, signal, week_end ORDER BY report_time DESC) = 1) b
          USING (geo, signal, week_end)
    """)
    md(con, "Finalized (latest) values", f"""
        SELECT {geo_class} AS geo_class, signal, count(*) AS n_geo_weeks,
               sum(CASE WHEN v5_value IS NULL THEN 1 ELSE 0 END) AS v4_only,
               sum(CASE WHEN v4_value IS NULL THEN 1 ELSE 0 END) AS v5_only,
               round(100 * avg(CASE WHEN v4_value = v5_value THEN 1.0 WHEN v4_value IS NULL OR v5_value IS NULL THEN NULL ELSE 0 END), 2) AS pct_exact,
               round(100 * avg(CASE WHEN abs(v4_value - v5_value) <= 5e-6 THEN 1.0 WHEN v4_value IS NULL OR v5_value IS NULL THEN NULL ELSE 0 END), 2) AS pct_within_5e6,
               max(abs(v4_value - v5_value)) AS max_abs_diff
        FROM fin GROUP BY ALL ORDER BY ALL
    """)
    md(con, "Finalized mismatches (diff > 5e-6), largest", """
        SELECT geo, signal, week_end, v4_value, v5_value, v4_value - v5_value AS diff FROM fin
        WHERE abs(v4_value - v5_value) > 5e-6
        ORDER BY abs(v4_value - v5_value) / greatest(abs(v4_value), 1) DESC LIMIT 15
    """)
    md(con, "Finalized coverage gaps by geo (weeks present in one source only)", f"""
        SELECT geo, signal, sum(CASE WHEN v5_value IS NULL THEN 1 ELSE 0 END) AS v4_only,
               min(CASE WHEN v5_value IS NULL THEN week_end END) AS v4_only_first,
               max(CASE WHEN v5_value IS NULL THEN week_end END) AS v4_only_last,
               sum(CASE WHEN v4_value IS NULL THEN 1 ELSE 0 END) AS v5_only,
               min(CASE WHEN v4_value IS NULL THEN week_end END) AS v5_only_first,
               max(CASE WHEN v4_value IS NULL THEN week_end END) AS v5_only_last
        FROM fin WHERE signal = 'ili' GROUP BY ALL
        HAVING v4_only + v5_only > 0 ORDER BY v4_only + v5_only DESC LIMIT 20
    """)

    md(con, "v4 state wili vs ili (v5 has no state wili)", """
        SELECT count(*) AS n, avg(CASE WHEN a.value = b.value THEN 1 ELSE 0 END) AS share_equal
        FROM v4 a JOIN v4 b USING (geo, epiweek, issue)
        WHERE a.signal = 'wili' AND b.signal = 'ili' AND a.geo != 'nat'
    """)
    md(con, "Issues (>= 2017w40) missing for some geos in v4", """
        WITH s AS (SELECT DISTINCT geo, issue FROM v4 WHERE signal = 'ili'),
             i AS (SELECT DISTINCT issue FROM v4 WHERE issue >= 201740),
             g AS (SELECT DISTINCT geo FROM s)
        SELECT issue, count(*) AS n_geo_missing FROM g CROSS JOIN i ANTI JOIN s USING (geo, issue)
        GROUP BY ALL ORDER BY issue
    """)

    # Revision trajectories for spot checks: first 8 issues plus any issue where either source changes.
    for geo, signal in [("ca", "ili"), ("ny", "ili"), ("tx", "ili"), ("nat", "wili")]:
        for ew in [201952, 202250, 202350]:
            md(con, f"Trajectory {geo} {signal} epiweek {ew}", f"""
                SELECT issue, release_date, lag, v4_value, v5_value, v5_report_date,
                       round(v4_value - v5_value, 6) AS diff
                FROM (SELECT *, lag(v4_value) OVER w AS p4, lag(v5_value) OVER w AS p5 FROM paired
                      WHERE geo = '{geo}' AND signal = '{signal}' AND epiweek = {ew}
                      WINDOW w AS (ORDER BY issue))
                WHERE lag <= 8 OR v4_value IS DISTINCT FROM p4 OR v5_value IS DISTINCT FROM p5
                ORDER BY issue
            """)

if __name__ == "__main__":
    app()
