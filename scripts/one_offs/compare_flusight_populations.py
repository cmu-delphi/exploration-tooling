"""Compare FluSight hub population values against Delphi epidata crosswalk populations.

Usage:
    uv run compare_flusight_populations.py [--hub PATH] [--version VERSION]

Fetches all available crosswalk versions from the epidata API and compares them
against the hub's locations.csv. Prints a summary table sorted by absolute
percent difference.
"""

import argparse
import csv
import io
import urllib.request
import json
from pathlib import Path


EPIDATA_BASE = "https://delphi.cmu.edu/epidata/v5"
DEFAULT_HUB = Path(__file__).parents[2] / "FluSight-forecast-hub" / "auxiliary-data" / "locations.csv"


def fetch_json(url: str) -> dict:
    with urllib.request.urlopen(url) as resp:
        return json.load(resp)


def fetch_crosswalk(version: str | None = None) -> dict[str, int] | None:
    """Return {state_abbr_upper: population} from the state->nation crosswalk, or None on error."""
    url = f"{EPIDATA_BASE}/geomap/crosswalk_pair/?first_geo_type=state&second_geo_type=nation"
    if version:
        url += f"&version={version}"
    try:
        with urllib.request.urlopen(url) as resp:
            text = resp.read().decode()
    except urllib.error.HTTPError as exc:
        print(f"  Skipping version {version}: HTTP {exc.code}")
        return None
    result = {}
    for row in csv.DictReader(io.StringIO(text)):
        result[row["from_val"].upper()] = int(row["pop"])
    return result


def load_hub_populations(path: Path) -> dict[str, int]:
    result = {}
    with open(path) as fh:
        for row in csv.DictReader(fh):
            result[row["abbreviation"]] = int(row["population"])
    return result


def available_versions() -> list[str]:
    data = fetch_json(f"{EPIDATA_BASE}/geomap/crosswalk_metadata/")
    return [vv["version_id"] for vv in data["versions"]]


def compare(hub: dict[str, int], epidata: dict[str, int], label: str) -> list[tuple]:
    """Return list of (abbr, hub_pop, api_pop, diff, pct) for states in both."""
    rows = []
    for abbr in sorted(set(hub) & set(epidata)):
        hpop = hub[abbr]
        epop = epidata[abbr]
        diff = hpop - epop
        pct = diff / epop * 100
        rows.append((abbr, hpop, epop, diff, pct))
    return sorted(rows, key=lambda x: abs(x[4]), reverse=True)


def print_comparison(rows: list[tuple], label: str, hub: dict, epidata: dict) -> None:
    only_hub = sorted(set(hub) - set(epidata))
    only_api = sorted(set(epidata) - set(hub))
    n_diff = sum(1 for *_, pct in rows if pct != 0)
    mae = sum(abs(pct) for *_, pct in rows) / len(rows) if rows else 0

    print(f"\n{'='*65}")
    print(f"  Epidata version: {label}")
    print(f"  {n_diff}/{len(rows)} states differ  |  MAE: {mae:.2f}%")
    if only_hub:
        print(f"  In hub only: {', '.join(only_hub)}")
    if only_api:
        print(f"  In API only: {', '.join(only_api)}")
    print(f"{'='*65}")
    print(f"{'Abbr':<6} {'Hub Pop':>12} {'API Pop':>12} {'Diff':>12} {'Diff%':>8}")
    print("-" * 55)
    for abbr, hpop, epop, diff, pct in rows:
        marker = " *" if diff != 0 else ""
        print(f"{abbr:<6} {hpop:>12,} {epop:>12,} {diff:>+12,} {pct:>+8.2f}%{marker}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hub", type=Path, default=DEFAULT_HUB, help="Path to locations.csv")
    parser.add_argument(
        "--version",
        help="Specific epidata version (e.g. 20251017). Omit to compare all versions.",
    )
    args = parser.parse_args()

    hub = load_hub_populations(args.hub)
    print(f"Loaded {len(hub)} locations from {args.hub}")

    versions = [args.version] if args.version else available_versions()
    print(f"Comparing against epidata versions: {', '.join(versions)}")

    for ver in versions:
        epidata = fetch_crosswalk(ver)
        if epidata is None:
            continue
        rows = compare(hub, epidata, ver)
        print_comparison(rows, ver, hub, epidata)


if __name__ == "__main__":
    main()
