# Python port experiment

An attempt to see the essential complexity in our pipelines.
This implements two forecasters in Python using sklearn: windowed_seasonal and climate_linear.
These two are chosen because they have non-trivial processing requirements and because the second one is a "meta"-forecasters, which will demo how ensembling can look in Python.

Whitening is ported. Adjust latency is NOT ported.
## Usage

```sh
# Get the NHSN and NSSP data with epidatpy
uv run py-probe/dump_inputs.py

# Run R forecasters (needs epipredict, quantreg, zeallot, here in the renv lib)
distrobox enter rocker -- Rscript py-probe/bakeoff.R

# Compare (runs the Python forecasters on the same csv and diffs)
uv run py-probe/compare.py
```

## Current Status

Numerical agreement (Python port vs. real R forecasters, same canonical
snapshot), with model logic equalized (`ADJUST_LATENCY=none`):

```
┌───────────────────────────┬──────┬────────────┬─────────┬─────────┬────────────────┐
│        forecaster         │  n   │ median |Δ| │ p90 |Δ| │ max |Δ| │ median rel err │
├───────────────────────────┼──────┼────────────┼─────────┼─────────┼────────────────┤
│ linear                    │ 5060 │ 0.03       │ 0.15    │ 0.78    │ 0.2%           │
├───────────────────────────┼──────┼────────────┼─────────┼─────────┼────────────────┤
│ climate_base              │ 5152 │ 0.13       │ 3.60    │ 181     │ 1.0%           │
├───────────────────────────┼──────┼────────────┼─────────┼─────────┼────────────────┤
│ windowed_seasonal         │ 5152 │ 0.08       │ 1.57    │ 26.4    │ 1.3%           │
├───────────────────────────┼──────┼────────────┼─────────┼─────────┼────────────────┤
│ climate_linear (ensemble) │ 5152 │ 0.35       │ 2.82    │ 94      │ 2.9%           │
├───────────────────────────┼──────┼────────────┼─────────┼─────────┼────────────────┤
│ climate_geo_agged         │ 5152 │ 1.00       │ 4.10    │ 9.0     │ 3.9%           │
└───────────────────────────┴──────┴────────────┴─────────┴─────────┴────────────────┘
```

## Files

```
common.py                  shared toolkit (season math, fetch, quantiles)
windowed_seasonal.py       windowed_seasonal + _extra_sources ports
climate_linear_ensemble.py climate components + the ensemble
dump_inputs.py             fetch canonical snapshot -> nhsn_input.csv
bakeoff.R                  real R forecasters on the same csv -> r_*.csv
compare.py                 diff, aligned by (forecaster, geo, quantile, horizon)
diagnose_windowed.py/.R    isolation harness (single ahead/quantile + fit introspection)
```
