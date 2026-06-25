# Orchestration & backtesting: design notes

Brainstorm on whether Python can clarify the pipeline structure needed for
backtests and improve on `targets`. No implementation yet — this captures the
structural argument and the decisions still open. Companion to `README.md`
(which covers the forecaster ports and the bake-off).

## First reframe: there are two pipelines, not one

`scripts/flu_hosp_prod.R` conflates them, and that's a major source of the
awkwardness:

- **Weekly production** — runs once, `as_of = today`. Caching mostly exists so
  *you* don't refit everything while iterating. Fine-grained invalidation
  ("I changed one forecaster, recompute only what's affected") earns its keep here.
- **Backtest** — runs over ~80 historical `as_of` dates. Embarrassingly parallel
  over dates. Wants throughput, version-faithful data, and resumability — *not* a
  general DAG.

They share forecaster functions but want different orchestration. Trying to make
one tool do both is part of why `targets` feels heavy.

**Recommendation: build the backtest harness first.** It's the cleaner, more
parallel structure, it's the capability currently missing, and it forces the
data layer (below) to be correct.

## The essential structure of a backtest

Stripped down, it's a partitioned map-reduce, not a general DAG:

```
for each forecast_date d:                    # partition — fully independent
    inputs_d   = archive.as_of(d)            # version-faithful slice  <- the crux
    leaf[d, f] = f(inputs_d, aheads)         # fan out over leaf forecasters
    ens[d, e]  = e(leaf[d, *])               # 2-stage intra-partition DAG
scores = score(all leaf + ens, latest_truth) # reduction across partitions
```

A date partition, a 2-stage dependency inside it (leaves -> ensembles), and a
final reduction. This is the same `pipeline()` shape used in the bake-off. It is
**not** the arbitrary DAG `targets` is built to schedule.

### Why `targets` fights us

A backtest is a *regular grid*; `targets` models *irregular DAGs*. To express a
regular grid in an irregular-DAG tool you have to **generate** it — which is
exactly the `tar_map` / `tar_combine` / `rlang::syms` / `!!!` metaprogramming
that makes `flu_hosp_prod.R` hard to read. Pick a tool whose primitive *is* the
grid/partition and the metaprogramming evaporates.

## Highest-clarity win: the data layer (orthogonal to orchestration)

`epix_as_of` hides the version logic inside an epiprocess S4 object. In Python it
becomes a visible, testable query:

```
archive   = one parquet of (geo, time_value, version, value)   # from epidata_archive
as_of(d)  = latest version <= d per (geo, time_value)          # a polars / DuckDB group-by
```

~10 lines replacing the most epidemiologically-critical and most opaque piece.
Worth doing **regardless** of the orchestration choice; it's the natural
extension of "make backtests work," and it upgrades the current bake-off (latest
snapshot) into a true as-of backtest.

## Orchestration options, ranked by current lean

1. **Plain Python + a content-addressed parquet cache.** Loop / process-pool over
   the date x forecaster grid; cache key = `hash(forecaster_id, params, as_of,
   input_version, code_version)`; ensembles read leaf parquets. ~100 lines,
   maximally legible, resumable. Default recommendation for the backtest.
2. **Dagster** — *if* a framework is wanted. Its **partitioned assets** map almost
   1:1 onto "backtest over a grid of dates," it tracks which partitions are
   materialized, and it gives lineage + a UI. The only mainstream tool whose core
   abstraction actually fits. Reach for it if observability/scale justify the
   dependency.
3. **A tiny custom asset layer** (asset = pure fn + deps + partition key + parquet
   cache). Only if option 1 grows warts — risk of reinventing a worse Dagster.

**Skeptical notes:**
- **Airflow is the wrong tool** — an operational *scheduler* for time-triggered
  DAGs, with no native notion of "cache this result keyed on its inputs."
- **Prefect** is lighter than Dagster but more imperative-glue than data-asset.

**The one genuinely hard part of rolling your own (option 1):** the `code_version`
component of the cache key — "did the forecaster logic change?". `targets` solves
this by hashing the command expression. For a backtest you usually rerun
everything, so a manual version bump or hashing the function source is plenty.
It only gets fussy if you want production-grade automatic invalidation — which is
a *production-pipeline* concern, reinforcing the two-pipeline split.

## Open forks (decide before implementing)

1. **Backtest-first, or replace production too?** Lean: backtest-first (cleaner,
   parallel, de-risks the data layer). Is the weekly run actually the bigger pain?
2. **Caching granularity for backtests:** rerun end-to-end each time, or need
   "add the new week / tweaked forecaster X -> recompute only affected cells"?
   Decides how much invalidation machinery is worth building.
3. **Scale:** roughly how many cells (dates x forecasters x geos x aheads), and how
   long does one forecaster fit take? Laptop + process pool vs. distributed.
4. **Observability:** UI/lineage wanted, or are CLI + logs + a scores parquet
   enough? This is the main thing that would justify Dagster over plain Python.

## Overall bet

**archive-as-parquet + `as_of()` query + a plain-Python partitioned fan-out with a
parquet cache** yields a backtest harness dramatically more legible than the
`targets` version. The production pipeline can graduate to Dagster later *if*
invalidation/observability demand it — but that's a separate, later decision.
