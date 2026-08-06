# Annotated commit log: ds/refactor2 (bcd7501..06c38a8)

All 71 commits on the branch relative to `main@origin` (bcd7501, the
pre-refactor baseline), oldest first, with what changed, why, and how it was
verified. Compiled 2026-07-22 by cross-referencing the session worklogs
(`2026-07-18-refactoring-log.md`, `2026-07-19-ensemble-layer-log.md` — since
removed from notes/; retrieve them from git history at `06c38a8` for
per-step verification detail), the surviving experiment record
`2026-07-22-prod-golden-main-vs-refactor2.md`, and the capture labels under
`cache/oracle/`. This file is the source of truth for what happened on the
branch.

Legend for the reviewer:

- Unmarked commits are **implemented, behavior-preserving refactors**.
- **[behavior change]** — intentional, visible output change; each one is
  attributed in the 2026-07-22 main-vs-branch golden.
- **[experiment]** / **[idea]** — notes-only commits: a measurement that led
  to no code change, or a design scout for future work. Nothing in them is
  implemented unless a later commit says so.
- Each code commit ends with a *Golden:* line: how it was replay-verified.
  "per-commit" means a dedicated capture/comparison for that step; "batch"
  means it's covered by a later golden spanning several commits;
  "store comparison" means old-vs-new diffed against the cached prod store
  rather than an oracle capture pair. Everything on the branch is also
  covered end-to-end by the final 2026-07-22 main-vs-branch prod golden
  (`today-main` vs `today-refactor2`), which found no unexplained diffs.

NB: the `ds/refactor2` bookmark currently points at `ea75903`; the final
notes commit `06c38a8` sits above it.

## Setup: pinning, goldens, plan (07-12..13)

- `5f998f9` doc: claude docs — Add CLAUDE.md and REFACTORING_IDEAS.md
  recording the 5-review consensus plan: unify the duplicated explore/prod
  forecaster wiring behind one spec + shared runner. The plan document; the
  rest of the branch executes it.
- `a568de0` Add oracle golden capture/compare framework from ds/refactor —
  `scripts/oracle/capture.R`/`compare.R` snapshot a targets store to labeled
  parquet captures and diff two labels. Every behavior-preserving step on
  this branch is verified by an empty golden diff against these captures.
  *Golden: n/a (is the golden tooling).*
- `6d6c568` Support FORECAST_REFERENCE_DATE pinning in flu prod pipeline —
  Env var pins the pipeline's "today" instead of bare `Sys.Date()`. Needed
  for reproducible replays, which the goldens depend on.
  *Golden: batch — 2026-07-13 flu prod capture pair
  (`baseline-ynxvxykm` vs `refactored-yopxxtmx`).*

## Flu prod: dissolve hand-written closures into a declarative grid (07-13)

These five landed together and were verified collectively by the 2026-07-13
flu prod golden (`baseline-ynxvxykm` vs `refactored-yopxxtmx`), not
per-commit.

- `fc0f547` refactor: move flu prod forecaster kwargs into grid params —
  Forecaster kwargs move out of the per-disease closures in
  `R/flu_prod_forecasters.R` into grid `params` consumed by the runner.
  First step of replacing hand-copied prod wrappers with declarative specs.
- `6d6e299` refactor: add explicit prod grid columns for smuggled wrapping
  logic — Cross-cutting behavior hidden in wrappers (as-of policy,
  extra-data join, source filter, geo exclusions) becomes explicit
  per-forecaster grid columns. Makes the conventions visible and diffable
  instead of buried in closure bodies.
- `a7c4096` refactor: wire prod grid columns into runner and drop id
  dispatch — The runner reads the new spec columns; the `grepl("latest",
  id)` id-string dispatch dies. Dispatch-by-id-substring was fragile and
  invisible to the grid.
- `e3700ad` refactor: rename latest as-of policy to cheating — The "latest"
  policy reads finalized data past the forecast date; naming it `cheating`
  keeps anyone from mistaking it for a real-time policy.
- `db83efe` Use tibble for single-row prod forecaster configs — Cosmetic
  consistency of the grid declarations.
- `00464e3` doc: update plan doc — Adds the phase 0–4 implementation sketch
  (canonical archives → shared snapshot → shared runner → unified grid →
  pipelines consume it) to REFACTORING_IDEAS.md.

## Flu prod: phases 0–4 of the shared stack (07-13..14)

- `74a8149` refactor: hoist version-independent munging into prod archive
  targets — Phase 0: canonical `nhsn_prod_archive`/`nssp_target_archive`
  built once (usa→us, Wednesday shift, season info, source stamp,
  ILI+/flusurv extras folded in, nssp-as-target spoof); `full_data` shrinks
  to an as-of slice. These transforms are version-independent, so doing them
  per forecaster × date was pure duplication and drift surface.
  *Golden: per-commit — full `prod-flu-backtest` (9051 targets, 0 errors)
  store-compared against `_local/flu_hosp_prod_pre_phase0_backup`; all 51
  shared targets identical.*
- `1be430b` doc: record phase 0 verification findings.
- `87ef5f9` refactor: extract make_forecast_snapshot from epix_slide_simple —
  Phase 1: one shared snapshot function (archive + dates + as-of policy +
  substitutions → `epi_df` with correct metadata); `epix_slide_simple`
  becomes a map over it, parquet cache preserved. Explore and prod now share
  the snapshot code path instead of re-implementing it.
  *Golden: per-commit store comparison — ~12 (forecast, generation) date
  pairs incl. substitution/holiday weeks match cached `full_data_*` under
  both policies; explore old-vs-new identical for 3 dates. Full backtest
  skipped as redundant.*
- `606786a` refactor: build flu prod full_data and nssp slice via
  make_forecast_snapshot — Prod's per-date data targets become calls to the
  shared snapshot. Deletes prod's independent copy of as-of slicing.
  *Golden: same phase-1 store comparison as `87ef5f9`.*
- `f9c6cd7` doc: record phase 1 status.
- `e0972a8` refactor: rename run_prod_forecaster to shared run_forecaster
  with sort_quantiles — Phase 2: the runner owns ahead scaling, source
  filtering, extra-data join, target-date shift, geo exclusions, id
  stamping. One place for cross-cutting conventions both pipelines must
  agree on.
  *Golden: per-commit store comparison — runner reproduces cached
  `forecast_nhsn_*` exactly for deterministic forecasters (stochastic match
  under same seed).*
- `d11eec6` refactor: route explore forecast/score through `run_forecaster`
  with `sort_quantiles`/`output_scale` specs — Explore calls the same runner;
  the `g_disease == "flu"` sort hack and the score-time population-column
  sniff become spec columns. This is the drift-killer: prod can no longer
  silently diverge from what exploration evaluated.
  *Golden: per-commit store comparison — explore forecasts and scores
  maxd=0 for 2 flu + 1 covid forecasters × 3 dates vs cached explore
  stores.*
- `1c903fe` doc: record phase 2 status.
- `72f4ce1` refactor: fold forecaster spec columns into shared
  make_forecaster_grid — Phase 3: `FORECASTER_SPEC_DEFAULTS` in `R/utils.R`;
  prod's separate metadata `left_join` table deleted, each forecaster tibble
  declares overrides inline. One grid schema for explore and prod.
  *Golden: per-commit — grid-information equivalence old-vs-new (rows, ids,
  params, spec columns) for flu prod + flu/covid explore; manifests build.*
- `e9eae9b` refactor: apply ahead multiplier at call site, drop it from
  run_forecaster — Per-forecaster `ahead_multiplier` (1 or 7) applied when
  building aheads; the runner goes unit-agnostic. Week-native and day-native
  forecasters coexist without the runner knowing about units.
  *Golden: per-commit store comparison — old-vs-new runner byte-identical
  incl. `target_end_date` on cached snapshots (shift-0, shift-3,
  cheating+extra-join cases).*
- `0246fba` doc: record phase 3 status.
- `be8f1de` refactor: hoist flu prod nssp snapshot into its own target —
  Phase 4: `nssp_forecast_data` snapshot target mirrors `full_data`;
  forecast targets shrink to essentially one `run_forecaster` call. Reaches
  the target shape the plan asked for; snapshots cache independently.
  *Golden: per-commit — full backtest (10,416 targets, 0 errors)
  store-compared vs `_local/flu_hosp_prod_pre_phase4_backup`: all 7,764
  shared forecast-family targets hash-identical.*
- `ae75429` doc: describe shared spec/runner wiring for new forecasters —
  README section for forecaster authors.
- `840ecab` doc: record phase 4 status.

## Flu prod: ports from the abandoned ds/refactor branch (07-15..16)

The 07-15..16 code commits below were spot-verified individually as noted,
then collectively confirmed by the 2026-07-18 batch golden: fresh eval
capture `head-nxpuszrv` vs the last golden `refactored-yopxxtmx` (07-13) —
deterministic non-cheating forecasters bit-identical, all other diffs
attributed to the marked intentional changes.

- `39f3298` fix: seed stochastic forecasters from semantic cell key —
  **[behavior change]** `set.seed(tar_seed_create(id/signal/date/ahead))`
  instead of target-name-derived seeds. Renaming a target no longer silently
  moves the three stochastic forecasters.
  *Golden: partial by design — self-check that the 5 deterministic
  forecasters stay bit-zero; stochastic ones move intentionally. Confirmed
  in the 07-18 batch golden.*
- `cbaa19e` refactor: parameterize primary source in `scaled_pop_seasonal` —
  `primary_source` arg (default `"nhsn"`) replaces hardcoded
  `source == "nhsn"` filters. Behavior-preserving; enables the honest nssp
  de-spoof below.
  *Golden: batch (07-18 `head` vs `yopxxtmx`).*
- `3b09620` refactor: split flu evaluation into its own targets project and
  store — `flu_hosp_evaluation` (same script, own store), `make eval-flu`,
  `EVALUATION_N_DATES`. Historical replays can no longer invalidate the
  weekly prod cache, retiring the `_local/*_backup` store-copy dance.
  *Golden: batch — plus every later flu golden exercises this store.*
- `a744b29` refactor: fold -log make recipes into tee'd prod recipes —
  Deduplicates the Makefile's parallel `*-log` recipe copies.
  *Golden: n/a (Makefile only).*
- `ede8f9b` doc: fold ds/refactor findings into REFACTORING_IDEAS —
  Condensed retrospective of the abandoned branch; durable findings carried
  forward.
- `4b19e85` refactor: stop relying on TAR_PROJECT default for project
  selection — Recipes select projects via `TAR_RUN_PROJECT`/explicit
  `Sys.setenv`. The repo `.Renviron` overrides shell-exported `TAR_PROJECT`
  on every Rscript start, so recipes could silently run the wrong project.
  *Golden: n/a (Makefile/run.R plumbing; exercised by every later replay).*
- `771dc44` refactor: unify flu forecast schedule into one tibble — One
  `g_forecast_schedule` pairing nominal forecast dates with actual
  generation dates, replacing parallel vectors that could skew.
  *Golden: batch (07-18 `head` vs `yopxxtmx`).*
- `5cb08e2` refactor: tighten `make_forecast_snapshot` API — Small
  signature/internals cleanup of the shared snapshot function.
  *Golden: batch.*
- `6e1a255` refactor: standardize `run_forecaster` `geo_value` on character —
  Drops a factor stamp; parquet-invisible, removes a gratuitous explore/prod
  difference.
  *Golden: batch.*
- `9c46043` fix: cut exogenous nssp cheating slice at generation date, hoist
  to target — **[behavior change]** The exogenous nssp slice's cheating
  cutoff sat at the nominal forecast date while everything else used the
  generation date — judged a latent bug (holiday weeks only, affects
  `seasonal_nssp_cheating`). Also hoists the slice to its own
  `nssp_exogenous_data` target through the shared snapshot.
  *Golden: per-commit store comparison — asof branch unchanged by
  construction; cheating diff analysis-verified. Batch golden attributes
  the residual `seasonal_nssp_cheating` drift.*
- `5e07703` refactor: stamp nssp target archive honestly, pass
  primary_source — **[behavior change]** `nssp_target_archive` stamps
  `source = "nssp"` instead of spoofing `"nhsn"`; `run_forecaster` injects
  `primary_source = "nssp"` at the nssp call site. Values bit-identical;
  only the output `source` column stops lying.
  *Golden: per-commit store comparison — nssp forecasts bit-identical
  except the `source` column flip.*
- `e401fa5` fix: scope pipefail to tee'd recipes instead of all Makefile
  recipes — Global pipefail broke unrelated recipes.
  *Golden: n/a (Makefile only).*
- `f460784` fix: stamp missing-source fallback with primary_source in
  scaled_pop_seasonal — The no-source-column fallback now labels rows with
  `primary_source` rather than hardcoded `"nhsn"`, completing the
  parameterization. *Golden: batch (07-18).*
- `caef497` perf: hoist archive hash out of per-date snapshot loop — Hash
  the archive once per run for the parquet cache key instead of once per
  date. Pure speed; cache paths unchanged. *Golden: batch (07-18).*
- `ef1ab9b` fix: assert augmentation rows predate the forecast window in
  nhsn_prod_archive — The phase-0 fold-in of faux-versioned ILI+/flusurv
  extras is only behavior-preserving because those datasets are strictly
  historical; this makes that empirical precondition a loud assert.
  *Golden: batch (07-18; assert is a no-op on passing data).*
- `1600a3d` docs: distill refactor vision into CLAUDE.md, move log to
  notes/, orthogonalize README.
- `3aceb83` docs: mark rsv prod pipeline as an unwritten stub — Every rsv
  reference points at a script that doesn't exist; say so rather than let
  someone run it.

## Contracts, and the two bugs they caught (07-18)

Each verified by eval replay + capture diff `head-nxpuszrv` vs
`contracts-nnqsptrt` with archives bit-identical between runs.

- `ba87cfb` feat: assert version faithfulness at the snapshot boundary —
  `make_forecast_snapshot()`'s asof branch aborts if any assembled row has
  `time_value > generation_date`, and rejects `forecast_date >
  generation_date`. Catches faux-versioned rows that `epix_as_of`'s version
  bound lets through.
  *Golden: per-commit (head vs contracts capture pair) + unit tests.*
- `66b50a9` fix: name the quantile column consistently in
  `g_baseline_forecaster` early return — **[behavior change]** The `ahead < 3`
  early return said `quantile_value`, injecting a spurious all-NA column
  into every `forecast_*_full` via bind_rows. Found by the output contract;
  schema-only fix.
  *Golden: per-commit — capture schema goes from 9 to 8 columns, values
  unchanged.*
- `87998cf` feat: validate forecaster output shape in run_forecaster —
  `validate_forecast_output()`: keys present, no NAs, non-negative,
  quantiles monotone per task. Asserted rather than silently re-sorted, so
  crossings surface unless a grid row opts into `sort_quantiles`.
  *Golden: per-commit (head vs contracts) + unit tests.*
- `3e0f28b` fix: sort quantiles for the seasonal-family flu prod
  forecasters — **[behavior change]** The contract proved flu prod was
  shipping crossing quantiles (~18% of seasonal-family tasks). Opts
  `scaled_pop_seasonal` into `sort_quantiles = TRUE`, matching what explore
  evaluated; ensembles built on previously-crossing tasks shift accordingly.
  *Golden: per-commit — every changed value verified a pure within-task
  permutation (multisets identical); contracts capture monotone everywhere.
  New flu baseline: `contracts-nnqsptrt`.*
- `b8defb7` docs: record golden confirmation and contract findings in
  roadmap and log.

## Covid prod: migration to the shared stack (07-18)

One golden capture per step in `cache/oracle/covid_hosp_evaluation/`
(pinned 2026-06-24, n=1 unless noted), plus a 4-date old-vs-new replay
(`multibase`/`multihead`) and, after the NA fix, a 9-date winter-window
replay (`multibase9`/`multihead9`).

- `bfcd4a4` refactor: pin covid prod on `FORECAST_REFERENCE_DATE` and unify
  its forecast schedule — Covid's `6d6c568` + `771dc44`.
  *Golden: none of its own (behavior-identical by construction when the env
  var is unset); covered by the `multibase`→`multihead` old-vs-new replays.*
- `69fe59f` refactor: split covid evaluation into its own targets project
  and store — `covid_hosp_evaluation` + `make eval-covid`; mirrors
  `3b09620`; BACKTEST_MODE becomes rsv-stub-only.
  *Golden: is the baseline — old-code capture `baseline-wtpmqmxu` taken
  here.*
- `5328f26` refactor: move covid prod forecasters onto the shared grid and
  runner — Closures in `R/covid_prod_forecasters.R` dissolve into grid rows;
  `grepl("latest")` → `as_of_policy = "cheating"`; new `min_train_date` spec
  column; seasonal family opts into sort_quantiles (**[behavior change]**,
  same crossing-quantiles fix as flu's `3e0f28b` — covid crossed on ~12% of
  seasonal tasks).
  *Golden: per-commit — `runner-orknoplo` vs `baseline-wtpmqmxu`: 5
  non-seasonal forecasters bit-zero; seasonal diffs pure within-task
  permutations.*
- `58a2f4d` fix: cut covid cheating and nssp as-of slices at the generation
  date — **[behavior change]** Mirrors flu's `9c46043`; holiday weeks only.
  *Golden: per-commit — `cutoffs-nxvqulyz` bit-identical at the pinned
  non-holiday date; holiday-week footprint analysis-verified then confirmed
  in the 9-date winter replay.*
- `6ea660a` refactor: covid canonical archives and shared snapshot targets —
  Covid's phases 0/1/4 in one verified step: canonical archives, hoisted
  `full_data`/`nssp_forecast_data`/`nssp_exogenous_data` snapshot targets;
  forecast targets shrink to seed + trim + `run_forecaster`.
  *Golden: per-commit — `archives-yvkntmtz` vs `cutoffs-nxvqulyz`: ALL
  MATCH (archives bit-identical between runs).*
- `f25a100` fix: seed covid stochastic forecasters from semantic cell key —
  **[behavior change]** Covid's `39f3298`.
  *Golden: per-commit — `seeded-yprtznnv`: only the 3 stochastic
  forecasters moved; all 6 deterministic ones bit-zero.*
- `c47d7e2` docs: record covid prod migration to the shared stack.
- `9d01b48` fix: drop NA nssp rows at archive build — **[behavior change]**
  NSSP published explicit NA values for Wyoming (mid-2024 → 2026-01-07),
  which crashed covid replays (`cdc_baseline` on an all-NA series) and made
  flu silently emit garbage wy forecasts in replays. Filtering at archive
  build restores the old wy exclusion for the outage window while leaving
  current-date slices untouched. Unblocked full covid eval replays.
  *Golden: per-commit — flu probe pair (`flu-wy-probe-kylwnluy` vs
  `flu-wy-probe2-vlnyvwnp`, ref 2026-01-07): only the 92 wy garbage rows and
  the wy-ingesting pooled forecaster change. Covid: 9-date winter-window
  old-vs-new (`multibase9` vs `multihead9`), every diff attributed.*
- `4ad8c8c` docs: record nssp NA fix in notes, keep CLAUDE.md a high-level
  overview.

## Ensemble layer: same treatment as the forecasters (07-19)

Each step: `tar_manifest()` pre/post on all four projects + unit tests +
golden captures (flu ref 2026-01-07 n=1; covid ref 2026-02-18 n=9).

- `a3b22d2` refactor: shared ensemble target factory for flu and covid
  prod — E0: the two ~230-line, ~95%-identical ensemble `tar_map`s become
  one `build_prod_ensemble_targets()`; per-disease asymmetries become named
  arguments spliced as literals. Kills the hand-copied fork.
  *Golden: per-commit — `e0-ensemble-skvtwnxz` vs prior baselines, flu and
  covid both ALL MATCH; manifest byte-identical outside the rewritten
  ensemble targets, so no cache invalidation.*
- `ff8ad54` chore: drop dead make_ensemble_grid — No callers anywhere.
  *Golden: n/a (dead code; covered by the E1 golden taken on top of it).*
- `67af21d` refactor: ensemble spec and runner with output contracts — E1:
  declarative per-disease `g_ensemble_specs` executed by `run_ensemble()`
  (component presence asserted loudly — previously a missing component was
  silently averaged over — method dispatch, geo exclusions, id stamping,
  `validate_forecast_output`). Ensemble analog of the forecaster
  spec/runner.
  *Golden: per-commit — `e1-ensemble` vs `e0-ensemble`, flu (n=1) and covid
  (n=9) both ALL MATCH.*
- `3acab1f` feat: validate prod weights csv schema at parse — E3:
  `parse_prod_weights()` validates the hand-edited
  `scripts/*_geo_exclusions.csv` (columns, dates, weights ≥ 0, known ids and
  geos; retired-but-inert ids whitelisted). A typo'd id previously just
  never joined, silently.
  *Golden: per-commit flu — `e3-weights` vs `e1-ensemble` ALL MATCH; covid
  golden deliberately skipped (low-risk, slow 9-date replay). +12 unit
  tests.*
- `eacb052` fix: old ensemble weight typo — The one real typo the validator
  surfaced: `linear_no_population-scale` (hyphen) in
  `flu_nssp_geo_exclusions.csv`'s stale 2025-12-17 block. Inert today, would
  have failed loudly on a historical replay of that date.
  *Golden: none — data-only edit to a superseded date block; inert for all
  current forecast dates by the validator's own scoping.*
- `701fc9d` refactor: stamp covid archives honestly, pass primary_source —
  **[behavior change]** Covid's nssp de-spoof mirroring flu's `5e07703`:
  archives stamp `source`, `forecast_nssp` passes `primary_source = "nssp"`.
  *Golden: per-commit — `despoof-mkvkxsqw` vs `e1-ensemble` (covid, n=9):
  all value columns bit-identical on aligned keys; only the `source` column
  changes. Flu manifest byte-identical (untouched).*

## Scouts, measurements, cleanups (07-19..22)

- `8d739c7` notes: E2 explore-ensemble design scout (recovered stray file) —
  **[idea]** Design for sweeping the shipped prod ensembles in exploration —
  the highest-value open thread; not implemented.
- `1446079` docs: correct output_scale roadmap note (not a live scoring
  bug) — **[experiment]** Magnitude investigation concluded the blanket flu
  `output_scale = "per100k"` is correct today, and that deriving it from
  `pop_scaling` would corrupt flu scores; roadmap wording fixed, no code
  change.
- `7057652` refactor: drop redundant param_names grid column — Verified
  empirically that `tar_map` keeps list-column names, so `param_names` was
  pure duplication; column, argument, and 5 call sites deleted.
  *Golden: per-commit — `param-names-qvqoylsp` vs `e3-weights` (flu) ALL
  MATCH; command text changes everywhere so the golden is the sole check
  (no manifest diff possible).*
- `9aad640` notes: validate_forecast_snapshot design scout — **[idea]**
  Design for an explicit snapshot input validator (and confirmation the
  as-of override is load-bearing for epipredict); not implemented.
- `b753567` notes: BLAS oversubscription timing experiments —
  **[experiment]** Pinning BLAS to 1 thread saves ~7% CPU-seconds but wall
  time is unchanged at evaluation scale; no code change, re-measure at prod
  scale before restructuring.
- `e0a719c` notes: clarity and simplification scout — **[idea]** Verified
  inventory of dead code and duplication for the next cleanup round; the two
  `clean:` commits below execute its first items.
- `451465a` docs: slim CLAUDE.md to current state and open threads.
- `7447d8e` ci: fix R version — Pin the tests workflow to the R version the
  repo actually uses. *Golden: n/a (CI config).*
- `ace2dad` clean: remove dead `g_rsv_*` prod-closure forecasters — Closures
  orphaned by the never-written rsv prod script.
  *Golden: none of its own — dead-code removal verified by rg per the
  scout; covered by the tip-of-branch prod golden below.*
- `ea75903` clean: remove dead and broken utility, scoring, aux-data, and
  formatter functions — ~380 lines of uncalled/broken functions deleted; one
  function used only by a one-off analysis moved into
  `scripts/one_offs/compare_nssp_sources.R`.
  *Golden: none of its own — dead-code removal verified by rg + tests;
  covered by the tip-of-branch prod golden below.*
- `06c38a8` notes: covid+flu prod golden test, main vs ds/refactor2 at
  2026-07-22 — **[experiment]** Full main-vs-branch golden on the real
  forecast Wednesday (`today-main` vs `today-refactor2`, both diseases,
  simultaneous runs in separate jj workspaces): no unexplained diffs; every
  change traces to one of the intentional **[behavior change]** commits
  above. This is the end-to-end validation of the whole branch.
