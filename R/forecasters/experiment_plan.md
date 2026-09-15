# Forecaster Experiment Plan

Planned experiments for improving on `scaled_pop_seasonal`.
Not a commitment schedule — add notes as experiments run.

## Background: What We're Trying to Fix

`scaled_pop_seasonal` uses several overlapping hacks to make quantile regression work across heterogeneous epidemic phases:

- `drop_non_seasons` — hard-filters training rows to the current disease season.
- `seasonal_method = "window"` — soft-filters via a calendar window around the forecast week.
- PCA / climatological features — projects the season shape into the feature space.
- `indicator` — raw before/after-peak binary columns.

The common failure mode: training includes dynamics structurally unlike the current situation (e.g., surge vs. endemic plateau), miscalibrating the learned quantiles.
For COVID especially, calendar position is a weak proxy for epidemic phase.

---

## Direction 1: Nonlinear Quantile Regression

Replace linear QR with a model that learns non-constant conditional distributions without seasonal filters as a crutch.

### 1a. Gradient-boosted quantile regression (LightGBM)

LightGBM with `objective = "quantile"` handles heteroscedasticity and regime-switching without structural assumptions about seasonality.
Tree splits discover relevant subspaces without manual season filtering.

**How to plug in:** call directly after `arx_preprocess` extracts the design matrix; bypass the epipredict `frosting` layer and format output using `run_workflow_and_format`'s pattern.

| id               | features                             | notes                               |
|------------------+--------------------------------------+-------------------------------------|
| `lgbm_q`         | lags only                            | baseline                            |
| `lgbm_q_state`   | lags + state id                      | can it learn geo-level baselines?   |
| `lgbm_q_seas`    | lags + state id + PCA/climatological | does seasonal feature help on top?  |
| `lgbm_q_drop`    | lags + state id + `drop_non_seasons` | does data filter help on top?       |

### 1b. grf::quantile_forest

Prior result was not great, likely due to too many lag features and default hyperparameters.
Worth re-testing with a simpler setup.

| id                  | features                             | notes                                                        |
|---------------------+--------------------------------------+--------------------------------------------------------------|
| `grf_q`             | lags {0,1,2} only                    | honest splitting baseline                                    |
| `grf_q_state`       | lags {0,1,2} + state as cluster id   | `clusters =` arg groups by geo without one-hotting           |
| `grf_q_state_seas`  | lags {0,1,2} + state id + season     | does seasonal signal add on top?                             |

Hyperparams to tune: `num.trees` ∈ {500, 1000, 2000}; `min.node.size` ∈ {5, 10}.

### 1c. Neural quantile regression (simple MLP)

Can share parameters across geo_values and handle the lag structure naturally.
Risk: data-hungry relative to ~3 seasons of training signal per geo.
Implement via `torch` in R or the `brulee` parsnip backend.

---

## Direction 2: Classifier-Based Example Filtering

Replace hand-coded calendar filters with a selector that identifies training examples whose epidemic dynamics resemble the current forecast context.

For each forecast date, embed the last `W` weeks as a feature vector.
Select training rows nearest to the query embedding; fit QR only on those.
This is a learnable, phase-aware replacement for `seasonal_method = "window"`.

The key features are: smoothed level (3–4 week rolling mean) and smoothed growth rate (week-over-week diff of the rolling mean).
These are robust to weekly noise and data revision without needing a learned encoder.
Adding the second difference (curvature) catches inflection points — peak vs. early-rise at the same level.

### 2a. k-NN window selection

Embed each training window as (smoothed level, smoothed Δ, optionally curvature).
At forecast time, select the `k` nearest training windows by distance; fit QR on those rows only.

Can be dropped into `scaled_pop_seasonal` as a pre-filter step before `arx_preprocess` — the `season_data` / `epi_data` split already exists for this purpose.

**Parameters to sweep:**

- `k` ∈ {20, 50, 100, adaptive by threshold}
- Embedding: level only; level + Δ; level + Δ + curvature
- Distance: L2, cosine

### 2b. Propensity-weighted QR

Fit a propensity score for "similar to current query" and reweight training examples via inverse-propensity weighting in the QR loss.
Smoother than hard-`k` selection.
Requires bypassing `quantile_reg()` and calling `quantreg::rq()` directly with `weights=` — same pattern as the LightGBM bypass.

### 2c. Learned encoder for window similarity

If hand-crafted (level, Δ, curvature) embeddings are insufficient, replace them with a small 1D conv encoder trained via contrastive loss (SimCLR-style).

**Architecture:** input is a length-`W` (W ≈ 8 weeks) window of scaled values.
One 1D conv layer, kernel size 2–3, 4–8 filters, ReLU, global average pool → L2-normalised embedding of dim 4–8.
Parameter count must stay in the **tens**: ~52 states × 52 weeks × 4 years ≈ 10k windows is not a large dataset for contrastive learning.

**Training:** two augmented views per window (Gaussian noise, random offset); NT-Xent loss.
Train once offline on the full archive; reload weights at forecast time.
Implement via `torch` in R.

**At forecast time:** drop-in replacement for the hand-crafted embedding in 2a.

---

## Direction 3: Vector AR Across States

Current forecasters treat each geo_value independently.
State counts are spatially correlated (regional spread, shared policy shocks); a VAR or factor model could borrow strength across states.

### 3a. Factor model + per-state residual QR

Extract `r` latent national/regional factors via PCA on the panel, fit AR on the factors, and fit individual QR on geo-level residuals.
Forecast = factor contribution + residual QR.

Factors are pre-computed as auxiliary data and passed as `extra_sources` into `scaled_pop_seasonal` — near-zero new infrastructure.

**Implementation:**
1. Extract factors from the `epi_archive` snapshot at forecast time.
2. Pass factor forecasts as `extra_sources` columns into `scaled_pop_seasonal`.

Limitation: PCA factors are a linear projection and won't capture nonlinear spatial heterogeneity.

### 3b. Panel VAR (state-level)

Fit a VAR(`p`) jointly across all states.
50+ states → 50×50 coefficient matrix per lag → badly underdetermined without regularization (`BigVAR` package provides elastic-net VAR).
Quantiles require bootstrapping residuals.

Scoped-down version: group states into HHS regions and fit a 10-dimensional VAR.

**Parameters:** `p` ∈ {1, 2, 4}; regularization ∈ {ridge, lasso, elastic net}; individual states vs. HHS regions.

---

## Direction 4: Direction-Conditional Forecasting (revived from covid-hosp-forecast)

A two-stage system: label history with a direction detector, then train a classifier to predict future direction, then condition the quantile forecast on the predicted direction.

This is likely a method that would benefit from using versioned data and not just the finalized data. We should do a comparison of the two to see if it helps.

### Direction detection

Label each (geo, week) as `up` / `steady` / `down` / `low` using a rolling-sum relative-change comparison.

- Compute a 2-week rolling sum of admissions.
- Compare the rolling sum now to the rolling sum 2–4 weeks ahead (choice of horizon is a parameter).
- Relative change above some threshold → `up`; below the symmetric inverse → `down`; otherwise `steady`.
- If the lagging 2-week sum is below some activity threshold → `low` (censor; don't classify).
- For flu: the `low` threshold is floored at a high percentile (e.g. 90th) of the rolling sum during June–September, so the known-inactive summer period is captured by the threshold itself rather than a hard calendar override.

Rather than porting the original hard-coded thresholds (which were tuned for daily COVID data and won't translate to weekly NHSN scale or flu), learn the bucket boundaries from the data:

- **`low` threshold:** a low quantile (e.g. 10th–20th percentile) of the 2-week rolling sum distribution across all geos and dates.
- **`up`/`down` thresholds:** symmetric quantiles of the relative-change distribution (e.g. top/bottom tercile or quartile), computed on the training window.

This automatically adapts to disease and scale, and makes the bucket sizes roughly balanced rather than dependent on arbitrary constants.
The quantiles themselves become tunable parameters to sweep.

In the revised case, this part should all be on finalized data, and stop ~5w before the forecast date.

### Classifier

Multinomial logistic lasso (`glmnet`) predicting direction class from ratio features.

Original features were `signal(t) / signal(t − k)` at lags `k ∈ {0, 7, 14}` days with optional 7-day smoothing.
Weekly adaptation: ratios at lags `k ∈ {0, 1, 2}` weeks; smoothing is less critical since the data is already weekly.

Returns a probability distribution over the 4 classes per geo per ahead.

In the revised case, the features should be as constructed in `archive_to_revision_predictors` (which should probably move to a different file).

### How to use the classifier output

Three options in increasing complexity:

1. **As features:** pass the predicted direction probabilities as `extra_sources` into `scaled_pop_seasonal`.
   Minimal new infrastructure; the QR learns to use them if they're informative.
2. **As a training filter (links to Direction 2):** select training windows whose *observed* direction label matches the *predicted* direction at forecast time.
   Supervised version of the kNN filter in 2a.
3. **Direction-conditional models:** fit a separate QR per direction class, route forecasts through whichever model the classifier predicts.
   Most expressive but multiplies the number of models; only worth it if class-conditional residuals are structurally different.

1 isn't really worth it, as basically any ARIMA worth using will already have that info baked-in. 2 is the main method the previous attempts used. We should focus on 3. Effectively this is an ensemble model where each has specific biases, which we combine in a principled way.

### Weekly adaptation notes

- Rolling sum window and comparison horizon both need re-tuning; daily windows don't translate directly.
- The `low` threshold needs recalibration against weekly NHSN totals (original ~130 admissions/14 days ≈ 9/day is a different scale than weekly hospital admissions).
- Ratio features at weekly lags are already de-noised relative to daily, so the optional smoothing step is lower priority.

---

### Testing
This has another layer of testing where we should run it on the entire archive and check both the prediction accuracy
## Sanity Checks

Before touching the explore pipeline, verify each experiment at two levels.

### Single-state smoke test

Fit on a single state (e.g. PA — mid-size, no major reporting gaps) across a handful of forecast dates.
Check: correct output shape, training set selected by the new method looks sensible when plotted against the query window, WIS not catastrophically worse than linear QR baseline.
Cheap enough to run in a REPL session.

### Synthetic dynamics test (Direction 2)

Single-state tests can't confirm whether a selector recovers similar *dynamics* vs. nearby *calendar dates*.

Construct a synthetic `epi_df` with two regimes spliced together (sinusoidal surge, flat endemic stretch, second surge).
At a query point in the second surge, the selector must pull from the first surge — not the endemic stretch, even if it is more recent.
Write as a standalone script in `_local/`.

---

## Evaluation Protocol

Evaluate on flu and COVID holdout sets via `make eval-flu` / `make eval-covid`.
Use `EVALUATION_N_DATES` to limit scope during iteration.

Primary metric: WIS by ahead, aggregated across geos.
Secondary: coverage at 50% and 95% PI; calibration plots by season week.

Baselines:
- `scaled_pop_seasonal` with `seasonal_method = "window"` (current best)
- `scaled_pop_seasonal` with `seasonal_method = "climatological"`
- `flatline_fc` (sanity floor)

Register new forecasters in `g_forecaster_parameter_combinations` in `scripts/*_hosp_explore.R` before running explore sweeps.

---

## Status

| experiment              | status   | notes                     |
|-------------------------+----------+---------------------------|
| lgbm_q (1a)             | todo     |                           |
| grf re-run (1b)         | todo     | narrow lags, more trees   |
| knn filter (2a)         | todo     |                           |
| factor + residual (3a)  | todo     | easiest VAR entry point   |
| panel VAR (3b)          | todo     | after 3a                  |
| MLP QR (1c)             | backlog  |                           |
| propensity QR (2b)      | backlog  | after 2a                  |
| conv encoder (2c)       | backlog  | if 2a embedding falls short |
| direction classifier as features (4, opt 1) | todo | port from covid-hosp-forecast, recalibrate thresholds |
| direction classifier as filter (4, opt 2)   | backlog | after opt 1 |
| direction-conditional models (4, opt 3)     | backlog | after opt 2 |
