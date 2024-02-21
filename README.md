# Exploration Tooling

This repo is meant to be a place to explore different forecasting methods and tools for both COVID and flu.
The repo is structured as a [targets](https://docs.ropensci.org/targets/) project, which means that it is easy to run things in parallel and to cache results.
The repo is also structured as an R package, which means that it is easy to share code between different targets.

## Usage

Define run parameters:

```sh
# Save to your `.Renviron` file:
EPIDATR_USE_CACHE=true
# not strictly necessary, but you probably want a long cache time, since this is for the historical data
EPIDATR_CACHE_DIR=~/.epidatr-cache
EPIDATR_CACHE_MAX_AGE_DAYS=42
DEBUG_MODE=true
USE_SHINY=false
TAR_PROJECT=covid_hosp_explore
EXTERNAL_SCORES_PATH=legacy-exploration-scorecards.qs
AWS_S3_PREFIX=exploration
```

-   `EPIDATR_USE_CACHE` controls whether `epidatr` functions use the cache.
-   `DEBUG_MODE` controls whether `targets::tar_make` is run with the `callr_function=NULL`, which allows for debugging. This only works if parallelization has been turned off in `scripts/targets-common.R` by setting the default controller to serial on line 51.
-   `USE_SHINY` controls whether we start a Shiny server after producing the targets.
-   `TAR_PROJECT` controls which `targets` project is run by `run.R`. Likely either `covid_hosp_explore` or `flu_hosp_explore`
-   `EXTERNAL_SCORES_PATH` controls where external scores are loaded from. If not set, external scores are not used.
-   `AWS_S3_PREFIX` controls the prefix to use in the AWS S3 bucket (a prefix is a pseudo-directory in a bucket).

Run the pipeline using:

```sh
# Install renv and R dependencies.
make install

# Pull pre-scored forecasts from the AWS bucket
make download
# or
make pull

# Run only the dashboard, to display results run on other machines
make dashboard

# Run the pipeline using the helper script `run.R`
make run
# or in the background
make run-nohup

# Upload/push complete or partial results to the AWS bucket
make upload
# or
make push
```

## Development

### Directory Layout

-   `run.R` and `Makefile`: the main entrypoint for all pipelines
-   `R/`: R package code to be reused
-   `scripts/`: plotting, code, and misc.
-   `tests/`: package tests
-   `covid_hosp_explore/` and `covid_hosp_explore.R`: a `targets` project for exploring covid hospitalization forecasters
-   `flu_hosp_explore/` and `flu_hosp_explore.R`: a `targets` project for exploring flu hospitalization forecasters
-   `covid_hosp_prod/` and `covid_hosp_prod.R`: a `targets` project for predicting covid hospitalizations
-   `flu_hosp_prod/` and `flu_hosp_prod.R`: a `targets` project for predicting flu hospitalizations
-   `forecaster_testing/` and `forecaster_testing.R`: a `targets` project for testing forecasters

### Parallelization Gotchas

When running a pipeline with parallelization, make sure to install the package via `renv::install(".")` and not just via `devtools::load_all()`.
It is safest to develop with parallelism disabled.

### Pipeline Design

See [this diagram](https://excalidraw.com/#json=AmMzzAKxSF5rz1dvuDJxj,0b53_5Ro6xwm13uQXrIGMQ).
Double diamond objects represent "plates" (to evoke [plate notation](https://en.wikipedia.org/wiki/Plate_notation), but don't take the comparison too literally), which are used to represent multiple objects of the same type (e.g. different forecasters).

## Notes on Forecaster Types

### Basic

The basic forecaster takes in an epi_df, does some pre-processing, does an epipredict workflow, and then some post-processing

### Ensemble

This kind of forecaster has two components: a list of existing forecasters it depends on, and a function that aggregates those forecasters.

### (to be named)

Any forecaster which requires a pre-trained component. An example is a forecaster with a sophisticated imputation method. Evaluating these has some thorns around training/testing splitting. It may be foldable into the basic variety though.
