# Exploration Tooling

This repo is meant to be a place to explore different forecasting methods and tools for both COVID and flu.
The repo is structured as a [targets](https://docs.ropensci.org/targets/) project, which means that it is easy to run things in parallel and to cache results.
The repo is also structured as an R package, which means that it is easy to share code between different targets.

## Usage

```sh
# Install renv and R dependencies.
make install

# Set your .Renviron settings.
EPIDATR_USE_CACHE=true
DEBUG_MODE=true
USE_SHINY=false
TAR_PROJECT=covid_hosp_explore
EXTERNAL_SCORES_PATH=

# Run the pipeline wrapper run.R.
make run
```

-   `EPIDATR_USE_CACHE` controls whether `epidatr` functions use the cache.
-   `DEBUG_MODE` controls whether `targets::tar_make` is run with the `callr_function=NULL`, which allows for debugging.
-   `USE_SHINY` controls whether we start a Shiny server after producing the targets.
-   `TAR_PROJECT` controls which `targets` project is run by `run.R`.
-   `EXTERNAL_SCORES_PATH` controls where external scores are loaded from. If not set, external scores are not used.

## Development

### Directory Layout

-   `run.R` and `Makefile`: the main entrypoint for all pipelines
-   `R/`: R package code to be reused
-   `extras/`: plotting, code, and misc.
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
