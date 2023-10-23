# Exploration Tooling

This repo is meant to be a place to explore different forecasting methods and tools for doing so.
The goal is to unify COVID forecasting and flu forecasting in one repo.
The repo is structured as a [targets](https://docs.ropensci.org/targets/) project, which means that it is easy to run things in parallel and to cache results.
The repo is also structured as an R package, which means that it is easy to share code between different targets.

## Usage

```sh
# Install renv and R dependencies.
make install

# Run the pipeline wrapper run.R.
make run
```

## Directory Layout

-   `R/`: R package code to be reused
-   `extras/`: plotting and notebook code
-   `covid_hosp_explore/`: a `targets` project for exploring covid hospitalization forecasters
-   `flu_hosp_explore/`: a `targets` project for exploring flu hospitalization forecasters
-   `covid_hosp_prod/`: a `targets` project for predicting covid hospitalizations
-   `flu_hosp_prod/`: a `targets` project for predicting flu hospitalizations
-   `testing`: for debugging forecasters and doing sanity checks

## Tricky Gotchas

Currently, to run in parallel, you need to make sure to install the package via `renv::install(".")` and not just via `devtools::load_all()`.
Therefore we recommend developing serially, but running exploration in parallel.

## Pipeline Design

See [this diagram](https://excalidraw.com/#room=85f8bfeb397ddf29f110,q8nOcBql7ACvhgCyjXu98g).
Double diamond objects represent plates (to evoke [plate notation](https://en.wikipedia.org/wiki/Plate_notation), but don't take the comparison too literally), which are used to represent multiple objects of the same type (e.g. different forecasters).

## Notes on Forecaster Types

### Basic

The basic forecaster takes in an epi_df, does some pre-processing, does an epipredict workflow, and then some post-processing

### Ensemble

This kind of forecaster has two components: a list of existing forecasters it depends on, and a function that aggregates those forecasters.

### (to be named)

Any forecaster which requires a pre-trained component. An example is a forecaster with a sophisticated imputation method. Evaluating these has some thorns around training/testing splitting. It may be foldable into the basic variety though.
