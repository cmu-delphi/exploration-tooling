# Exploration Tooling

This repo is for exploring forecasting methods and tools for both COVID and Flu.
The repo is structured as a [targets](https://docs.ropensci.org/targets/) project, which means that it is easy to run things in parallel and to cache results.
The repo is also structured as an R package, which means that it is easy to share code between different targets.

## Production Usage 2024-2025

The pipeline should run on a schedule and by ~10:45AM PST time, you should find the new reports on https://delphi-forecasting-reports.netlify.app/.
If not, see the instructions below for manual running.

Define run parameters in your `.Renviron` file:

```sh
EPIDATR_USE_CACHE=true
# Choose a cache timeout for yourself. We want a long cache time, since we work with historical data.
EPIDATR_CACHE_MAX_AGE_DAYS=42
DEBUG_MODE=false
DUMMY_MODE=false
USE_SHINY=false
TAR_PROJECT=covid_hosp_explore
FLU_SUBMISSION_DIRECTORY=cache
COVID_SUBMISSION_DIRECTORY=cache
EXTERNAL_SCORES_PATH=legacy-exploration-scorecards.qs
AWS_S3_PREFIX=exploration
AUX_DATA_PATH=aux_data
```

- `EPIDATR_USE_CACHE` controls whether `epidatr` functions use the cache.
- `DEBUG_MODE` controls whether `targets::tar_make` is run with the `callr_function=NULL`, which allows for debugging. This only works if parallelization has been turned off in `scripts/targets-common.R` by setting the default controller to serial on line 51.
- `DUMMY_MODE` controls whether all forecasters are replaced with a dummy. This is useful for testing a new pipeline.
- `USE_SHINY` controls whether we start a Shiny server after producing the targets.
- `TAR_PROJECT` controls which `targets` project is run by `run.R`. Likely either `covid_hosp_explore` or `flu_hosp_explore`
- `SUBMISSION_DIRECTORY` the path to where production forecasts should be saved to be submitted to the hub.
- `EXTERNAL_SCORES_PATH` controls where external scores are loaded from. If not set, external scores are not used.
- `AWS_S3_PREFIX` controls the prefix to use in the AWS S3 bucket (a prefix is a pseudo-directory in a bucket).
- `AUX_DATA_PATH=aux_data`

Run the pipeline using:

```sh
# Install renv and R dependencies
make install

# Pull pre-scored forecasts from the AWS bucket
make pull

# Make forecasts
make prod-flu
make prod-covid

# The job output can be found in nohup.out
# If there are errors, you can view the top n with the following command (replace with appropriate project)
source("scripts/targets-common.R");
get_targets_errors("covid_hosp_prod", top_n = 10)

# Automatically append the new reports to the site index and host the site on netlify
# (this requires the netlify CLI to be installed and configured, talk to Dmitry about this)
make update_site && make netlify

# Update weights until satisfied using *_geo_exclusions.csv, rerun the make command above
# Submit (makes a commit, pushes to our fork, and makes a PR; this requires a GitHub token
# and the gh CLI to be installed and configured, talk to Dmitry about this)
make submit-flu
make submit-covid
```

## Development

### Directory Layout

- `run.R` and `Makefile`: the main entrypoint for all pipelines
- `R/`: R package code to be reused
- `scripts/`: plotting, code, and misc.
- `tests/`: package tests
- `covid_hosp_explore/` and `scripts/covid_hosp_explore.R`: a `targets` project for exploring covid hospitalization forecasters
- `flu_hosp_explore/` and `scripts/flu_hosp_explore.R`: a `targets` project for exploring flu hospitalization forecasters
- `covid_hosp_prod/` and `scripts/covid_hosp_prod.R`: a `targets` project for predicting covid hospitalizations
- `flu_hosp_prod/` and `scripts/flu_hosp_prod.R`: a `targets` project for predicting flu hospitalizations
- `forecaster_testing/` and `scripts/forecaster_testing.R`: a `targets` project for testing forecasters

### Debugging

Insert a `browser()` statements into the code you want to debug and then run

```r
# use_crew=FALSE disables parallelization
tar_make(target_name, callr_function = NULL, use_crew = FALSE)
```

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
