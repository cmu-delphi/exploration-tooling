# Exploration Tooling

This repo is for exploring forecasting methods and tools for both COVID and Flu.
The repo is structured as a [targets](https://docs.ropensci.org/targets/) project, which means that it is easy to run things in parallel and to cache results.
The repo is also structured as an R package, which means that it is easy to share code between different targets.

## Production Usage 2024-2025

The pipeline should run on a schedule and by ~9:45AM PST time, you should find the new reports on https://delphi-forecasting-reports.netlify.app/.
If not, see the instructions below for manual running.

Define these parameters in your `.Renviron` file:

```sh
# Choose whether to use the epidatr cache. Generally should be safe, since it's used for historical data only.
# The main gotcha otherwise is to make sure to clear it if you know the data has been revised.
# Here we set the cache to last 42 days.
EPIDATR_USE_CACHE=true
EPIDATR_CACHE_MAX_AGE_DAYS=42
# Controls whether all forecasters are replaced with a dummy. This is useful for testing a new pipeline.
DUMMY_MODE=false
# Set to the project you want targets to use by default. The options can be found in `_targets.yaml`.
TAR_PROJECT=covid_hosp_explore
# If you're on a production machine, set these to the path of the directory where you want to save the submission.
FLU_SUBMISSION_DIRECTORY=cache
COVID_SUBMISSION_DIRECTORY=cache
# Set to the prefix of the AWS S3 bucket where you want to save the exploration data.
AWS_S3_PREFIX=exploration
# Set to the path of the directory where you want to save the auxiliary data.
AUX_DATA_PATH=aux_data
```

Run the pipeline using:

```sh
# Install R 4.4.1 (we recommend https://github.com/r-lib/rig)
# Install renv and R dependencies
make install

# Pull various data used by the forecasters from the AWS bucket
# Requires the AWS CLI to be installed and configured
# Reference: https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3/index.html
# Installation instructions: https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html
make pull

# Make forecasts
make prod-flu
make prod-covid

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

# Push the data to the AWS bucket (requires AWS CLI)
make push
```

## Exploration Usage

Exploration usage is similar to production usage, but much bulkier. It tests
many forecaster ideas by sweeping over many parameter settings. The typical time
to run is about 3 hours per disease.

```sh
# Install renv and R dependencies
make install

# Run the pipeline
make explore-flu
make explore-covid

# If you anticipate running the pipeline for a long time and possibly logging off, you can run the pipeline with nohup
# The job output can be found in nohup.out
nohup make explore-flu &
nohup make explore-covid &
```

## Development Overview

This repo is organized as a monorepo with multiple `targets` projects.
The four targets projects are:

- `covid_hosp_explore`: for exploring covid hospitalization forecasters
- `flu_hosp_explore`: for exploring flu hospitalization forecasters
- `covid_hosp_prod`: for predicting covid hospitalizations
- `flu_hosp_prod`: for predicting flu hospitalizations

Each of these projects has its own script file that defines the pipeline.
These are in the `scripts/` directory.

### Directory Layout

- `run.R` and `Makefile`: the main entrypoint for all pipelines
- `R/`: reusable R code for forecasters, targets, and data processing functions
- `scripts/`: entry-points for target pipelines, one-off data processing
  scripts, and report generation scripts
- `tests/`: package tests
- `covid_hosp_explore/` and `scripts/covid_hosp_explore.R`: a `targets` project for exploring covid hospitalization forecasters
- `flu_hosp_explore/` and `scripts/flu_hosp_explore.R`: a `targets` project for exploring flu hospitalization forecasters
- `covid_hosp_prod/` and `scripts/covid_hosp_prod.R`: a `targets` project for predicting covid hospitalizations
- `flu_hosp_prod/` and `scripts/flu_hosp_prod.R`: a `targets` project for predicting flu hospitalizations
- `forecaster_testing/` and `scripts/forecaster_testing.R`: a `targets` project for testing forecasters

### Pipeline Structure

The structure of each of these projects is the same:

- All global variables at the top are prefixed with `g_` and are used in the
  functions that build the targets (for complicated reasons involving deferred
  execution, we are not able to use function arguments to pass variables to the
  functions that build the targets).
- The `g_forecast_generation_date` variable is used to control the date when the
  forecast is run. This is in contrast to `g_forecast_dates`, which is a vector
  of the nominal dates for which forecasts are generated. If all things go well,
  these two are the same value. However, sometimes, due to holiday delays or
  data outages, the forecast generation date is delayed. In this case, it's
  important to record this deviation from a weekly pattern, so that in the
  future we can use `g_forecast_generation_date` as the correct as of date.
- `g_forecaster_parameter_combinations` is a human-readable tibble of
  forecasters and the parameter settings we want to use for each. In exploration,
  this typically is a sweep over many parameter settings, searching for the best
  performing ones.
- `g_forecaster_params_grid` contains the same data as
  `g_forecaster_parameter_combinations`, but in a format that `targets` can use
  to generate the targets for each forecaster.

Some general tips:

- Do not rely on a forecasting pipeline to get the data you need. Forecasting
  pipelines are bulky and are not designed for fast polling, which is the
  opposite of what you want from a fast fetching script. Instead, write a
  script that polls your source frequently, gets the raw data, and builds an
  archive. See `scripts/build_nhsn_archive.R` for an example of how to do this.
  Then, you can make your forecasting pipeline depend on this archive, which
  (assuming everything went well) will be up to date.

### Running and debugging pipelines

You can run these pipelines as described above, but you can also run them in the R REPL.

```r
suppressPackageStartupMessages(source("R/load_all.R"))

# Make sure to set the project to the one you want to run
Sys.setenv(TAR_PROJECT = "covid_hosp_explore")

# Run the pipeline
tar_make()
```

Frequently, you will want to run and debug a single target. First place a
`browser()` statement in the target function. Then run the following command:

```r
tar_make(target_name, callr_function = NULL, use_crew = FALSE)
```

This will run the pipeline up to and including the target you specified and drop
you into the R debugger.

### Adding a new forecaster

To add a new forecaster, we recommend copying the `scaled_pop` forecaster and
modifying it to suit your needs. What follows is a brief introduction to the
structure of a typical forecaster function. First, add a new forecaster function
in `R/forecasters/` with the following general format:

```r
function(epi_data,
         outcome,
         ahead = 1,
         ...)
```

The `epi_data` argument is the data frame of epidemic data. You can typically
assume that this is an `epi_df` object (with all the standard columns, like
`geo_value`, `time_value`, `version`, etc.). The `outcome` argument is the
column name of the outcome variable that you want to forecast. The `ahead`
argument is the number of time steps ahead to forecast. The `...` argument is
for additional arguments that are passed to the forecaster.

Some other common arguments are:

- `extra_sources`: a string of column names of additional data to use for the
  forecast. This is often empty, but useful if you have exogenous variables.
- `filter_source`: a string of the source to filter to. Most of the time, it
  should just be a string like "nhsn", but if you want to use augmented data
  (i.e. treat data from other sources as additional rescaled observations for
  "NHSN" data), you can leave this as "", which won't filter anything. This
  allows us to have a single dataset (such as `joined_archive_data`) that's
  passed uniformly to all forecasters and have them select which data they want
  to use.
- `...`: any other arguments are passed to `default_args_list` to control the
  training and prediction process of epipredict (such as lags, quantile levels,
  nonneg, etc.)

Second, add your new forecaster to the `g_forecaster_parameter_combinations`
variable. This is a tibble that describes the different parameter settings you
want to try for your forecaster.

At this point, we would recommend commenting out most of the other forecasters
and reducing the forecast dates to a few dates. This will help you iterate
faster. Run the pipeline until your forecaster produces sensible results. If you
add your forecaster under a new heading in
`g_forecaster_parameter_combinations`, it will get a new report notebook in
`reports/` (such as `reports/flu-notebook-new_forecaster.html`).

### Some handy utilities

We have a few utility functions that are useful for developing new forecasters.

```r
# Look up the forecaster and its parameter settings in the grid, using
# its code name (the prefix "forecast_" is ignored)
forecaster_lookup("forecast_surprised.tarantula")
forecaster_lookup("surprised.tarantula")

# Get the errors from a targets pipeline
get_targets_errors("covid_hosp_explore", top_n = 10)

# Look a tibble of the files we have on S3. Requires access to the Delphi
# S3 bucket. See "R/utils.R" for more.
df <- get_bucket_df_delphi(bucket = "forecasting-team-data", prefix = "nhsn_data_raw")
df
```

### Tests

The tests can be run with `make test`. We try to test the utility functions as
much as we can. Due to the nature of data work, testing forecasting functions is
hard and tends to be done by examining results. We made an attempt to add
synthetic data tests, but they were very slow and not very useful.
