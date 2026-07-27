# Exploration Tooling

This repo produces Delphi's COVID, flu, and RSV hospitalization forecasts
(submitted to the CDC forecast hubs as "CMU-TimeSeries") and hosts the
exploration sweeps used to develop them. It is organized as several
[targets](https://docs.ropensci.org/targets/) pipeline projects sharing one R
codebase, which makes it easy to run things in parallel and cache results.

This README is the human-facing guide: setup, weekly production operation,
exploration runs, and how to add a forecaster. `CLAUDE.md` holds the terse
working reference (command cheatsheet, REPL debugging, architecture map, and
the refactor roadmap); `notes/` holds dated work logs.


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
# If you're on a production machine, set these to the path of the directory where you want to save the submission.
FLU_SUBMISSION_DIRECTORY=cache
COVID_SUBMISSION_DIRECTORY=cache
# Set to the prefix of the AWS S3 bucket where you want to save the exploration data.
AWS_S3_PREFIX=exploration
# Set to the path of the directory where you want to save the auxiliary data.
AUX_DATA_PATH=aux_data
```

Do NOT set `TAR_PROJECT` in `.Renviron`: R re-loads `.Renviron` on every
`Rscript` start (including the callr subprocess `tar_make` spawns), and its
values override the shell environment — a default here silently redirects any
invocation that selects a project via `export TAR_PROJECT=...`. Make recipes
select projects via `TAR_RUN_PROJECT` (see `scripts/run.R`); in a REPL, start
with `Sys.setenv(TAR_PROJECT = "<project>")` (options in `_targets.yaml`).

Run the pipeline using:

```sh
# Install R 4.4.1 (we recommend https://github.com/r-lib/rig)
# Install renv and R dependencies
make install

# Sync data and forecasts from the AWS bucket
# - downloads the aux_data/ folder which contains non-public data used by some forecasters
# - downloads previously generated forecasts, to save recomputing them
# Requires the AWS CLI to be installed and configured
# Reference: https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3/index.html
# Installation instructions: https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html
make pull

# Make forecasts
make prod-flu
make prod-covid
# (make prod-rsv is a stub: scripts/rsv_hosp_prod.R is not written yet)

# If there are errors, view the top n with (in an R session; replace with appropriate project):
#   suppressPackageStartupMessages(source("R/load_all.R"))
#   get_targets_errors("covid_hosp_prod", top_n = 10)
# or use the make shortcut, e.g.
make get-flu-prod-errors

# Automatically append the new reports to the site index and host the site on netlify
# (this requires the netlify CLI to be installed and configured, talk to Dmitry about this)
make update-site && make netlify

# Update weights until satisfied using *_geo_exclusions.csv, rerun the make command above
# Submit (makes a commit, pushes to our fork, and makes a PR; this requires a GitHub token
# and the gh CLI to be installed and configured, talk to Dmitry about this)
make submit-flu
make submit-covid

# Push your forecasts to the AWS bucket (requires AWS CLI)
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

This repo is organized as a monorepo with multiple `targets` projects,
declared in `_targets.yaml`. Each project maps a pipeline script in `scripts/`
to a store directory (targets cache) of the same name:

- `covid_hosp_explore`, `flu_hosp_explore`: exploration sweeps over many
  forecaster/parameter combinations
- `covid_hosp_prod`, `flu_hosp_prod`: weekly production forecasts and reports
- `rsv_hosp_prod`: a stub — its Makefile recipes and `_targets.yaml` entry
  point at a not-yet-written `scripts/rsv_hosp_prod.R` (not a priority)
- `flu_hosp_evaluation`: historical replay + scoring; same script as flu prod
  but a separate store, so replays don't invalidate the production cache

### Directory Layout

- `scripts/run.R` and `Makefile`: the main entrypoints for all pipelines
- `R/`: reusable R code for forecasters, targets, and data processing functions
- `scripts/`: pipeline definitions (one per project), archive-building and
  data-processing scripts, and report generation scripts
- `tests/`: package tests
- `notes/`: dated work logs
- `deploy/`: systemd units for the archive pollers and scheduled prod runs

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

Pipelines can also be run and debugged interactively from the R REPL
(single-target debugging with `browser()`, error inspection, forecaster
lookup): see "REPL workflow" in `CLAUDE.md`.

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

Exploration and production share one runner. A forecaster is a bare function
`fn(epi_data, outcome, ahead, ...)`; everything that is a cross-cutting
convention rather than a modeling parameter is declared as a *spec column* on
its grid row and applied uniformly by `run_forecaster()` (in `map()` over dates
for explore, once per `(forecaster, date)` target for prod). The spec columns
and their defaults live in `FORECASTER_SPEC_DEFAULTS` (`R/utils.R`):
`as_of_policy` (`"asof"` real-time vs `"cheating"` finalized-with-cutoff),
`ahead_multiplier` (1 for day-native, 7 for week-native forecasters),
`target_date_shift`, `join_extra_data`, `filter_sources`, `excluded_geos`,
`sort_quantiles` (the flu quantile-whitening workaround), and `output_scale`
(`"count"` vs `"per100k"`, controlling whether scoring rescales to counts).
`make_forecaster_grid()` splits these off from the params list-column and fills
defaults, so a grid row omits whatever it doesn't need; explore rides the
defaults while prod forecasters declare their overrides inline in
`g_forecaster_params_grid` (see `scripts/flu_hosp_prod.R`). Because both
pipelines consume the same grid and runner, a forecaster that works in an
exploration sweep runs verbatim in backtesting and prod — there is no separate
per-disease prod closure to keep in sync. (Covid and RSV prod are still on the
older per-disease wiring pending their own migration; see "Shared forecaster
architecture" in `CLAUDE.md` for status and roadmap.)

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
