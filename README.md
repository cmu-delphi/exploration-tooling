# Existing tools

- [batchtools](https://mllg.github.io/batchtools/)
  - probably worth comparing the save output format with that generated by hubutils
  - definitely seems like it will save a lot of headache on exploration; probably not as useful for actual live forecasting
  - their "algorithm" should I think correspond to a forecaster
  - any `problem`s we add that modify the data should do so by returning the modified version in instance, rather than as access functions.
- [hubutils](https://infectious-disease-modeling-hubs.github.io/hubUtils/index.html)
  - sort of a different direction focused more on aggregating results from several places. I think the output format is something I should target; file format of parquet
- [epiforecasts](https://github.com/epiforecasts)
  - another group, they have a scoring utils package
  - [scoringutils](https://epiforecasts.io/scoringutils/)
    - it does not. For quantile models, they expect ‘true_value’, ‘prediction’, ‘quantile’
    - hubverse expects 'output_type' 'output_type_id' and 'value'
    - easy enough to map between them though

# Things I definitely need:

- a way to produce forecasts
  this should also be easily used in production
- a way to score forecasts
  - I currently have one that only does WIS; I think switching to scoringutils wouldn't take much time at all
- a way to compare scores

currently, I'm producing forecasts and evaluating at the same time. Actually, no I'm not. I'm first doing an `epix_slide` to produce forecasts, and then

- parallel over forecasterXahead definitions:
  - for each (forecaster,ahead):
    - generate forecast
    - evaluate forecast
    - save

# Kinds of forecasters
## Basic
The basic forecaster takes in an epi_df, does some pre-processing, does an epipredict workflow, and then some post-processing
## Ensemble
This kind of forecaster has two components: a list of existing forecasters it depends on, and a function that aggregates those forecasters.
## (to be named)
Any forecaster which requires a pre-trained component. An example is a forecaster with a sophisticated imputation method. Evaluating these has some thorns around training/testing splitting. It may be foldable into the basic variety though.
# later things
- a way to check that a given function is or is not in the right format to be a forecaster


# Random notes
Currently, to run in parallel, you need to install the package via `renv::install(".")`.
The parallel workers will continue to use the version as of the last time you ran `renv::install`, while the non-parallel ones won't. This separates development from exploration.
