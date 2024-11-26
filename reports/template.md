# Forecast Reports

[GitHub Repo](https://github.com/cmu-delphi/explorationt-tooling/)

## Production Reports


## Exploration Reports

- [NHSN 2024-2025 Data Analysis](new_data.html)
- [Flu Overall](flu-overall-notebook.html)
- [Flu AR with population scaling (quantile reg)](flu-notebook-scaled_pop_main.html)
- [Flu AR with population scaling and data augmented](flu-notebook-scaled_pop_data_augmented.html)
- [Flu AR with population scaling, data augmented, and exogenous features](flu-notebook-scaled_pop_exogenous.html)
- [Flu AR with seasonal features](flu-notebook-scaled_pop_season.html)
- [Flu AR with seasonal features and data augmented](flu-notebook-scaled_pop_season_data_augmented.html)
- [Flu no recent](flu-notebook-no_recent_quant.html)
- [Flu flatline](flu-notebook-flatline.html)
- [Covid AR with population scaling](covid-notebook-1.html)
- [Covid AR with population scaling and smoothed features](covid-notebook-2.html)
- [Covid Flatline](covid-notebook-3.html)

## Descriptions of Forecaster Families

### Training Data Information

(Taken from [David's Org File](https://github.com/cmu-delphi/exploration-tooling/blob/5a6da8d0d0202da6d79a5ee8e702d4654364ce46/forecasters_description.org#flusion).)

Some use just NHSN, while others use historical data from ILI+ and Flusurv+ as
additional rows in training. ILI+ and Flusurv+ have been adjusted so that the
total for the season matches NHSN’s total. Flusurv is taken from epidata, but
ILI+ was constructed by Evan and given to Richard. The testing date range is
roughly the 2023 season, so October 2023 through late April 2024.

### Flu exogenous features

- NSSP
  Note that this data set is possibly cheating, as we don't have revisions before April of this year, so it is using the latest data.
  If we narrow down to `time_value`s after that, the revision behavior is

  ```
  Min lag (time to first version):
      min median     mean     max
  7 days 7 days 7.7 days 14 days
  Fraction of epi_key+time_values with
  No revisions:
  • 362 out of 954 (37.95%)
  Quick revisions (last revision within 3 days of the `time_value`):
  • 0 out of 954 (0%)
  Few revisions (At most 3 revisions for that `time_value`):
  • 946 out of 954 (99.16%)

  Fraction of revised epi_key+time_values which have:
  Less than 0.1 spread in relative value:
  • 329 out of 592 (55.57%)
  Spread of more than 0.1015 in actual value (when revised):
  • 18 out of 592 (3.04%)
  days until within 20% of the latest value:
      min median   mean     max
  7 days 7 days 9 days 70 days
  ```

  So most days have some revisioning, but with fairly small total changes, with the vast majority of days being within 20% of their eventual value within a week (with some much longer exceptions, apparently).
  So the impact of the cheating is likely small but of course hard to know.

- Google-Symptoms
  This dataset doesn't have revisions, but has a history of suddenly disappearing.
  The latest value was used to simulate actually having the data; at worst, it breaks down to being the underlying forecaster.
- NWSS and NWSS_regional
  The originating dataset has minimal revisions, but as this is a dataset with quite a lot of processing from the underlying that involves some amount of time travel, it is unclear how much revision behavior it effectively has.

### Data Whitening

The data augmented models using ILI+ and FluSurv+ take a few different approaches to data whitening, depending on the `scale_method, center_method, nonlin_method` parameters.

TODO: Add descriptions.

This is more closely in line with the [RobustScaler](https://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.RobustScaler.html#sklearn.preprocessing.RobustScaler) from scikit-learn (using a much wider quantile than the default settings there).

## Overall comparison

This takes the best mean WIS result from each of the forecaster families below, and puts them in the same notebook for inter-family comparison.

## Forecaster Families

### AR with population scaling

Internal name: `scaled_pop`.

A simple model, which predicts using

$$x_{t+k} = ar(x)$$

where $x$ is scaled according to each state’s population.

Three versions, two with different engines `quantreg` and `grf`, and the final one with augmented data.

### AR with population scaling and seasonal features

Internal name: `scaled_pop_seasonal`.

There are 2 seasonal features that we're trying here:

1. taking the first 3 PC components from the whitened fused data (so nhsn, ILI+, and Flusurv). (Note that it's 2 for covid).
2. 2 indicators that roughly correspond to before, during and after the typical peak (first is true when `season_week < 16`, the second is true when `season_week > 20`, and the peak is captured by the overall constant).
   Note that unusually, these features are actually led rather than lagged, since we should be predicting using the target's coefficient, rather than the present one.

### Flusion-like

Roughly designed in line with the flusion model.

### No Recent Outcome

This is the fall-back forecaster, in case we have no data, but are forced to make a prediction.

A flusion-adjacent model pared down to handle the case of not having the target as a predictor.

$$\bar{x}_{t+k} = f(t_{season}) + p + d + \big\langle y_{t-k}\big\rangle_{k=0:1} + \big\langle y_{t-k}\big\rangle_{t=0:3}$$

where $y$ here is any exogenous variables; initially this will be empty, as nssp is missing some states, so we will have to rewrite these models to handle missing geos (most likely by having a separate model for the case when an exogenous variable is missing).

$f$ is either the identity or 2 sine terms, defined so that the first has half a period during the season, and is zero after it, while the second is one period over the season, with zero after

### Flatline

This is what the FluSight-baseline is based on, so they should be identical. However, at the moment, this has scaling issues.

# Covid Forecasts 2024-2025

For now, just AR forecasters with source-pooled data. Forecaster descriptions
are the same as above.

TODO: Get lagged correlations notebook hosted.
