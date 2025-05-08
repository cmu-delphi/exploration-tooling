<style>
body {
  max-width: 800px;
  margin: 2rem auto;
  padding: 0 1rem;
  font-family: sans-serif;
  background: white;
  color: black;
}
</style>

# Delphi Forecast Reports

[GitHub Repo](https://github.com/cmu-delphi/explorationt-tooling/)

## Overview

- The weekly fanplots were used by the team to visually inspect the forecasts.
- The season reports provide a general analysis of the 2024-2025 season's data and forecaster performance.
- The backtesting reports were pre-season tests of a variety of forecasters on the 2023-2024 season's data.
- A description of the forecaster families explored is provided at the bottom of the page.

## Weekly Fanplots 2024-2025 Season


## 2024-2025 Season Reports

- [Season Summary](season_summary_2025.html) (the notebooks below are linked from here)
  - [Covid's Problematic Initial Forecast](first_day_wrong.html)
  - [NHSN Revision Behavior](revision_summary_2025.html)
- [An Analysis of Decreasing Behavior in Forecasters](decreasing_forecasters.html)
- [NHSN 2024-2025 Data Analysis](new_data.html)

## 2023-2024 Season Backtesting

- [Forecaster Exploration Summary](exploration_summary_2024.html)
- Flu
  - All forecasters population scale their data, use geo pooling, and train using quantreg.
  - These definitions are in the `flu_forecaster_config.R` file.
  - [Flu Overall](flu-overall-notebook.html)
  - [Flu AR](flu-notebook-scaled_pop_main.html)
  - [Flu AR with augmented data](flu-notebook-scaled_pop_data_augmented.html)
  - [Flu AR with exogenous features](flu-notebook-scaled_pop_exogenous.html)
  - [Flu AR with different seasonal schemes](flu-notebook-scaled_pop_season.html)
  - [Flu AR with augmented data and with different seasonal window sizes](flu-notebook-season_window_sizes.html)
  - [Flu AR with augmented data, exogenous features, and seasonal windowing](flu-notebook-scaled_pop_season_exogenous.html)
  - Simplistic/low data methods:
    - [Flu no recent](flu-notebook-no_recent_quant.html)
    - [Flu no recent](flu-notebook-no_recent_quant.html)
    - [Flu flatline](flu-notebook-flatline.html)
    - [Flu climate](flu-notebook-climate_linear.html)
- Covid
  - All forecasters population scale their data, use geo pooling, and train using quantreg.
  - These definitions are in the `covid_forecaster_config.R` file.
  - [Covid Overall](covid-overall-notebook.html)
  - [Covid AR](covid-notebook-scaled_pop_main.html)
  - [Covid AR with seasonal features](covid-notebook-scaled_pop_season.html)
  - [Covid AR with exogenous features](covid-notebook-scaled_pop_exogenous.html)
  - [Covid Flatline](covid-notebook-flatline_forecaster.html)
  - Simplistic/low data methods:
    - [Covid no recent](covid-notebook-no_recent_quant.html)
    - [Covid flatline](covid-notebook-flatline.html)
    - [Covid climate](covid-notebook-climate_linear.html)

## Description of Forecaster Families

The main forecaster families were:

- Autoregressive models (AR)
  - with seasonal features
  - with exogenous features
  - with augmented data
- "Ad-hoc" models
  - Climatological
  - Linear trend
  - No recent outcome
- Baseline models
  - Flatline

All the AR models had the option of population scaling, seasonal features, exogenous features, and augmented data.
We tried all possible combinations of these features.
All models had the option of using the `linreg`, `quantreg`, or `grf` engine.
We found that `quantreg` gave better results than `linreg` and we had computational issues with `grf`, so we used `quantreg` the rest of the time.

### Autoregressive models (AR)

Internal name: `scaled_pop`.

A simple autoregressive model, which predicts using

$$x_{t+k} = ar(x)$$

where $x$ is the target variable and $ar(x)$ is a linear combination of the target variable's past values, which can be scaled according to each state's population or whitened according to another scheme (or both). In practice, we found that using lags (0, 7) was quite effective (with (0, 7, 14) and (0, 7, 14, 21) providing no discernible advantage), so we focused on those, so in practice our model was

$$x_{t+k} = x_t + x_{t-7}$$

where $k \in \{0, 7, 14, 21, 28\}$ is the forecast horizon.

### Autoregressive models with seasonal features

Internal name: `scaled_pop_seasonal`.

We tried a few different attempts at incorporating seasonal features:

- The approach that performed the best was using a **seasonal training window** that grabbed a window of data (about 4 weeks before and ahead) around the forecast epiweek from the current and previous seasons.
- Two **indicator variables** that roughly correspond to before, during, and after the typical peak (roughly, `before = season_week < 16`, `during = 16 <= season_week <= 20`, and `after = season_week > 20`).
- Taking the first two **principal components** of the full whitened augmented data reshaped as `(epiweek, state_source_season_value)`.
(We found that this was not particularly effective, so we did not use it.
Despite spending a week debugging this, we could not rule out the possibility that it was a bug.
However, we also had mixed results from tests of this feature in very simple synthetic data cases.)
- We also tried using the **climatological median** of the target variable as a feature (see below for definition of "climatological").
- Note that unusually, the last two features are actually led rather than lagged, since we should be predicting using the target's coefficient, rather than the present one.

### Autoregressive models with exogenous features

Internal name: `scaled_pop_seasonal`.

These models could opt into the same seasonal features as the `scaled_pop_seasonal` forecaster, but also included exogenous features.

#### Flu exogenous features

- NSSP - we don't have revisions before Spring 2024 for this data, so we used a revision analysis from the data collected after that date to estimate the lag (roughly 7 days) and used that lag to simulate delays.
- Google-Symptoms - this dataset doesn't have revisions, but has a history of suddenly disappearing, resulting in intermittent long update lags.
We did not simulate a lag and just used to latest value for a best case scenario.
The symptom set used was s01, s03, and s04 from [here](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/google-symptoms.html).
- NWSS - the originating dataset has minimal revisions, but as this is a dataset with quite a lot of processing from the underlying that involves some amount of time travel, so it is unclear how much revision behavior is present.
- NWSS_regional - same as NWSS, just aggregated to the HHS region level.

#### Covid exogenous features

- NSSP - same as flu.
- Google-Symptoms - same as flu, though we used a slightly different symtom set (just s04 and s05 from [here](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/google-symptoms.html)).

### Autoregressive models with augmented data

Internal name: `scaled_pop` (with `filter_source = ""`).

This forecaster is still the standard autoregressive model, but with additional training data.
Inspired by UMass-flusion, the additional training data consisted of historical data from ILI+ and Flusurv+, which was brought to a comprable level with NHSN and treated as additional observations of the target variable (hence the name "augmented data").
Flusurv was taken from epidata, but ILI+ was constructed by Evan Ray and given to Richard (Berkeley Summer 2024 intern).
Naturally, this forecaster was only used for flu, as the same data was not available for covid.

#### Scaling Parameters (Data Whitening)

We tried a few different approaches to data whitening.

- `scale_method`
  - `quantile` - scales the data so that the difference between the 5th and 95th quantiles is 1 (akin to [RobustScaler](https://scikit-learn.org/stable/modules/generated/sklearn.preprocessing.RobustScaler.html#sklearn.preprocessing.RobustScaler) from scikit-learn)
  - `quantile_upper` - scales the data so that the 95th quantile is 1 (this was used by UMass-flusion)
  - `std` - scales the data so that one standard deviation is 1
  - `none` - no scaling
  - We did not see a significant difference in changing the above parameter, so we used the default `quantile` the rest of the time.
- `center_method`
  - `median` - centers the data so that the median is 0
  - `mean` - centers the data so that the mean is 0
  - `none` - no centering
  - We did not see a significant difference in changing the above parameter, so we used the default `median` the rest of the time.
- `nonlin_method`
  - `quart_root` - takes the 4th root of the data (and adds 0.01 to avoid negative values)
  - `none` - no non-linear transformation
  - Of these, `quart_root` gave us the best results, so we used that the rest of the time (beware: the epsilon offset can interact poorly with forecasts clipped to be non-negative).

### Climatological

This was our term for a forecaster that directly forecast a distribution built from similar weeks from previous seasons (in analogy with baseline weather forecasting).
We found that in some cases it made a reasonable baseline, though when the current season's peak time was significatly different from the seasons in the training data, it was not particularly effective.

### Linear Trend

A simple linear trend model that predicts the median using linear extrapolation from the past 4 weeks of data and then uses residuals to create a distributional forecast.

### No Recent Outcome

This was a fall-back forecaster built for the scenario where NHSN data was not going to be reported in time for the start of the forecasting challenge.

A flusion-adjacent model pared down to handle the case of not having the target as a predictor.

$$\bar{x}_{t+k} = \big\langle y_{t-k}\big\rangle_{k=0:1} + \big\langle y_{t-k}\big\rangle_{t=0:3}$$

where $y$ here is any set of exogenous variables.

### Flatline

A simple "LOCF" forecaster that simply forecasts the last observed value and uses residuals to create a distributional forecast. This is what the FluSight-baseline is based on, so they should be identical.
