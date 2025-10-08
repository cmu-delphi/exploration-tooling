# Delphi Forecast Reports

[GitHub Repo](https://github.com/cmu-delphi/explorationt-tooling/)

## Overview

- The weekly fanplots were used by the team to visually inspect the forecasts.
- The season reports provide a general analysis of the 2024-2025 season's data and forecaster performance.
- The backtesting reports were pre-season tests of a variety of forecasters on the 2023-2024 season's data.
- A description of the forecaster families explored is provided at the bottom of the page.

## Most recent week


## Score notebooks


## Weekly Fanplots 2025-2026 Season


## Weekly Fanplots 2024-2025 Season


## 2024-2025 Season Reports

- [Season Summary](season_summary_2025.html) (the notebooks below are linked from here)
  - [Covid's Problematic Initial Forecast](first_day_wrong.html)
  - [NHSN and NSSP Revision Behavior](revision_summary_2025.html)
- [Season Summary Talk](season_summary_2025_presentation.html)
- [An Analysis of Decreasing Behavior in Forecasters](decreasing_forecasters.html)
- [NHSN 2024-2025 Data Analysis](new_data.html)

## 2023-2024 Season Backtesting

- [Forecaster Exploration Summary](#exploration-summary-2024-2025)
- Flu
  - [Flu Overall](flu-overall-notebook.html)
  - [Flu AR](flu-notebook-scaled_pop_main.html)
  - [Flu AR with augmented data](flu-notebook-scaled_pop_data_augmented.html)
  - [Flu AR with exogenous features](flu-notebook-scaled_pop_exogenous.html)
  - [Flu AR with different seasonal schemes](flu-notebook-scaled_pop_season.html)
  - [Flu AR with augmented data and with different seasonal window sizes](flu-notebook-season_window_sizes.html)
  - [Flu AR with augmented data, exogenous features, and seasonal windowing](flu-notebook-scaled_pop_season_exogenous.html)
  - Simplistic/low data methods:
    - [Flu no recent](flu-notebook-no_recent_quant.html)
    - [Flu flatline](flu-notebook-flatline.html)
    - [Flu climate and linear](flu-notebook-climate_linear.html)
- Covid
  - [Covid Overall](covid-overall-notebook.html)
  - [Covid AR](covid-notebook-scaled_pop_main.html)
  - [Covid AR with seasonal features](covid-notebook-scaled_pop_season.html)
  - [Covid AR with exogenous features](covid-notebook-scaled_pop_exogenous.html)
  - [Covid Flatline](covid-notebook-flatline_forecaster.html)
  - Simplistic/low data methods:
    - [Covid no recent](covid-notebook-no_recent_quant.html)
    - [Covid flatline](covid-notebook-flatline.html)
    - [Covid climate and linear](covid-notebook-climate_linear.html)

## Description of Forecaster Families

The main forecaster families were:

- [Autoregressive models (AR)](#autoregressive-models-ar)
  - [with seasonal features](#autoregressive-models-with-seasonal-features)
  - [with exogenous features](#autoregressive-models-with-exogenous-features)
  - [with augmented data](#autoregressive-models-with-augmented-data)
- "Ad-hoc" models
  - [Climatological](#climatological)
  - [Linear trend](#linear-trend)
  - [No recent outcome](#no-recent-outcome)
- Baseline models
  - [Flatline](#flatline)

Notes:

- All forecasters population scale their data, use geo pooling, and train using quantreg.
  We found that `quantreg` gave better results than `linreg` and we had computational issues with `grf`, so we used `quantreg` the rest of the time.
- All the AR models had the option of population scaling, seasonal features, exogenous features, and augmented data.
  We tried all possible combinations of these features (in notebooks above).
- The forecaster definitions are in the [`flu_forecaster_config.R`](https://github.com/cmu-delphi/exploration-tooling/blob/main/R/targets/flu_forecaster_config.R) and [`covid_forecaster_config.R`](https://github.com/cmu-delphi/exploration-tooling/blob/main/R/targets/covid_forecaster_config.R) files.

### Autoregressive models (AR)

Internal name: `scaled_pop`.

A simple autoregressive model, which predicts using

$$x_{t+k} = x_{t-7} + x_{t-14}$$

where $x$ is the target variable (which can be scaled according to each state's population or whitened according to another scheme (or both)), $t$ is the data in days, and $k \in \{0, 7, 14, 21, 28\}$ is the forecast horizon.
In practice, we found that adding more lags provided no discernible advantage (such as (0, 7, 14) and (0, 7, 14, 21))

### Autoregressive models with exogenous features

Internal name: `scaled_pop_seasonal`.

$$x_{t+k} = x_{t-7} + x_{t-14} + y_{t-7} + y_{t-14}$$

where $x$ is the target variable, $y$ is the exogenous variable, and $t$ is the data in days.
See the list of exogenous features below.
These models could opt into the same seasonal features as the `scaled_pop_seasonal` forecaster (see below).

#### Flu exogenous features

- NSSP - we don't have revisions before Spring 2024 for this data, so we used a revision analysis from the data collected after that date to estimate the lag (roughly 7 days) and used that lag to simulate delays. The [Revision summary](revision_summary_2025.html) from this year suggests that this likely missed revisions, so the backtested version was overly optimistic.
- Google-Symptoms - this dataset doesn't have revisions, but has a history of suddenly disappearing, resulting in intermittent long update lags.
We did not simulate a lag and just used to latest value for a best case scenario.
The symptom set used was s01, s03, and s04 from [here](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/google-symptoms.html).
- NWSS - the originating dataset has minimal revisions, but as this is a dataset with quite a lot of processing from the underlying that involves some amount of time travel, so it is unclear how much revision behavior is present.
- NWSS_regional - same as NWSS, just aggregated to the HHS region level.

#### Covid exogenous features

- NSSP - same as flu.
- Google-Symptoms - same as flu, though we used a slightly different symtom set (just s04 and s05 from [here](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/google-symptoms.html)).

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

These models could be combined with the exogenous features (see above).

### Autoregressive models with augmented data

Internal name: `scaled_pop` (with `filter_source = ""`).

This forecaster is still the standard autoregressive model, but with additional training data.
Inspired by UMass-flusion, the additional training data consisted of historical data from ILI+ and Flusurv+, which was brought to a comprable level with NHSN and treated as additional observations of the target variable (hence the name "augmented data").
Flusurv was taken from epidata, but ILI+ was constructed by Evan Ray and given to Richard (Berkeley Summer 2024 intern).
Naturally, this forecaster was only used for flu, as the same data was not available for covid.

These models could be combined with the exogenous features and the seasonal features (see above).

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
We found that in some cases it made a reasonable baseline, though when the current season's peak time was significantly different from the seasons in the training data, it was not particularly effective.

### Linear Trend

A simple linear trend model that predicts the median using linear extrapolation from the past 4 weeks of data and then uses residuals to create a distributional forecast.

### Climate Linear

An ensemble model that combines a climatological forecast with a linear trend forecast.
It is a bilinear interpolation between the two forecasts across the ahead and quantile extremity; as the quantile moves away from the median, and the ahead moves further in the future, the ensemble interpolates between the linear and climate forecasts.
As the ahead goes from -1 to 4, it linearly interpolates between a 5% weight on the climate model and a 90% weight on the climate model (so the furthest ahead is mostly a climate model).
At the same time, as the quantile level goes further away from the median, it interpolates between a 10% weight on the climate model at the median and a 100% weight on the climate model at either the 1% or 99% quantile levels.
In net, at the median -1 ahead, the climate models have a weight of 0.5%, and the linear model of 99.5%.

### No Recent Outcome

This was a fall-back forecaster built for the scenario where NHSN data was not going to be reported in time for the start of the forecasting challenge.

A flusion-adjacent model pared down to handle the case of not having the target as a predictor.

$$\bar{x}_{t+k} = \big\langle y_{t-k}\big\rangle_{k=0:1} + \big\langle y_{t-k}\big\rangle_{t=0:3}$$

where $y$ here is any set of exogenous variables.

### Flatline

A simple "LOCF" forecaster that simply forecasts the last observed value and uses residuals to create a distributional forecast. This is what the FluSight-baseline is based on, so they should be identical.

## Exploration Summary 2024-2025

Here we summarize our findings from backtesting a large variety of forecasters on the 2023-2024 season.

### Best Performing Families

#### Flu

[The best performing families](https://delphi-forecasting-reports.netlify.app/flu-overall-notebook) were:

- AR with seasonal windows and the NSSP exogenous feature
  - This forecaster was about 10 mean WIS points behind UMass-flusion, but on par with the FluSight-ensemble.
- AR with seasonal window (same as above, but without the NSSP exogenous feature)
  - This forecaster was only 2 mean WIS points behind the above forecaster.
  - We explored a wide variety of parameters for this family and found that the number of weeks to include in the training window was not particularly important, so we settled on 5 weeks prior and 3 weeks ahead.
- An ensemble of climatological and the linear trend model (we used this at the start of the season when we didn't trust the data to support a more complex model)
  - We were surprised to find that this was only 7 mean WIS points behind our best performing family.
- For context, the gap between our best performing family and FluSight-baseline was only about 15 mean WIS points.
- Surprisingly, AR forecasters with augmented data performed **worse** than those that did not.
  However, AR forecasters with seasonal windows and augmented data performed better than AR forecasters with only seasonal windows.

#### Covid

[The best performing families](https://delphi-forecasting-reports.netlify.app/covid-overall-notebook) were:

- AR with seasonal windows and the NSSP exogenous feature.
  - This forecaster outperformed the CDC ensemble by about 15 mean WIS points.
- Surprisingly, the `climate_linear` model was only about 4 mean WIS points behind our best performing family.
  (`climate_linear` combines the `climate_*` models with the `linear` model using a special weighting scheme.
  See the [season summary](season_summary_2025.html) for more details.)

### Important Parameters

- Forecasters that used a seasonal training window were substantially better than those that did not.
- Forecasters that used the NSSP exogenous feature were substantially better than those that did not.

### Important Notes

One of the most concerning behaviors in our forecasters was the bias towards predicting a down-swing in the target.
After a deeper analysis, we concluded that this is due to a downward bias in the data set, which our linear AR models were picking up and translating into coefficients that were less than 1, making declines almost certain.
The complete analysis can be found [here](https://delphi-forecasting-reports.netlify.app/decreasing_forecasters).
