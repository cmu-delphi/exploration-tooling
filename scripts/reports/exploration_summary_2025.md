<!-- Render this with pandoc -->
<!-- pandoc scripts/reports/exploration_summary_2025.md -s -o reports/exploration_summary_2025.html --css reports/style.css --metadata pagetitle='Exploration Summary 2024-2025' -->

# Exploration Summary 2024-2025

In this document, we will summarize our findings from backtesting a large variety of forecasters on the 2023-2024 season.
The forecaster family definitions can be found at the bottom of [this page](https://delphi-forecasting-reports.netlify.app/).

## Best Performing Families

### Flu

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

### Covid

[The best performing families](https://delphi-forecasting-reports.netlify.app/covid-overall-notebook) were:

- AR with seasonal windows and the NSSP exogenous feature.
  - This forecaster outperformed the CDC ensemble by about 15 mean WIS points.
- Surprisingly, the `climate_linear` model was only about 4 mean WIS points behind our best performing family.
  (`climate_linear` combines the `climate_*` models with the `linear` model using a special weighting scheme.
  See the [season summary](season_summary_2025.html) for more details.)

## Important Parameters

- Forecasters that used a seasonal training window were substantially better than those that did not.
- Forecasters that used the NSSP exogenous feature were substantially better than those that did not.

## Important Notes

One of the most concerning behaviors in our forecasters was the bias towards predicting a down-swing in the target.
After a deeper analysis, we concluded that this is due to a downward bias in the data set, which our linear AR models were picking up and translating into coefficients that were less than 1, making declines almost certain.
The complete analysis can be found [here](https://delphi-forecasting-reports.netlify.app/decreasing_forecasters).
