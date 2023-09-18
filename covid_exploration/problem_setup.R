library(explorationTools)
source_signal_pairs <- list(
  list(covidcast, "hhs", "confirmed_admissions_covid_1d"),
  list(covidcast, "chng", "smoothed_adj_outpatient_covid")
)
add_data_problem("COVID bulk", source_signal_pairs)


add_forecaster("population scaled", forecaster)
