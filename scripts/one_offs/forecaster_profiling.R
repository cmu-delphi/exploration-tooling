# Profiling Script
#
# Use this script to profile a slow forecaster.
#
# 2024-08-15: will profile these forecasters (both took ~1 hour per ahead when run in parallel)
# - arduous.dingo (scaled_pop)
# - byzantium.metamorphosis (smoothed_scaled)
source("scripts/targets-common.R")

# Forecaster profiling.
d <- tar_read(joined_archive_data_2022, store = "covid_hosp_explore")
p <- profvis::profvis({
  slide_forecaster(
    epi_archive = d,
    outcome = "hhs",
    ahead = 2,
    extra_sources = "",
    forecaster = dummy_forecaster,
    n_training_pad = 30L,
    forecaster_args = list(
      lags = list(c(0, 3, 5, 7, 14), c(0)),
      pop_scaling = TRUE,
      trainer = quantile_reg
    ),
    forecaster_args_names = c("lags", "pop_scaling", "trainer"),
    date_range_step_size = 7,
    cache_key = "joined_archive_data_2022"
  )
})
htmlwidgets::saveWidget(p, "profvis.html", selfcontained = TRUE)
