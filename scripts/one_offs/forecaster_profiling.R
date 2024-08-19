# Profiling Script
#
# Use this script to profile a slow forecaster.
#
source("scripts/targets-common.R")

# Forecaster profiling.
d <- tar_read(joined_archive_data_2022, store = "covid_hosp_explore")
p <- profvis::profvis({
  slide_forecaster(
    epi_archive = d,
    outcome = "hhs",
    ahead = 2,
    extra_sources = "",
    forecaster = scaled_pop,
    n_training_pad = 30L,
    forecaster_args = list(
      lags = c(0, 3, 5, 7, 14),
      pop_scaling = TRUE,
      trainer = quantreg
    ),
    forecaster_args_names = c("lags", "pop_scaling", "trainer"),
    date_range_step_size = 7,
    cache_key = "joined_archive_data_2022"
  )
})
htmlwidgets::saveWidget(p, glue::glue("profvis_{Sys.time()}.html"), selfcontained = TRUE)
