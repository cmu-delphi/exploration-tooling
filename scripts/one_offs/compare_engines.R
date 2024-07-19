# Compare quantreg engines.
#
# I suspect that the default method="br" in quantreg::rq is causing the slow
# performance. Here we will compare the performance of the "br" method to the
# "fn" and other methods. Since the existing parsnip model define in epipredict
# does not expose the method argument, I redefine that interface in
# new_engines.R.
source("scripts/targets-common.R")
source("scripts/one_offs/new_engines.R")
make_quantile_reg2()


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
    date_range_step_size = 60,
    cache_key = "joined_archive_data_2022"
  )
})
htmlwidgets::saveWidget(p, glue::glue("profvis_{Sys.time()}.html"), selfcontained = TRUE)

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
      trainer = quantile_reg2(method = "br")
    ),
    forecaster_args_names = c("lags", "pop_scaling", "trainer"),
    date_range_step_size = 60,
    cache_key = "joined_archive_data_2022"
  )
})
htmlwidgets::saveWidget(p, glue::glue("profvis_{Sys.time()}.html"), selfcontained = TRUE)

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
      trainer = quantile_reg2(method = "fn")
    ),
    forecaster_args_names = c("lags", "pop_scaling", "trainer"),
    date_range_step_size = 60,
    cache_key = "joined_archive_data_2022"
  )
})
htmlwidgets::saveWidget(p, glue::glue("profvis_{Sys.time()}.html"), selfcontained = TRUE)

# TODO: Adapt code so we can get back multiple taus at once.
quantile_reg_pfn <- quantile_reg2(method = "pfn")
quantile_reg_pfnb <- quantile_reg2(method = "pfnb")
quantile_reg_qfnb <- quantile_reg2(method = "qfnb")
