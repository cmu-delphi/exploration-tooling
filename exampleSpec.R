# to define a forecaster:
# first define any pre-epipredict steps (they should be operating on a epi_df)
# then define any post-epipredict steps (same)
# then create the forecaster as a function, which must have the following arguments:
# 1. ahead
# 2.
# should at least have ahead as a parameter, but could have any number of other parameters; this example has all of the `arx_args_list` parameters, and this should be typical.
scaled_pop <- function(...) {
  args_list <- arx_args_list(...)
  # preprocessing not supported by epipredict
  preprocessing <- list()
  # preprocessing supported by epipredict
  epipredict_steps <- list(
    list(
      step = step_population_scaling,
      args = list(
        all_numeric(),
        df = state_census,
        df_pop_col = "pop",
        create_new = FALSE,
        rate_rescaling = 1e5,
        by = c("geo_value" = "abbr")
      )
    ),
    list(step = "default")
  )

  # postprocessing supported by epipredict
  epipredict_layers <- list(
    list(
      layer = layer_population_scaling,
      args = list(
        ".pred",
        df = state_census,
        df_pop_col = "pop",
        create_new = FALSE,
        rate_rescaling = 1e5,
        by = c("geo_value" = "abbr")
      )
    ),
    list(step = "default")
  )
  # postprocessing not supported by epipredict
  postprocessing <- list()
  return(list(preprocessing, epipredict_steps, epipredict_layers, postprocessing, args_list))
}
