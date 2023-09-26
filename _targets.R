# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

library(epipredict)
library(parsnip)
library(tidyr)
library(epieval)
library(epidatr)

# Set target options:
tar_option_set(
  packages = c(
    "assertthat",
    "epieval",
    "epipredict",
    "parsnip",
    "tibble",
    "tidyr",
    "epidatr"
  ), # packages that your targets need to run
  imports = c("epieval", "parsnip"),
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  controller = crew::crew_controller_local(workers = 8),
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multicore")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
linreg <- parsnip::linear_reg()
quantreg <- quantile_reg()
values <- expand_grid(
  trainer = rlang::syms(c("linreg", "quantreg")),
  ahead = 1:4,
  pop_scaling = c(TRUE, FALSE)
)

# Replace the target list below with your own:
list(
  tar_target(
    name = hhs_data_2022,
    command = download_data("hhs", "confirmed_admissions_covid_1d", "20220101", "20220401")
  ),
  tar_target(
    name = chng_data_2022,
    command = download_data("chng", "smoothed_adj_outpatient_covid", "20220101", "20220401")
  ),
  tar_target(
    name = data_archive_2022,
    command = {
      hhs_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("hhs" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      chng_data_2022 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("chng" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      epix_merge(hhs_data_2022, chng_data_2022, sync = "locf")
    }
  ),
  tar_map(
    values = values,
    tar_target(
      name = sig_pop_predictions,
      command = {
        forecaster_pred(
          data = data_archive_2022,
          outcome = "hhs",
          extra_sources = c(""),
          forecaster = scaled_pop,
          slide_training = Inf,
          slide_training_pad = 30L,
          ahead = ahead,
          trainer = trainer
        )
      }
    )
  )
  ## tar_map(
  ##   values = list(a = c(300, 15)),
  ##   tar_target(
  ##     name = ensemble_forecast,
  ##     command = {
  ##       forecasterA <- `sig_pop_predictions_.parsnip..linear_reg..._1_TRUE`
  ##       forecasterB <- `sig_pop_predictions_.parsnip..linear_reg..._1_FALSE`
  ##       ensemble_forecast <- forecasterA %>%
  ##         full_join(
  ##           forecasterB,
  ##           by = c("geo_value", "forecast_date", "target_end_date", "quantile")
  ##         ) %>%
  ##         mutate(
  ##           value = (value.x + value.y + a) / 2
  ##         ) %>%
  ##         select(
  ##           geo_value,
  ##           forecast_date,
  ##           target_end_date,
  ##           quantile,
  ##           value
  ##         )
  ##       ensemble_forecast
  ##     }
  ##   )
  ## )
)
