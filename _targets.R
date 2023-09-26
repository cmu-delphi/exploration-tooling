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

# Set target options:
tar_option_set(
  packages = c(
    "assertthat",
    "epieval",
    "epipredict",
    "parsnip",
    "tibble",
    "tidyr"
  ) # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
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

values <- expand_grid(
  # trainer = rlang::syms(c("linear_reg()", "quantile_reg()")),
  # trainer = rlang::syms(c("parsnip::linear_reg()")),
  ahead = c(1, 2, 3)
)

# Replace the target list below with your own:
list(
  tar_target(
    name = evaluation_data_file,
    command = "/home/dshemeto/repos/covid-hosp-forecast/evaluation-data-2023-09-15.RDS",
    format = "file"
  ),
  tar_target(
    name = evaluation_data,
    command = read_evaluation_data(evaluation_data_file)
  ),
  tar_target(
    name = prediction_cards_file,
    command = "/home/dshemeto/repos/covid-hosp-forecast/exploration-predictions-2023-09-15.RDS",
    format = "file"
  ),
  tar_target(
    name = prediction_cards,
    command = read_prediction_data(
      prediction_cards_file, "AR3_rate_acctlatency_1dtargets_postprocessing"
    )
  ),
  tar_map(
    values = list(measure = rlang::syms(c("weighted_interval_score", "absolute_error"))),
    tar_target(
      name = measure_eval_out,
      command = run_evaluation_measure(prediction_cards, evaluation_data, measure)
    ),
    tar_target(
      name = measure_eval_out2,
      command = measure_eval_out[[1L]]
    )
  ),
  tar_target(
    name = forecaster1,
    command = read_prediction_data(prediction_cards_file, "AR3_rate_acctlatency_1dtargets_postprocessing")
  ),
  tar_target(
    name = forecaster2,
    command = read_prediction_data(prediction_cards_file, "CDFens5p")
  ),
  tar_map(
    values = list(a = c(1000, 2000, 3000)),
    tar_target(
      name = ensemble_forecast,
      command = {
        forecaster1 %<>% select(geo_value, forecast_date, target_end_date, quantile, value)
        forecaster2 %<>% select(geo_value, forecast_date, target_end_date, quantile, value)
        ensemble_forecast <- forecaster1 %>%
          full_join(
            forecaster2,
            by = c("geo_value", "forecast_date", "target_end_date", "quantile")
          ) %>%
          mutate(
            value = (value.x + value.y + a) / 2
          ) %>%
          select(
            geo_value,
            forecast_date,
            target_end_date,
            quantile,
            value
          )
        ensemble_forecast
      }
    )
  ),
  tar_target(
    name = hhs_data_2023,
    command = download_data("hhs", "confirmed_admissions_covid_1d", "20230101", "20230401")
  ),
  tar_target(
    name = chng_data_2023,
    command = download_data("chng", "smoothed_adj_outpatient_covid", "20230101", "20230401")
  ),
  tar_target(
    name = data_archive_2023,
    command = {
      hhs_data_2023 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("hhs" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      chng_data_2023 %<>%
        select(geo_value, time_value, value, issue) %>%
        rename("chng" := value) %>%
        rename(version = issue) %>%
        as_epi_archive(
          geo_type = "state",
          time_type = "day",
          compactify = TRUE
        )
      epix_merge(hhs_data_2023, chng_data_2023, sync = "locf")
    }
  ),
  tar_map(
    values = values,
    tar_target(
      name = sig_pop_predictions,
      command = {
        forecaster_pred(
          data = data_archive_2023,
          outcome = "hhs",
          extra_sources = c(""),
          forecaster = scaled_pop,
          slide_training = Inf,
          slide_training_pad = 20L,
          ahead = ahead
        )
      }
    )
  )
)
