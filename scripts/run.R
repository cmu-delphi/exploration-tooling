#!/usr/bin/env Rscript
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

# This is a helper script to run the pipeline. Choose how to execute the
# pipeline below. See https://books.ropensci.org/targets/hpc.html to learn
# about your options.

# This is TAR_RUN_PROJECT and not TAR_PROJECT, because an .Renviron that sets
# TAR_PROJECT overrides the shell environment on every R start — including the
# callr subprocess tar_make spawns — so TAR_PROJECT can't reliably carry the
# selection. Script and store are then passed to tar_make explicitly.
tar_project <- Sys.getenv("TAR_RUN_PROJECT", "flu_hosp_prod")
# Where to place files in S3 (mostly unused)
aws_s3_prefix <- Sys.getenv("AWS_S3_PREFIX", "2024") %>% paste0("/", tar_project)
# Where to place flu forecasts
flu_submission_directory <- Sys.getenv("FLU_SUBMISSION_DIRECTORY", "cache")
# Where to place covid forecasts
covid_submission_directory <- Sys.getenv("COVID_SUBMISSION_DIRECTORY", "cache")
# Where to place rsv forecasts
rsv_submission_directory <- Sys.getenv("RSV_SUBMISSION_DIRECTORY", "cache")
# Backtest mode
backtest_mode <- Sys.getenv("BACKTEST_MODE", "FALSE")
cli::cli_inform(
  c(
    "i" = "Reading environment variables...",
    "*" = "TAR_RUN_PROJECT = {tar_project}",
    "*" = "AWS_S3_PREFIX = {aws_s3_prefix}",
    "*" = "FLU_SUBMISSION_DIRECTORY = {flu_submission_directory}",
    "*" = "COVID_SUBMISSION_DIRECTORY = {covid_submission_directory}",
    "*" = "RSV_SUBMISSION_DIRECTORY = {rsv_submission_directory}",
    "*" = "BACKTEST_MODE = {backtest_mode}"
  )
)

# Targets needs the output dir to already exist. NB: resolve it for the project
# being run — a bare tar_path_store() falls back to TAR_PROJECT, which is
# whatever the .Renviron says, not necessarily this run's project.
store_dir <- tar_config_get("store", project = tar_project)
if (!dir.exists(store_dir)) dir.create(store_dir)

tar_make(
  store = store_dir,
  script = tar_config_get("script", project = tar_project)
)
