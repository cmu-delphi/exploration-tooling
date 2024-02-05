library(shiny)
args <- commandArgs(trailingOnly = TRUE)
epieval::manage_S3_forecast_cache(direction = args[1])
