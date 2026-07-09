# The Flu Hospitalization Production Forecasting Pipeline (as_of = today).
#
# Entry point for the weekly production run. The target DAG lives in
# scripts/_flu_prod_shared.R and is shared with the backfill pipeline
# (scripts/flu_hosp_backfill.R).
suppressPackageStartupMessages(source("R/load_all.R"))
source("scripts/_flu_prod_shared.R")

# Production mode: a single forecast for the current week.
g_backtest_mode <- FALSE
# The as_of for the forecast. On our typical schedule this is today (a
# Wednesday); for a delayed forecast it can be a Thursday. Used both for
# stamping the data and for choosing the as_of when creating the forecast.
g_forecast_generation_dates <- Sys.Date()
# Usually the forecast_date equals the generation date, but it can be
# overridden. It should be a Wednesday.
g_forecast_dates <- round_date(g_forecast_generation_dates, "weeks", week_start = 3)
# The forecast is actually for the Wednesday beforehand on these days.
if (Sys.Date() %in% as.Date(c("2025-12-29"))) {
  g_forecast_dates <- as.Date("2025-12-24")
}

build_flu_prod_pipeline()
