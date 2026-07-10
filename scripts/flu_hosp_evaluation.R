# The Flu Hospitalization Evaluation Pipeline.
#
# Historical replay of the production forecast over many past forecast dates,
# scored against latest truth. Shares the target DAG with the production
# pipeline (scripts/flu_hosp_prod.R) via scripts/_flu_prod_shared.R. This is the
# same forecasting pipeline as production, run over a season of past dates and
# evaluated rather than submitted.
suppressPackageStartupMessages(source("R/load_all.R"))
source("scripts/_flu_prod_shared.R")

# Evaluation mode: replay production over historical forecast dates. Skips the
# weekly report notebook (each week's report is preserved as an ASOF snapshot)
# and instead builds the evaluation scoring notebook; submission CSVs are written
# only for the final date.
g_evaluation_mode <- TRUE
# End of the replay window; pin FORECAST_REFERENCE_DATE for a reproducible oracle
# capture (default Sys.Date()). Must be >= 2025-12-31 so the trailing seq.Date()s
# stay non-empty and 1:1 aligned with g_forecast_dates.
g_reference_date <- g_forecast_reference_date()
g_forecast_generation_dates <- c(
  as.Date(c("2024-11-21", "2024-11-27", "2024-12-04", "2024-12-11", "2024-12-18", "2024-12-26", "2025-01-02")),
  seq.Date(as.Date("2025-01-08"), as.Date("2025-12-17"), by = 7L),
  as.Date(c("2025-12-29")),
  seq.Date(as.Date("2025-12-31"), g_reference_date, by = 7L)
)
# Every Wednesday since mid-Nov 2024.
g_forecast_dates <- seq.Date(as.Date("2024-11-20"), g_reference_date, by = 7L)
# Optional: keep only the last N dates for a fast partial evaluation (REFACTOR.md
# oracle). Inert when unset. Both date vectors are 1:1 aligned, so slice both by
# the same indices.
g_evaluation_n_dates <- as.integer(Sys.getenv("EVALUATION_N_DATES", "0"))
if (!is.na(g_evaluation_n_dates) && g_evaluation_n_dates > 0) {
  keep <- tail(seq_along(g_forecast_dates), g_evaluation_n_dates)
  g_forecast_dates <- g_forecast_dates[keep]
  g_forecast_generation_dates <- g_forecast_generation_dates[keep]
}

build_flu_prod_pipeline()
