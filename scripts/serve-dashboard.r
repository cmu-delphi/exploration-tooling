# Run the dashboard
library(targets)
library(here)
source(here::here("R", "load_all.R"))

Sys.setenv(TAR_PROJECT = "dashboard-proj")

tar_make()
rmarkdown::run(
  file = here::here("scripts", "reports", "forecast_dashboard.Rmd"),
  render_args = list(
    params = list(
      nhsn_latest_data = tar_read(nhsn_latest_data),
      nhsn_archive_data = tar_read(nhsn_archive_data),
      all_forecasts = tar_read(all_forecasts),
      all_scores = tar_read(all_scores)
    )
  )
)
