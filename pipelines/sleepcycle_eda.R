# EDA project: sleepcycle cough 2+ vs NHSN/NSSP. Data targets plus a rendered notebook.
suppressPackageStartupMessages({
  library(targets); library(tarchetypes); library(dplyr); library(tidyr); library(purrr)
  library(lubridate); library(epidatr)
})
for (f in c("R/targets/sleepcycle_data_targets.R")) source(here::here(f))
tar_option_set(packages = c("dplyr", "tidyr", "purrr", "lubridate", "epidatr"))

list(
  sleepcycle_data_targets(),
  tar_target(panel, make_weekly_panel(sc_archive, nhsn_archive, nssp_archive, state_pop)),
  tar_target(national, make_national(panel)),
  tar_target(national_cors, national_lag_cors(national)),
  tar_target(state_cors, state_lag_cors(panel)),
  tar_target(sc_revisions, sc_revision_summary(sc_archive)),
  tar_target(
    report,
    render_quarto_report(
      "reports/writeups/sleepcycle/sleepcycle_cough.qmd", "rendered_reports",
      panel, national, national_cors, state_cors, sc_revisions, sc_archive
    ),
    format = "file"
  )
)
