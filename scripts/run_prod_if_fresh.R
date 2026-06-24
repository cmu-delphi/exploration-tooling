# Gates the flu/covid prod forecast run on NHSN/NSSP data freshness.
#
# Intended to be fired repeatedly (see prod-forecasts.timer: every 30 minutes,
# 07:00-14:00 America/Los_Angeles, Wednesdays). On each firing:
#   - if today's forecast already ran successfully, do nothing.
#   - if NHSN/NSSP data is fresh (latest time_value <=7 days old), run the
#     covid and flu prod pipelines, sync/update the report site, and deploy
#     to netlify, then mark today as done.
#   - if data is stale, wait for the next firing (the freshness check itself
#     re-fetches the archive targets, so a later firing will pick up newer
#     upstream data automatically) or, if this is the last firing of the day
#     (>=14:00), log a CRITICAL line and give up for today.
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

log_file <- here::here("cache", "logs", "prod_forecast_freshness.log")
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), msg), file = log_file, append = TRUE)
}

today <- Sys.Date()
hour <- as.integer(format(Sys.time(), "%H"))
marker <- here::here("cache", sprintf("prod_forecast_done_%s", today))

if (file.exists(marker)) {
  log_msg(sprintf("Forecast already completed today (%s), skipping.", today))
  quit(status = 0)
}

check_project_freshness <- function(project) {
  Sys.setenv(TAR_PROJECT = project)
  check_data_freshness()
}

fresh <- check_project_freshness("covid_hosp_prod") && check_project_freshness("flu_hosp_prod")

if (!fresh) {
  log_msg(sprintf("Data is stale (local hour=%d).", hour))
  if (hour >= 14) {
    log_msg(sprintf(
      "CRITICAL: NHSN/NSSP data is still stale at the 14:00 cutoff. Skipping forecast run for %s; upstream data needs investigation.",
      today
    ))
  }
  quit(status = 0)
}

log_msg("Data is fresh, running prod forecasts.")

#' Run a project's targets pipeline, appending console output to `log_path`.
run_project_pipeline <- function(project, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  store <- targets::tar_config_get("store", project = project)
  script <- targets::tar_config_get("script", project = project)
  dir.create(store, showWarnings = FALSE)
  con <- file(log_path, open = "a")
  sink(con, append = TRUE, split = TRUE)
  sink(con, append = TRUE, type = "message")
  result <- tryCatch(
    {
      targets::tar_make(store = store, script = script)
      0L
    },
    error = function(e) {
      message(sprintf("Pipeline for %s failed: %s", project, conditionMessage(e)))
      1L
    }
  )
  sink(type = "message")
  sink()
  close(con)
  result
}

#' Run an external command, appending its combined stdout/stderr to
#' `log_path`. Returns the command's exit status.
run_logged <- function(command, args, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  output <- system2(command, args, stdout = TRUE, stderr = TRUE)
  cat(output, sep = "\n", file = log_path, append = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) 0L else status
}

steps <- list(
  list(
    name = "covid prod pipeline",
    run = function() run_project_pipeline("covid_hosp_prod", here::here("cache", "logs", "prod_covid"))
  ),
  list(
    name = "flu prod pipeline",
    run = function() run_project_pipeline("flu_hosp_prod", here::here("cache", "logs", "prod_flu"))
  ),
  list(
    name = "sync reports to S3",
    run = function() {
      run_logged("aws", c("s3", "sync", "reports/", "s3://forecasting-team-data/2024/reports/"), here::here("cache", "logs", "update_site_log.txt"))
      run_logged("aws", c("s3", "sync", "s3://forecasting-team-data/2024/reports/", "reports/"), here::here("cache", "logs", "update_site_log.txt"))
    }
  ),
  list(
    name = "update site",
    run = function() {
      log_path <- here::here("cache", "logs", "update_site_log.txt")
      dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
      con <- file(log_path, open = "a")
      sink(con, append = TRUE, split = TRUE)
      sink(con, append = TRUE, type = "message")
      result <- tryCatch(
        {
          update_site()
          0L
        },
        error = function(e) {
          message(sprintf("update_site() failed: %s", conditionMessage(e)))
          1L
        }
      )
      sink(type = "message")
      sink()
      close(con)
      result
    }
  ),
  list(
    name = "netlify deploy",
    run = function() run_logged("netlify", c("deploy", "--dir=reports", "--prod"), here::here("cache", "prod_netlify"))
  )
)

for (step in steps) {
  log_msg(sprintf("Starting: %s", step$name))
  status <- step$run()
  if (status != 0) {
    log_msg(sprintf("CRITICAL: %s failed with exit code %s. Aborting prod run for %s.", step$name, status, today))
    quit(status = 1)
  }
}

file.create(marker)
log_msg(sprintf("Prod forecast run for %s completed successfully.", today))
quit(status = 0)
