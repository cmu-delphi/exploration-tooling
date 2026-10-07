# Gates the flu/covid prod forecast run on NHSN/NSSP data freshness.
#
# Fired every 30 minutes, 07:00-14:00 America/Los_Angeles, on Wednesdays (see
# prod-forecasts.timer). Runs the forecasts once the data is fresh, retries on
# later firings if not, and gives up with an alert at the 14:00 cutoff. After
# the pipelines it renders each disease's health notebook, publishes, and posts
# links to the new reports on Slack; a pipeline error, failed health check, or failed publish step alerts on Slack
# (see notes/prod-health-check.md) and leaves the day unfinished so the next
# firing retries.
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

log_file <- here::here("cache", "logs", "prod_forecast_freshness.log")
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), msg), file = log_file, append = TRUE)
}

# Post `text` to Slack (SLACK_WEBHOOK_URL), once per distinct message per day
# so the half-hourly retries don't repeat it. Returns FALSE if the post failed.
post_once <- function(text) {
  sent_file <- here::here("cache", sprintf("prod_alerts_sent_%s", Sys.Date()))
  sent <- if (file.exists(sent_file)) readLines(sent_file) else character(0)
  key <- rlang::hash(text)
  if (key %in% sent) {
    return(TRUE)
  }
  delivered <- notify_slack(text)
  if (delivered) {
    cat(key, "\n", file = sent_file, append = TRUE, sep = "")
  }
  delivered
}

# Log a CRITICAL line and post it to Slack.
alert <- function(msg) {
  log_msg(sprintf("CRITICAL: %s", msg))
  if (!post_once(sprintf(":rotating_light: prod forecasts\n%s", msg))) {
    log_msg("CRITICAL: the alert above was not delivered (SLACK_WEBHOOK_URL unset or the post failed).")
  }
}

# Post links to the site and to the prod reports and health notebooks rendered
# today, whether or not the run had failures.
announce_reports <- function(failed) {
  site_url <- "https://delphi-forecasting-reports.netlify.app"
  reports <- basename(Sys.glob(here::here("rendered_reports", sprintf("*_on_%s.html", Sys.Date()))))
  reports <- reports[grepl("_(prod|health)_on_", reports)]
  text <- paste(
    c(
      sprintf(
        ":bar_chart: prod forecasts for %s published%s: <%s|reports site>",
        Sys.Date(), if (failed) " with failures (see alert)" else "", site_url
      ),
      sprintf("- <%s/%s|%s>", site_url, reports, reports)
    ),
    collapse = "\n"
  )
  if (!post_once(text)) {
    log_msg("The report announcement was not delivered (SLACK_WEBHOOK_URL unset or the post failed).")
  }
}

today <- Sys.Date()
hour <- as.integer(format(Sys.time(), "%H"))
marker <- here::here("cache", sprintf("prod_forecast_done_%s", today))

if (file.exists(marker)) {
  log_msg(sprintf("Forecast already completed today (%s), skipping.", today))
  quit(status = 0)
}

freshness <- map(c("covid_hosp_prod", "flu_hosp_prod"), function(project) {
  Sys.setenv(TAR_PROJECT = project)
  check_data_freshness() %>% mutate(project = project)
}) %>% bind_rows()
log_msg(paste0("Freshness: ", paste(
  sprintf(
    "%s %s %s (latest %s, %d days old)",
    freshness$project, freshness$source, ifelse(freshness$fresh, "fresh", "stale"),
    freshness$latest, as.integer(freshness$age_days)
  ),
  collapse = "; "
)))

if (!all(freshness$fresh)) {
  log_msg(sprintf("Data is stale (local hour=%d).", hour))
  if (hour >= 14) {
    alert(sprintf(
      "NHSN/NSSP data is still stale at the 14:00 cutoff. Skipping forecast run for %s; upstream data needs investigation.",
      today
    ))
  }
  quit(status = 0)
}

log_msg("Data is fresh, running prod forecasts.")

#' Run `expr`, appending its console/message output to `log_path`. Returns 0
#' on success, 1 if `expr` raises an error.
run_logged_r <- function(log_path, expr) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(log_path, open = "a")
  sink(con, append = TRUE, split = TRUE)
  sink(con, append = TRUE, type = "message")
  result <- tryCatch(
    {
      force(expr)
      0L
    },
    error = function(e) {
      message(conditionMessage(e))
      1L
    }
  )
  sink(type = "message")
  sink()
  close(con)
  result
}

#' Run an external command, appending its combined stdout/stderr to
#' `log_path`. Returns the command's exit status, or 127 if the command cannot
#' run (for example, it is not on PATH).
run_logged <- function(command, args, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  output <- tryCatch(
    system2(command, args, stdout = TRUE, stderr = TRUE),
    error = function(cond) {
      structure(sprintf("%s: %s (PATH=%s)", command, conditionMessage(cond), Sys.getenv("PATH")), status = 127L)
    }
  )
  cat(output, sep = "\n", file = log_path, append = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) 0L else status
}

run_project_pipeline <- function(project, log_path) {
  store <- targets::tar_config_get("store", project = project)
  script <- targets::tar_config_get("script", project = project)
  dir.create(store, showWarnings = FALSE)
  run_logged_r(log_path, targets::tar_make(store = store, script = script))
}

pipelines <- list(
  list(project = "covid_hosp_prod", disease = "covid", log = here::here("cache", "logs", "prod_covid")),
  list(project = "flu_hosp_prod", disease = "flu", log = here::here("cache", "logs", "prod_flu"))
)
publish_steps <- list(
  list(
    name = "sync reports to S3",
    run = function() {
      log_path <- here::here("cache", "logs", "update_site_log.txt")
      upload <- run_logged("aws", c("s3", "sync", "rendered_reports/", "s3://forecasting-team-data/2024/reports/"), log_path)
      download <- run_logged("aws", c("s3", "sync", "s3://forecasting-team-data/2024/reports/", "rendered_reports/"), log_path)
      max(upload, download)
    }
  ),
  list(
    name = "update site",
    run = function() run_logged_r(here::here("cache", "logs", "update_site_log.txt"), update_site())
  ),
  list(
    name = "netlify deploy",
    run = function() run_logged("netlify", c("deploy", "--dir=rendered_reports", "--prod"), here::here("cache", "prod_netlify"))
  )
)

# Run both pipelines, then always render their health notebooks and publish, so
# a failed or degraded run is visible on the site before anyone is alerted.
failures <- character(0)
for (p in pipelines) {
  log_msg(sprintf("Starting: %s prod pipeline", p$disease))
  status <- run_project_pipeline(p$project, p$log)
  log_msg(sprintf("Finished: %s prod pipeline (%s)", p$disease, if (status == 0) "ok" else "failed"))
  if (status != 0) {
    failures <- c(failures, sprintf("%s prod pipeline errored (log: %s)", p$disease, p$log))
  }
}
for (p in pipelines) {
  health <- tryCatch(
    render_prod_health(p$project, p$disease),
    error = function(e) list(status = "error", reasons = sprintf("health notebook failed: %s", conditionMessage(e)), file = NA)
  )
  log_msg(sprintf("%s health: %s. %s", p$disease, health$status, paste(health$reasons, collapse = "; ")))
  if (health$status %in% c("fail", "error")) {
    failures <- c(failures, sprintf(
      "%s health check %s: %s (%s)",
      p$disease, toupper(health$status), paste(health$reasons, collapse = "; "), basename(health$file %||% "no report")
    ))
  }
}
for (step in publish_steps) {
  log_msg(sprintf("Starting: %s", step$name))
  status <- step$run()
  log_msg(sprintf("Finished: %s (%s)", step$name, if (status == 0) "ok" else "failed"))
  if (status != 0) {
    failures <- c(failures, sprintf("%s failed", step$name))
  }
}

announce_reports(failed = length(failures) > 0)
if (length(failures) > 0) {
  alert(paste("-", failures, collapse = "\n"))
  quit(status = 1)
}
file.create(marker)
log_msg(sprintf("Prod forecast run for %s completed successfully.", today))
quit(status = 0)
