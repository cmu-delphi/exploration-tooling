# Summarize today's systemd prod forecast run (see deploy/systemd/README.md).
# Reads the log that scripts/run_prod_if_fresh.R writes and checks that the
# live site serves the local index.
log_file <- here::here("cache", "logs", "prod_forecast_freshness.log")
site_url <- "https://delphi-forecasting-reports.netlify.app/"
today <- format(Sys.Date())

log_lines <- if (file.exists(log_file)) readLines(log_file) else character(0)
todays_lines <- log_lines[startsWith(log_lines, sprintf("[%s", today))]

# Return the result of the last "Finished: <step>" line today, or the reason
# there is none.
step_status <- function(step) {
  finished <- todays_lines[grepl(sprintf("] Finished: %s (", step), todays_lines, fixed = TRUE)]
  if (length(finished) > 0) {
    last <- tail(finished, 1)
    return(sprintf("%s at %s", sub("^.*\\((.*)\\)$", "\\1", last), substr(last, 13, 20)))
  }
  if (any(grepl(sprintf("] Starting: %s", step), todays_lines, fixed = TRUE))) {
    return("started, no finish logged (still running, or the runner died)")
  }
  "not run today"
}

# Named vector of systemd unit properties; empty values become "-".
unit_props <- function(unit, props) {
  out <- system2("systemctl", c("--user", "show", unit, "--no-pager", "-p", paste(props, collapse = ",")), stdout = TRUE)
  values <- sub("^[^=]*=", "", out)
  values[!nzchar(values)] <- "-"
  setNames(values, sub("=.*$", "", out))
}

cat("== timer ==\n")
timer <- unit_props("prod-forecasts.timer", c("ActiveState", "LastTriggerUSec", "NextElapseUSecRealtime"))
service <- unit_props("prod-forecasts.service", c("ActiveState", "Result", "ExecMainStatus", "ExecMainExitTimestamp"))
cat(sprintf(
  "%-15s %s\n",
  c("timer:", "last trigger:", "next trigger:", "service:", "last result:"),
  c(
    timer[["ActiveState"]], timer[["LastTriggerUSec"]], timer[["NextElapseUSecRealtime"]],
    service[["ActiveState"]],
    sprintf("%s (exit %s, finished %s)", service[["Result"]], service[["ExecMainStatus"]], service[["ExecMainExitTimestamp"]])
  )
), sep = "")
if (service[["Result"]] != "success") {
  cat("last journal lines:\n")
  system2("journalctl", c("--user", "-u", "prod-forecasts.service", "-n", "10", "--no-pager"))
}

cat("\n== data freshness ==\n")
last_poll <- tail(grep("\\] Data is (fresh|stale)", log_lines, value = TRUE), 1)
if (length(last_poll) == 0) {
  cat(sprintf("no freshness poll logged in %s\n", log_file))
} else {
  cat(sprintf("last poll: %s\n", last_poll))
  last_freshness <- tail(grep("] Freshness: ", log_lines, value = TRUE, fixed = TRUE), 1)
  if (length(last_freshness) > 0) {
    sources <- strsplit(sub("^.*Freshness: ", "", last_freshness), "; ", fixed = TRUE)[[1]]
    cat(paste0("  ", sources), sep = "\n")
  }
}

cat("\n== upstream (epidata) ==\n")
invisible(suppressMessages(loadNamespace("epidatr")))
if (!nzchar(epidatr::get_api_key())) {
  cat("WARNING: no epidata API key set (DELPHI_EPIDATA_KEY); requests are anonymous and rate limited\n")
}
for (source in c("nhsn", "nssp")) {
  meta <- tryCatch(epidatr::epidata_meta(source), error = function(cond) conditionMessage(cond))
  if (is.character(meta)) {
    cat(sprintf("%s: metadata request failed: %s\n", source, meta))
    next
  }
  latest_reference <- as.Date(meta$reference_time_range$latest)
  age_days <- as.integer(Sys.Date() - latest_reference)
  cat(sprintf(
    "%s: %s (latest week %s, %d days old; last report %s)\n",
    source, if (age_days <= 7) "fresh" else "stale", latest_reference, age_days,
    as.Date(meta$report_time_range$latest)
  ))
}

cat(sprintf("\n== today (%s) ==\n", today))
steps <- c(
  "covid prod" = "covid prod pipeline",
  "flu prod" = "flu prod pipeline",
  "s3 sync" = "sync reports to S3",
  "site update" = "update site",
  "netlify" = "netlify deploy"
)
step_statuses <- vapply(steps, step_status, character(1))
cat(sprintf("%-15s %s\n", paste0(names(steps), ":"), step_statuses), sep = "")
done_marker <- here::here("cache", sprintf("prod_forecast_done_%s", today))
cat(if (file.exists(done_marker)) "run marked complete\n" else "run NOT marked complete\n")
critical <- grep("] CRITICAL: ", todays_lines, value = TRUE, fixed = TRUE)
if (length(critical) > 0) cat(paste0("  ", critical), sep = "\n")

cat("\n== live site ==\n")
live_index <- tryCatch(readLines(site_url, warn = FALSE), error = function(cond) NULL)
if (is.null(live_index)) {
  cat(sprintf("could not fetch %s\n", site_url))
} else {
  cat(sprintf("reports generated today listed on live site: %d\n", sum(grepl(sprintf("_on_%s", today), live_index))))
  local_index <- readLines(here::here("rendered_reports", "index.html"), warn = FALSE)
  cat(if (identical(live_index, local_index)) {
    "live index matches local rendered_reports/index.html\n"
  } else {
    "live index DIFFERS from local rendered_reports/index.html\n"
  })
}

for (disease in c("covid", "flu")) {
  if (startsWith(step_statuses[[sprintf("%s prod", disease)]], "failed")) {
    cat(sprintf(
      "\n%s prod failed. To see the errors:\n  make get-%s-prod-errors\n  tail -n 100 cache/logs/prod_%s\n",
      disease, disease, disease
    ))
  }
}
