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

# Dispatch time of each target from the runner log's timestamp reporter lines.
dispatch_times <- function(runner_lines) {
  m <- regmatches(runner_lines, regexec("^(\\S+ \\S+) dispatched target (\\S+)$", runner_lines))
  m <- m[lengths(m) == 3L]
  times <- setNames(vapply(m, `[`, "", 2L), vapply(m, `[`, "", 3L))
  times[!duplicated(names(times), fromLast = TRUE)]
}

cat("\n== in flight (tar_progress; `make logs project=<p>` to follow) ==\n")
for (disease in c("covid", "flu", "rsv")) {
  project <- sprintf("%s_hosp_prod", disease)
  store <- here::here(targets::tar_config_get("store", project = project))
  cat(sprintf("%s: ", project))
  progress <- tryCatch(suppressMessages(targets::tar_progress(store = store)), error = function(cond) NULL)
  dispatched <- if (is.null(progress)) character(0) else progress$name[progress$progress == "dispatched"]
  if (length(dispatched) == 0) {
    cat("nothing dispatched\n")
  } else {
    runner_log <- here::here("cache", "logs", sprintf("prod_%s", disease))
    since <- dispatch_times(if (file.exists(runner_log)) readLines(runner_log) else character(0))[dispatched]
    elapsed <- difftime(Sys.time(), as.POSIXct(since), units = "mins")
    cat(sprintf("%d dispatched (stays so if the runner died)\n", length(dispatched)))
    cat(sprintf("  %-60s since %s (%.1f min)\n", dispatched, substr(since, 12, 19), as.numeric(elapsed)), sep = "")
  }
  meta <- tryCatch(
    suppressMessages(targets::tar_meta(fields = c(time, warnings, error), store = store)),
    error = function(cond) NULL
  )
  if (!is.null(meta)) {
    meta <- meta[!is.na(meta$time) & format(meta$time, "%F") == today, ]
    problems <- c(
      sprintf("warning %s: %s", meta$name, meta$warnings)[!is.na(meta$warnings)],
      sprintf("error %s: %s", meta$name, meta$error)[!is.na(meta$error)]
    )
    if (length(problems) > 0) {
      cat(sprintf("  %d targets with warnings or errors today (tar_meta); last:\n", length(problems)))
      cat(paste0("    ", substring(tail(problems, 3), 1, 200)), sep = "\n")
    }
  }
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

suppressPackageStartupMessages({
  library(magrittr)
  library(purrr)
})

# Print the number of geos with a value in the latest week and in the week
# before. `counts` is named by week, latest first.
cat_geo_counts <- function(label, counts) {
  cat(sprintf(
    "  %-30s %s geos on %s (%s the week before)\n",
    paste0(label, ":"), counts[1], names(counts)[1], counts[2]
  ))
}

cat("\n== upstream (epidata) ==\n")
Sys.setenv(EPIDATR_USE_CACHE = "FALSE")
invisible(suppressMessages(loadNamespace("epidatr")))
if (!nzchar(epidatr::get_api_key())) {
  cat("WARNING: no epidata API key set (DELPHI_EPIDATA_KEY); requests are anonymous and rate limited\n")
}
epidata_signals <- list(
  "nhsn" = c("confirmed_admissions_covid_ew", "confirmed_admissions_flu_ew", "confirmed_admissions_rsv_ew"),
  "nssp" = c("pct_ed_visits_covid", "pct_ed_visits_influenza", "pct_ed_visits_rsv")
)
# Count the states with a value for each of the latest two reference weeks.
epidata_latest_counts <- function(source, signal) {
  window <- epidatr::epirange(format(Sys.Date() - 28, "%Y%m%d"), format(Sys.Date(), "%Y%m%d"))
  snapshot <- epidatr::epidata_snapshot(source, signal, geo_type = "state", reference_time = window)
  rev(tail(table(as.character(snapshot$reference_time[!is.na(snapshot$value)])), 2))
}
for (epidata_source in names(epidata_signals)) {
  meta <- tryCatch(epidatr::epidata_meta(epidata_source), error = function(cond) conditionMessage(cond))
  if (is.character(meta)) {
    cat(sprintf("%s: metadata request failed: %s\n", epidata_source, meta))
    next
  }
  reported_at <- as.POSIXct(meta$report_time_range$latest, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  age_days <- as.numeric(difftime(Sys.time(), reported_at, units = "days"))
  cat(sprintf(
    "%-12s %s (last report %s, %.1f days ago)\n",
    paste0(epidata_source, ":"), if (age_days <= 7) "fresh" else "stale", format(reported_at, "%F"), age_days
  ))
  for (signal in epidata_signals[[epidata_source]]) {
    counts <- tryCatch(epidata_latest_counts(epidata_source, signal), error = function(cond) conditionMessage(cond))
    if (is.character(counts)) {
      cat(sprintf("  %-30s request failed: %s\n", paste0(signal, ":"), counts))
      next
    }
    cat_geo_counts(signal, counts)
  }
}

cat("\n== upstream (socrata) ==\n")
source(here::here("R", "data", "data_freshness.R"))
nhsn_columns <- c(covid = "totalconfc19newadm", flu = "totalconfflunewadm", rsv = "totalconfrsvnewadm")
socrata_datasets <- list(
  "nhsn" = list(id = "ua7e-t2fy", date_column = "weekendingdate", columns = nhsn_columns),
  "nhsn prelim" = list(id = "mpgq-jmmr", date_column = "weekendingdate", columns = nhsn_columns),
  "nssp" = list(
    id = "rdmq-nq56", date_column = "week_end", where = "county = 'All'",
    columns = c(covid = "percent_visits_covid", flu = "percent_visits_influenza", rsv = "percent_visits_rsv")
  )
)
# Count the rows with a value in each column for the latest two weeks. Returns
# one named count vector per column.
socrata_latest_counts <- function(dataset) {
  response <- httr::GET(
    sprintf("https://data.cdc.gov/resource/%s.json", dataset$id),
    query = compact(list(
      `$select` = paste(c(dataset$date_column, sprintf("count(%s)", dataset$columns)), collapse = ","),
      `$where` = dataset$where,
      `$group` = dataset$date_column,
      `$order` = paste(dataset$date_column, "DESC"),
      `$limit` = 2
    )),
    httr::timeout(30)
  )
  httr::stop_for_status(response)
  weeks <- httr::content(response)
  map(dataset$columns, function(column) {
    setNames(
      map_int(weeks, ~ as.integer(.x[[paste0("count_", column)]])),
      map_chr(weeks, ~ substr(.x[[dataset$date_column]], 1, 10))
    )
  })
}
for (name in names(socrata_datasets)) {
  dataset <- socrata_datasets[[name]]
  metadata_url <- sprintf("https://data.cdc.gov/api/views/%s", dataset$id)
  updated_at <- get_socrata_updated_at(metadata_url, missing_value = NA)
  if (is.na(updated_at)) {
    cat(sprintf("%-12s metadata request failed (%s)\n", paste0(name, ":"), metadata_url))
    next
  }
  age_days <- as.numeric(difftime(Sys.time(), updated_at, units = "days"))
  cat(sprintf(
    "%-12s %s (rows updated %s, %.1f days ago)\n",
    paste0(name, ":"), if (age_days <= 7) "fresh" else "stale",
    format(updated_at, "%F %H:%M %Z", tz = Sys.timezone()), age_days
  ))
  counts <- tryCatch(socrata_latest_counts(dataset), error = function(cond) conditionMessage(cond))
  if (is.character(counts)) {
    cat(sprintf("  count request failed: %s\n", counts))
    next
  }
  iwalk(counts, ~ cat_geo_counts(dataset$columns[[.y]], .x))
}

cat(sprintf("\n== today (%s) ==\n", today))
steps <- c(
  "covid prod" = "covid prod pipeline",
  "flu prod" = "flu prod pipeline",
  "rsv prod" = "rsv prod pipeline",
  "s3 sync" = "sync reports to S3",
  "site update" = "update site",
  "netlify" = "netlify deploy"
)
step_statuses <- vapply(steps, step_status, character(1))
cat(sprintf("%-15s %s\n", paste0(names(steps), ":"), step_statuses), sep = "")
done_marker <- here::here("cache", sprintf("prod_forecast_done_%s", today))
cat(if (file.exists(done_marker)) "run marked complete\n" else "run NOT marked complete\n")
handoff_marker <- here::here("cache", sprintf("prod_forecast_handed_off_%s", today))
if (file.exists(handoff_marker)) cat("failed run was published and handed off; automatic retries stopped\n")
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

for (disease in c("covid", "flu", "rsv")) {
  if (startsWith(step_statuses[[sprintf("%s prod", disease)]], "failed")) {
    cat(sprintf(
      "\n%s prod failed. To see the errors:\n  make get-%s-prod-errors\n  tail -n 100 cache/logs/prod_%s\n",
      disease, disease, disease
    ))
  }
}
