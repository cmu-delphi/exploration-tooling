# Backfill missing Wednesday blocks in a geo-exclusions weights csv from git history.
#
# Usage: Rscript scripts/backfill_geo_exclusions.R [pipelines/covid_geo_exclusions.csv]
#
# For each Wednesday since the file first appeared, take the version of the file
# that prod would have used: the last commit before the next Wednesday. Dates in
# `keep_current` are not changed. Resolve
# the weights for that date the way `parse_prod_weights()` does: the rows dated
# that Wednesday, else the rows of the earliest date (the defaults), with later
# duplicate keys overriding earlier ones.
#
# Wednesdays without a block in the current file get a block of the resolved
# historical weights. Wednesdays with a block get compared to the historical
# weights. If they differ, the historical rows replace the current rows, and the
# script prints every difference.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
filename <- if (length(args) > 0) args[[1]] else "pipelines/covid_geo_exclusions.csv"
weight_keys <- c("forecaster", "geo_value", "ahead")
# Forecast dates where the current file has the weights that prod used, per file.
# Usually the weights were committed after the cutoff.
keep_current <- as.Date(list(
  "pipelines/covid_geo_exclusions.csv" = c(
    "2024-11-20", "2025-02-19", "2025-07-30", "2025-08-06", "2025-08-20",
    "2025-09-03", "2025-12-31", "2026-08-26"
  ),
  "pipelines/covid_nssp_geo_exclusions.csv" = c(
    "2025-06-25", "2025-07-23", "2025-07-30", "2025-08-06", "2025-08-13",
    "2025-08-20", "2026-08-26"
  )
)[[filename]])

git <- function(...) {
  out <- system2("git", shQuote(c(...)), stdout = TRUE)
  if (!is.null(attr(out, "status"))) stop("git ", paste(c(...), collapse = " "), " failed")
  out
}

# One row per commit that touched the file, with the path of the file at that
# commit. `--follow` also follows copies, so the history stops at the commit
# that added or copied the file.
file_commits <- function(filename) {
  log_lines <- git("log", "--follow", "--name-status", "--format=@@%H %aI", "--", filename)
  log_lines <- log_lines[log_lines != ""]
  header_idx <- which(startsWith(log_lines, "@@"))
  headers <- strsplit(sub("^@@", "", log_lines[header_idx]), " ")
  status_fields <- strsplit(log_lines[header_idx + 1], "\t")
  commits <- tibble(
    sha = vapply(headers, `[[`, character(1), 1),
    commit_date = as.Date(substr(vapply(headers, `[[`, character(1), 2), 1, 10)),
    status = substr(vapply(status_fields, `[[`, character(1), 1), 1, 1),
    path = vapply(status_fields, \(fields) fields[[length(fields)]], character(1))
  )
  created_idx <- which(commits$status %in% c("A", "C"))[[1]]
  commits %>%
    slice(rev(seq_len(created_idx))) %>%
    select(-status)
}

read_weights <- function(text) {
  raw <- read_csv(I(paste(text, collapse = "\n")), comment = "#", show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!"ahead" %in% names(raw)) raw$ahead <- NA_character_
  raw %>%
    transmute(
      forecast_date = as.Date(forecast_date),
      forecaster,
      geo_value,
      ahead = as.integer(ahead),
      weight = as.numeric(weight)
    ) %>%
    filter(!is.na(forecast_date))
}

# Rows that `parse_prod_weights()` uses for `forecast_date`, in file order, with
# only the last row of each key kept.
resolve_weights <- function(weights, forecast_date) {
  dated <- filter(weights, .data$forecast_date == .env$forecast_date)
  if (nrow(dated) == 0) {
    dated <- filter(weights, .data$forecast_date == min(weights$forecast_date))
  }
  dated %>%
    mutate(row_idx = row_number()) %>%
    slice_tail(n = 1, by = all_of(weight_keys)) %>%
    arrange(row_idx) %>%
    select(all_of(weight_keys), weight)
}

compare_weights <- function(historical, current) {
  full_join(historical, current, by = weight_keys, suffix = c("_historical", "_current")) %>%
    filter(is.na(weight_historical) | is.na(weight_current) | !near(weight_historical, weight_current))
}

format_rows <- function(weights, forecast_date) {
  weight_str <- vapply(weights$weight, format, character(1), scientific = FALSE, drop0trailing = TRUE, digits = 15)
  sprintf(
    "%s, %s, %s,%s, %s",
    format(forecast_date, "%Y-%m-%d"), weights$forecaster, weights$geo_value,
    ifelse(is.na(weights$ahead), "", paste0(" ", weights$ahead)), weight_str
  )
}

format_block <- function(weights, forecast_date, sha) {
  c(
    "##################",
    sprintf("# %s: backfilled from the defaults as of %s", format(forecast_date, "%b %-d"), substr(sha, 1, 7)),
    "##################",
    format_rows(weights, forecast_date)
  )
}

# Remove all the rows of `forecast_date` and put the historical rows where the
# first removed row was. Comment lines stay where they are.
overwrite_block <- function(lines, weights, forecast_date, sha) {
  date_idx <- which(startsWith(lines, format(forecast_date, "%Y-%m-%d")))
  replacement <- c(sprintf("# overwritten with the weights as of %s", substr(sha, 1, 7)), format_rows(weights, forecast_date))
  append(lines[-date_idx], replacement, after = date_idx[[1]] - 1)
}

# Put the block above the first dated block that is older than `forecast_date`,
# together with the comment header of that block. The defaults blocks do not count.
insert_block <- function(lines, block, forecast_date, default_date) {
  line_dates <- suppressWarnings(as.Date(substr(lines, 1, 10), format = "%Y-%m-%d"))
  older_idx <- which(!is.na(line_dates) & line_dates < forecast_date & line_dates != default_date)
  if (length(older_idx) == 0) {
    return(c(lines, block))
  }
  insert_at <- older_idx[[1]]
  while (insert_at > 1 && startsWith(lines[[insert_at - 1]], "#")) insert_at <- insert_at - 1
  append(lines, block, after = insert_at - 1)
}

commits <- file_commits(filename)
current_lines <- readLines(filename)
current_weights <- read_weights(current_lines)
default_date <- min(current_weights$forecast_date)

first_wednesday <- min(commits$commit_date) + (3 - as.integer(format(min(commits$commit_date), "%u"))) %% 7
last_wednesday <- Sys.Date() - (as.integer(format(Sys.Date(), "%u")) - 3) %% 7
wednesdays <- seq(first_wednesday, last_wednesday, by = "week")

snapshot_cache <- list()
report <- list()
new_lines <- current_lines
for (ii in seq_along(wednesdays)) {
  wednesday <- wednesdays[[ii]]
  if (wednesday %in% keep_current) {
    report[[ii]] <- tibble(forecast_date = wednesday, status = "kept")
    next
  }
  snapshot <- commits %>%
    filter(commit_date < wednesday + 7) %>%
    slice_tail(n = 1)
  if (is.null(snapshot_cache[[snapshot$sha]])) {
    snapshot_cache[[snapshot$sha]] <- read_weights(git("show", sprintf("%s:%s", snapshot$sha, snapshot$path)))
  }
  historical <- resolve_weights(snapshot_cache[[snapshot$sha]], wednesday)

  if (!wednesday %in% current_weights$forecast_date) {
    new_lines <- insert_block(new_lines, format_block(historical, wednesday, snapshot$sha), wednesday, default_date)
    report[[ii]] <- tibble(forecast_date = wednesday, sha = substr(snapshot$sha, 1, 7), status = "backfilled")
    next
  }
  diffs <- compare_weights(historical, resolve_weights(current_weights, wednesday))
  if (nrow(diffs) > 0) {
    new_lines <- overwrite_block(new_lines, historical, wednesday, snapshot$sha)
  }
  report[[ii]] <- tibble(
    forecast_date = wednesday,
    sha = substr(snapshot$sha, 1, 7),
    status = if (nrow(diffs) == 0) "consistent" else "overwritten",
    diffs = list(diffs)
  )
}
report <- bind_rows(report)

writeLines(new_lines, filename)

cat(sprintf("%s: %s\n", filename, paste(names(table(report$status)), table(report$status), sep = " = ", collapse = ", ")))
cat("\nBackfilled:", format(report$forecast_date[report$status == "backfilled"]), fill = 80)
differing <- filter(report, status == "overwritten")
for (jj in seq_len(nrow(differing))) {
  cat(sprintf("\n%s overwritten with %s:\n", differing$forecast_date[[jj]], differing$sha[[jj]]))
  print(differing$diffs[[jj]], n = Inf)
}
