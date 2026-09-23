#' Validate a hand-edited prod weights csv has the required columns and that
#' `forecast_date` is parseable for every row. Cheap structural checks run
#' over the whole file, independent of which forecast_date is being requested
#' (a malformed row in a stale/historical block is still worth catching).
validate_prod_weights_columns <- function(raw, filename) {
  required_cols <- c("forecast_date", "forecaster", "geo_value", "weight")
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.file {filename}}: missing required column(s) {.val {missing_cols}}")
  }
  if (!inherits(raw$forecast_date, "Date")) {
    # readr guesses the whole column as character if any entry doesn't match
    # a date format; recover per-row so the error can name the offending value.
    # as.Date() throws (rather than returns NA) on totally unrecognized
    # formats, so parse element-wise and treat throws as unparseable too.
    parsed <- vapply(raw$forecast_date, function(x) {
      tryCatch(as.character(as.Date(x)), error = function(e) NA_character_)
    }, character(1))
    bad <- unique(raw$forecast_date[is.na(parsed)])
    if (length(bad) > 0) {
      cli::cli_abort("{.file {filename}}: unparseable `forecast_date` value(s) {.val {bad}}")
    }
  }
  invisible(raw)
}

# `ensemble_climate_linear()` doesn't join `other_weights` against a
# registered forecaster list -- it matches by `grepl("climate|linear",
# forecaster)` against whatever ids are actually present in the forecast
# frame. So the weights csvs carry a few names that never correspond to a
# currently-active `g_forecaster_params_grid$id` and never will: "linearlog"
# and "climate_quantile_extrapolated" (retired forecasters, kept at weight 0
# as inert documentation across every date block) and "climate_linear"
# (predates the `climate_linear` ensemble target of the same name; never a
# base forecaster id, always inert). All three are harmless -- they simply
# never match a real `forecaster` column value downstream -- so they're
# whitelisted alongside the active ids rather than flagged as typos.
LEGACY_PROD_WEIGHT_FORECASTER_IDS <- c("linearlog", "climate_quantile_extrapolated", "climate_linear")

#' Validate the forecaster/geo/weight values that will actually drive this
#' forecast_date's ensemble weighting (i.e. the post-date-filter subset, since
#' only one date block is ever consumed per call -- historical blocks for
#' other dates are inert and not checked here).
validate_prod_weights_values <- function(weights, filename, forecaster_fn_names, all_geos) {
  if (!is.numeric(weights$weight) || any(!is.finite(weights$weight))) {
    bad <- weights$weight[!is.numeric(weights$weight) | !is.finite(weights$weight)]
    cli::cli_abort("{.file {filename}}: non-numeric or non-finite `weight` value(s) {.val {bad}}")
  }
  if (any(weights$weight < 0)) {
    bad <- unique(weights$weight[weights$weight < 0])
    cli::cli_abort("{.file {filename}}: negative `weight` value(s) {.val {bad}} (weights must be >= 0; they're renormalized as relative mass downstream)")
  }
  known_forecasters <- c("all", forecaster_fn_names, LEGACY_PROD_WEIGHT_FORECASTER_IDS)
  bad_forecasters <- setdiff(unique(weights$forecaster), known_forecasters)
  if (length(bad_forecasters) > 0) {
    cli::cli_abort(c(
      "{.file {filename}}: unknown forecaster id(s) {.val {bad_forecasters}} in column `forecaster`",
      "i" = "expected one of {.val {forecaster_fn_names}}, the sentinel {.val all}, or a known-legacy id {.val {LEGACY_PROD_WEIGHT_FORECASTER_IDS}}"
    ))
  }
  bad_geos <- setdiff(unique(weights$geo_value), c("all", all_geos))
  if (length(bad_geos) > 0) {
    cli::cli_abort("{.file {filename}}: unknown `geo_value`(s) {.val {bad_geos}}")
  }
  invisible(weights)
}

parse_prod_weights <- function(filename, forecast_date_int, forecaster_fn_names) {
  forecast_date_val <- as.Date(forecast_date_int)
  all_states <- c(
    unique(
      readr::read_csv(
        "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv",
        show_col_types = FALSE
      )$state_id
    ),
    "usa",
    "us"
  )
  all_prod_weights <- readr::read_csv(filename, comment = "#", show_col_types = FALSE)
  validate_prod_weights_columns(all_prod_weights, filename)
  has_ahead <- "ahead" %in% names(all_prod_weights)
  # if we haven't set specific weights, use the overall defaults
  useful_prod_weights <- filter(all_prod_weights, forecast_date == forecast_date_val)
  if (nrow(useful_prod_weights) == 0) {
    useful_prod_weights <- all_prod_weights %>%
      filter(forecast_date == min(forecast_date)) %>%
      mutate(forecast_date = forecast_date_val)
  }
  validate_prod_weights_values(useful_prod_weights, filename, forecaster_fn_names, all_states)
  # weights that apply to specific states
  state_weights <- useful_prod_weights %>%
    filter(geo_value != "all") %>%
    mutate(
      forecaster = ifelse(forecaster == "all", list(forecaster_fn_names), forecaster),
    ) %>%
    unnest_longer(forecaster)
  forecaster_weights <-
    useful_prod_weights %>%
    filter(geo_value == "all") %>%
    mutate(
      geo_value = list(all_states)
    ) %>%
    unnest_longer(geo_value)
  # bind together and overwrite any generic weights with geo_specific ones.
  # Group includes `ahead` when present so ahead-specific rows override only
  # their matching (forecaster, geo_value, ahead) combination.
  group_cols <- c("forecast_date", "forecaster", "geo_value")
  if (has_ahead) group_cols <- c(group_cols, "ahead")
  forecaster_weights %>%
    bind_rows(state_weights) %>%
    group_by(across(all_of(group_cols))) %>%
    filter(row_number() == n()) %>%
    mutate(forecast_date = as.Date(forecast_date_int)) %>%
    ungroup()
}

exclude_geos <- function(geo_forecasters_weights) {
  geo_exclusions <- geo_forecasters_weights %>%
    group_by(forecast_date, geo_value) %>%
    filter(near(max(weight), 0)) %>%
    pull(geo_value) %>%
    unique()
}

#' Append a dated copy of the default weights block to a geo-exclusions CSV.
#'
#' Reads the file as raw text, copies every line whose date matches the earliest
#' date in the file (the defaults block), replaces that date with
#' `forecast_date`, and inserts the new lines immediately after the last default
#' line. Does nothing if `forecast_date` is already present.
stamp_default_weights <- function(filename, forecast_date) {
  date_str <- format(as.Date(forecast_date), "%Y-%m-%d")
  lines <- readLines(filename)
  parsed <- readr::read_csv(filename, comment = "#", show_col_types = FALSE)
  default_date_str <- format(min(parsed$forecast_date), "%Y-%m-%d")
  if (date_str %in% format(parsed$forecast_date, "%Y-%m-%d")) {
    return(invisible(NULL))
  }
  all_default_indices <- which(startsWith(trimws(lines), default_date_str))
  if (length(all_default_indices) == 0) return(invisible(NULL))
  # Only copy the first contiguous block (the actual defaults section).
  # Later sections that reuse the default date are real dated entries.
  if (length(all_default_indices) > 1) {
    first_break <- which(diff(all_default_indices) > 1)[1]
    if (!is.na(first_break)) all_default_indices <- all_default_indices[seq_len(first_break)]
  }
  header <- c(
    "##################",
    paste0("# ", format(as.Date(forecast_date), "%b %-d")),
    "##################"
  )
  new_lines <- sub(default_date_str, date_str, lines[all_default_indices], fixed = TRUE)
  last_idx <- max(all_default_indices)
  writeLines(
    c(lines[seq_len(last_idx)], header, new_lines, lines[seq(last_idx + 1L, length(lines))]),
    filename
  )
  invisible(NULL)
}
