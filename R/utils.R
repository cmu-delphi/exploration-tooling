EPIDATA_V5_URL <- "https://delphi.cmu.edu/epidata/v5"
#' Look up forecasters by name
#'
#' Given a (partial) forecaster name, look up all forecasters in the given
#' project which contain part of that name.
#'
#' @param forecaster_grid the forecaster grid to search.
#' @param pattern string to search in the forecaster name.
#'
#' @export
forecaster_lookup <- function(pattern, forecaster_params_grid = NULL) {
  if (is.null(forecaster_params_grid)) {
    if (!exists("g_forecaster_params_grid")) {
      cli::cli_warn(
        "Reading `forecaster_params_grid` target. If it's not up to date, results will be off.
    Update with `tar_make(g_forecaster_params_grid)`."
      )
      forecaster_params_grid <- tar_read_raw("forecaster_params_grid")
    } else {
      forecaster_params_grid <- forecaster_params_grid %||% g_forecaster_params_grid
    }
  }

  # Remove common prefix for convenience.
  if (grepl("forecast_", pattern)) {
    pattern <- gsub("forecast_", "", pattern)
  }
  if (grepl("forecaster_", pattern)) {
    pattern <- gsub("forecaster_", "", pattern)
  }

  out <- forecaster_params_grid %>% filter(grepl(pattern, .data$id))
  if (nrow(out) > 0) {
    out %>% unlist()
    return(out)
  }
}

#' Add a unique id based on the column contents
#'
#' Create a string of `n_adj` that is a hash of the parameters.
#'
#' @param tib the tibble to add a column to. everything should be convertable to a string
#' @param exclude a vector of column names to exclude from the hash
#'
#' @export
add_id <- function(tib, exclude = c()) {
  ids <- tib %>%
    select(-all_of(exclude)) %>%
    purrr::transpose() %>%
    map_chr(get_single_id)
  tib %>%
    mutate(id = ids) %>%
    relocate(id, .before = everything())
}

#' Generate a two-word id from a simple list of parameters.
#'
#' @param param_list a list of parameters.
#'
#' @export
get_single_id <- function(param_list) {
  param_list[sort(names(param_list))] %>%
    paste(sep = "", collapse = "") %>%
    gsub("[[:blank:]]", "", .) %>%
    cli::hash_animal(n_adj = 1) %>%
    purrr::pluck("words", 1) %>%
    paste(sep = ".", collapse = ".")
}

# Per-forecaster wrapping/metadata read by run_forecaster() and scoring rather
# than passed to the forecaster as a modeling parameter. Declared inline in the
# parameter tibbles; make_forecaster_grid() separates them from the params
# list-column and fills these defaults for whatever a forecaster omits, so both
# explore and prod grids carry them uniformly (replacing prod's metadata
# left_join and explore's per-script column stamping).
#   as_of_policy      "asof"/"cheating": train on as-of-generation-date data or
#                     the latest available revision.
#   ahead_multiplier  factor the (weekly) ahead is multiplied by before it
#                     reaches the forecaster: 1 for week-native forecasters
#                     (cdc/linear/climate), 7 for day-native ones (scaled_pop*).
#   target_date_shift days added to target_end_date after forecasting (Wed->Sat).
#   join_extra_data   left-join extra_data before forecasting (drop source after).
#   filter_sources    if non-NULL, keep only these sources in the input data.
#   excluded_geos     geos dropped from the output.
#   sort_quantiles    enforce quantile monotonicity (the flu whitening workaround).
#   output_scale      "count"/"per100k": whether scoring rescales to counts.
#   min_train_date    if non-NULL (declare as list(as.Date(...))), drop training
#                     rows before this absolute date at the snapshot boundary
#                     (covid cdc_baseline uses it to bound quantile spread).
#   needs_archive     TRUE hands the forecaster the truncated epi_archive instead
#                     of an as-of epi_df snapshot, for revision-aware forecasters
#                     (see make_forecast_archive_snapshot / run_forecaster).
FORECASTER_SPEC_DEFAULTS <- list(
  as_of_policy = "asof",
  ahead_multiplier = 1L,
  target_date_shift = 0L,
  join_extra_data = FALSE,
  filter_sources = NULL,
  excluded_geos = NULL,
  sort_quantiles = FALSE,
  output_scale = "count",
  min_train_date = NULL,
  needs_archive = FALSE
)

#' Make a forecaster grid.
#'
#' Convert a tibble of forecasters and their parameters to a specific format
#' that we can iterate over in targets. Currently only `forecaster` and
#' `trainer` can be symbols. Any of the columns in `FORECASTER_SPEC_DEFAULTS`
#' are treated as per-forecaster spec/metadata (kept as top-level grid columns,
#' filled with defaults when absent) rather than forecaster parameters.
#'
#' @param tib the tibble of parameters. Must have the forecaster and trainer
#' columns, everything else is optional.
#'
#' @export
make_forecaster_grid <- function(tib, family) {
  if ("trainer" %in% colnames(tib)) {
    tib$trainer <- rlang::syms(tib$trainer)
  }
  spec_names <- names(FORECASTER_SPEC_DEFAULTS)
  # turns a tibble into a list of named lists (spec columns are not params)
  params_list <- tib %>%
    select(-any_of(c("forecaster", "id", spec_names))) %>%
    split(seq_len(nrow(.))) %>%
    unname() %>%
    lapply(as.list)
  # for whatever reason, trainer ends up being a list of lists, which we do not want
  params_list %<>%
    lapply(function(x) {
      x$trainer <- x$trainer[[1]]
      x$lags <- x$lags[[1]]
      x
    })

  if (length(params_list) == 0) {
    out <- tibble(
      family = family,
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = list(list())
    )
  } else {
    out <- tibble(
      family = family,
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = params_list
    )
  }

  # Attach spec columns, taking each forecaster's inline override when present
  # and the shared default otherwise.
  for (col in spec_names) {
    default <- FORECASTER_SPEC_DEFAULTS[[col]]
    if (col %in% colnames(tib)) {
      out[[col]] <- tib[[col]]
    } else if (is.null(default)) {
      # list-valued spec (filter_sources / excluded_geos): default is NULL
      out[[col]] <- rep(list(NULL), nrow(out))
    } else {
      out[[col]] <- rep(default, nrow(out))
    }
  }

  return(out)
}

data_substitutions <- function(dataset, substitutions_path, forecast_generation_date) {
  # Get the substitutions from the table, matched by forecast generation date
  substitutions <- readr::read_csv(
    substitutions_path,
    comment = "#",
    show_col_types = FALSE,
    col_types = readr::cols(
      geo_value = readr::col_character(),
      forecast_date = readr::col_date(),
      time_value = readr::col_date(),
      value = readr::col_double()
    )
  ) %>%
    filter(forecast_date == forecast_generation_date) %>%
    select(-forecast_date) %>%
    rename(new_value = value) %>%
    mutate(
      time_value = floor_date(time_value, "week", week_start = 7) + 3
    )
  # Replace the most recent values in the appropriate keys with the substitutions
  new_values <- dataset %>%
    inner_join(substitutions, by = join_by(geo_value, time_value)) %>%
    mutate(value = ifelse(!is.na(new_value), new_value, value)) %>%
    select(-new_value)
  # Remove keys from dataset that have been substituted
  dataset %>%
    anti_join(new_values, by = c("geo_value", "time_value")) %>%
    bind_rows(new_values)
}

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

#' `ensemble_climate_linear()` doesn't join `other_weights` against a
#' registered forecaster list -- it matches by `grepl("climate|linear",
#' forecaster)` against whatever ids are actually present in the forecast
#' frame. So the weights csvs carry a few names that never correspond to a
#' currently-active `g_forecaster_params_grid$id` and never will: "linearlog"
#' and "climate_quantile_extrapolated" (retired forecasters, kept at weight 0
#' as inert documentation across every date block) and "climate_linear"
#' (predates the `climate_linear` ensemble target of the same name; never a
#' base forecaster id, always inert). All three are harmless -- they simply
#' never match a real `forecaster` column value downstream -- so they're
#' whitelisted alongside the active ids rather than flagged as typos.
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
  # bind together and overwrite any generic weights with geo_specific ones
  forecaster_weights %>%
    bind_rows(state_weights) %>%
    group_by(forecast_date, forecaster, geo_value) %>%
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

`%nin%` <- function(x, y) !(x %in% y)

get_population_data <- function() {
  readr::read_csv(
    "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv",
    show_col_types = FALSE
  ) %>%
    rename(population = pop) %>%
    # Add a row for the United States
    bind_rows(
      (.) %>% summarize(state_id = "us", population = sum(population), state_name = "United States", state_code = "US")
    ) %>%
    # Duplicate the last row, but with state_id = "usa".
    bind_rows((.) %>% filter(state_id == "us") %>% mutate(state_id = "usa"))
}

filter_forecast_geos <- function(forecasts, truth_data) {
  subset_geos <- unique(forecasts$geo_value)
  # Bad forecast filters
  c(
    # 1. Filter out forecasts that trend down
    tibble(
      geo_value = subset_geos,
      trend_down = map(
        subset_geos,
        ~ lm(value ~ target_end_date, data = forecasts %>% filter(geo_value == .x))$coefficients[2] < 0
      ) %>%
        unlist()
    ) %>%
      filter(trend_down) %>%
      pull(geo_value),
    # 2. Filter forecasts where the median exceeds all prior peaks at any ahead.
    tibble(
      geo_value = subset_geos
    ) %>%
      left_join(
        forecasts %>% filter(quantile == 0.5) %>% group_by(geo_value) %>% summarize(mv = max(value)),
        by = "geo_value"
      ) %>%
      left_join(
        truth_data %>% group_by(geo_value) %>% summarize(pp = max(value, na.rm = TRUE)),
        by = "geo_value"
      ) %>%
      filter(mv >= pp) %>%
      pull(geo_value),
    # 3. If .75 quantile exceeds all prior peaks at 2 ahead, filter out.
    tibble(
      geo_value = subset_geos
    ) %>%
      left_join(
        forecasts %>%
          filter(
            near(quantile, 0.75),
            target_end_date == MMWRweek2Date(epiyear(forecast_date), epiweek(forecast_date)) + 6
          ),
        by = "geo_value"
      ) %>%
      left_join(
        truth_data %>% group_by(geo_value) %>% summarize(pp = max(value, na.rm = TRUE)),
        by = "geo_value"
      ) %>%
      filter(value >= pp) %>%
      pull(geo_value)
  ) %>%
    unique()
}

#' Write a submission file. pred is assumed to be in the correct submission format.
write_submission_file <- function(pred, forecast_reference_date, submission_directory, file_name = "CMU-TimeSeries") {
  if (!file.exists(submission_directory)) {
    cli::cli_abort("Submission directory does not exist.", call = rlang::current_call())
  }
  file_path <- file.path(submission_directory, sprintf("%s-%s.csv", forecast_reference_date, file_name))
  if (file.exists(file_path)) {
    cli::cli_warn(c("Overwriting existing file in", file_path), call = rlang::current_call())
    file.remove(file_path)
  }
  readr::write_csv(pred, file_path)
}

#' Utility to get the reference date for a given date. This is the last day of
#' the epiweek that the date falls in.
get_forecast_reference_date <- function(date) {
  date <- as.Date(date)
  MMWRweek::MMWRweek2Date(lubridate::epiyear(date), lubridate::epiweek(date)) + 6
}

#' Update the site with the latest reports.
#'
#' Looks at that `reports/` directory and updates `template.md` with new reports
#' that follow a naming convention. This is translated into `report.md` which is
#' then converted to `index.html` with pandoc.
update_site <- function() {
  library(fs)
  library(stringr)
  # Define the directories
  reports_dir <- "reports"
  template_path <- "reports/template.md"

  # Create the reports directory if it doesn't exist
  if (!dir_exists(reports_dir)) {
    dir_create(reports_dir)
  }

  # Read the template file
  if (!file_exists(template_path)) {
    stop("Template file does not exist.")
  }

  report_md_content <- readLines(template_path)
  # Get the list of files in the reports directory
  report_files <- dir_ls(reports_dir, regexp = ".*_prod_on_.*.html")
  report_table <- tibble(
    filename = report_files,
    dates = str_match_all(filename, "[0-9]{4}-..-..")
  ) %>%
    unnest_wider(dates, names_sep = "_") %>%
    rename(forecast_date = dates_1, generation_date = dates_2) %>%
    mutate(
      forecast_date = ymd(forecast_date),
      generation_date = ymd(generation_date),
      disease = str_match(filename, "flu|covid")
    )

  # use the most recently generated forecast, and sort descending on the
  # forecast date
  used_reports <- report_table %>%
    group_by(forecast_date, disease) %>%
    slice_max(generation_date) %>%
    ungroup() %>%
    arrange(forecast_date)
  seasons <- tibble(
    season_name = c("2024-2025", "2025-2026"),
    season_start = as.Date(c("2024-11-20", "2025-06-04")),
    season_end = as.Date(c("2025-06-03", "2026-10-15"))
  )
  for (iSeason in 1:nrow(seasons)) {
    season_name <- seasons[[iSeason, "season_name"]]
    season_start <- seasons[[iSeason, "season_start"]]
    season_end <- seasons[[iSeason, "season_end"]]
    # Process each report file
    files_this_season <- used_reports %>%
      filter(season_start <= forecast_date, forecast_date < season_end) %>%
      pull(filename)
    for (report_file in files_this_season) {
      file_name <- path_file(report_file)
      file_parts <- str_match(file_name, "(\\d{4}-\\d{2}-\\d{2})_(.*)_prod_on_(\\d{4}-\\d{2}-\\d{2})\\.html")
      date <- file_parts[2]
      disease <- file_parts[3]
      generation_date <- file_parts[4]

      report_link <- sprintf(
        "- [Rendered %s, %s Forecasts on %s](%s)",
        generation_date,
        str_to_title(disease),
        date,
        file_name
      )

      # Insert into Production Reports section, skipping a line
      prod_reports_index <- which(grepl(glue("## Weekly Fanplots {season_name} Season"), report_md_content)) + 1
      report_md_content <- append(report_md_content, report_link, after = prod_reports_index)
      # insert into This week if it's actually from within the past week
      if (as.Date(generation_date) > Sys.Date() - 7) {
        prod_reports_index <- which(grepl(glue("## Most recent week"), report_md_content)) + 1
        report_md_content <- append(report_md_content, report_link, after = prod_reports_index)
      }
    }
  }

  # Handle score reports
  score_files <- dir_ls(reports_dir, regexp = ".*_scoring.*.html")
  score_table <- tibble(
    filename = score_files,
    dates = str_match_all(filename, "[0-9]{4}-..-..")
  ) %>%
    unnest_wider(dates, names_sep = "_") %>%
    rename(generation_date = dates_1) %>%
    mutate(
      generation_date = ymd(generation_date),
      disease = str_match(filename, "flu|covid")[1]
    ) %>%
    arrange(generation_date)
  for (score_file in score_table$filename) {
    file_name <- path_file(score_file)
    file_parts <- str_match(file_name, "(\\d{4}-\\d{2}-\\d{2})_(.*)\\.html")
    file_path <- file_parts[1]
    generation_date <- file_parts[2]
    report_type <- file_parts[3]

    report_link <- sprintf(
      "- [Rendered %s, %s](%s)",
      generation_date,
      report_type,
      file_path
    )

    # Insert into Production Reports section, skipping a line
    prod_reports_index <- which(grepl("## Score notebooks", report_md_content)) + 1
    report_md_content <- append(report_md_content, report_link, after = prod_reports_index)
    # insert into This week if it's actually from within the past week
    if (as.Date(generation_date) > Sys.Date() - 7) {
      prod_reports_index <- which(grepl(glue("## Most recent week"), report_md_content)) + 1
      report_md_content <- append(report_md_content, report_link, after = prod_reports_index)
    }
  }

  # Handle backtesting reports
  backtest_season_names <- c("2024-2025", "2025-2026")
  for (season_name in backtest_season_names) {
    season_pattern <- str_replace_all(season_name, "-", "_")
    backtest_files <- dir_ls(reports_dir, regexp = glue(".*_backtesting_{season_pattern}_on_.*\\.html"))
    if (length(backtest_files) == 0) next

    backtest_table <- tibble(filename = backtest_files) %>%
      mutate(
        file_name = path_file(filename),
        generation_date = ymd(str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}(?=\\.html)")),
        disease = str_extract(file_name, "^(flu|covid)"),
        target = str_match(file_name, glue("^(?:flu|covid)_(nhsn|nssp)_backtesting_{season_pattern}"))[, 2]
      ) %>%
      group_by(disease, target) %>%
      slice_max(generation_date) %>%
      ungroup() %>%
      arrange(disease, target)

    section_header <- glue("## {season_name} Season Backtesting")
    for (ii in seq_len(nrow(backtest_table))) {
      row <- backtest_table[ii, ]
      target_str <- if (!is.na(row$target)) toupper(row$target) else "All"
      report_link <- sprintf(
        "- [%s %s Backtesting (rendered %s)](%s)",
        str_to_title(row$disease),
        target_str,
        row$generation_date,
        row$file_name
      )
      insert_index <- which(grepl(section_header, report_md_content, fixed = TRUE)) + 1
      report_md_content <- append(report_md_content, report_link, after = insert_index)
    }
  }

  # Handle season-stamped explore notebooks ({disease}-[overall-]notebook-{YYYY-YYYY}.html)
  explore_overall_files <- dir_ls(reports_dir, regexp = ".*-overall-notebook-\\d{4}-\\d{4}\\.html")
  explore_family_files <- dir_ls(reports_dir, regexp = ".*-notebook-[^0-9].*-\\d{4}-\\d{4}\\.html")
  explore_files <- c(explore_overall_files, explore_family_files)
  if (length(explore_files) > 0) {
    explore_table <- tibble(filename = explore_files) %>%
      mutate(
        file_name = path_file(filename),
        disease = str_extract(file_name, "^(flu|covid)"),
        season = str_extract(file_name, "\\d{4}-\\d{4}"),
        is_overall = grepl("overall", file_name),
        family = if_else(
          is_overall, "Overall",
          str_remove(str_remove(file_name, glue("^{disease}-notebook-")), "-\\d{4}-\\d{4}\\.html$")
        )
      ) %>%
      arrange(disease, season, desc(is_overall), family)

    for (season_name in unique(explore_table$season)) {
      section_header <- glue("## {season_name} Season Backtesting")
      season_rows <- explore_table %>% filter(season == season_name)
      for (ii in seq_len(nrow(season_rows))) {
        row <- season_rows[ii, ]
        report_link <- sprintf(
          "- [%s %s](%s)",
          str_to_title(row$disease),
          row$family,
          row$file_name
        )
        insert_index <- which(grepl(section_header, report_md_content, fixed = TRUE)) + 1
        report_md_content <- append(report_md_content, report_link, after = insert_index)
      }
    }
  }

  # Write the updated content to report.md
  report_md_path <- path(reports_dir, "report.md")
  writeLines(report_md_content, report_md_path)

  # Convert the markdown file to HTML
  system(
    "pandoc reports/report.md -s -o reports/index.html --css=style.css --mathjax='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js' --metadata pagetitle='Delphi Reports'"
  )
}

#' Ensure that forecast values are monotically increasing
#' in quantile order.
sort_by_quantile <- function(forecasts) {
  forecasts %>%
    arrange(geo_value, target_end_date, forecast_date, quantile) %>%
    group_by(geo_value, forecast_date, target_end_date) %>%
    mutate(value = sort(value)) %>%
    ungroup()
}

#' Print recent targets errors.
get_targets_errors <- function(project = tar_path_store(), top_n = 10) {
  meta_df <- targets::tar_meta(store = project)
  forecast_errors <- meta_df %>%
    filter(!is.na(parent), !is.na(error)) %>%
    distinct(parent, error, .keep_all = TRUE) %>%
    mutate(parent = gsub("forecast_", "", parent)) %>%
    slice_max(time, n = top_n)

  # Print each error message, along with the parent target.
  if (nrow(forecast_errors) > 0) {
    cat("Forecast errors:\n")
    for (i in 1:nrow(forecast_errors)) {
      cli::cli_inform(c(
        "Parent target: {forecast_errors$parent[i]}",
        "Time: {forecast_errors$time[i]}",
        "Error: {forecast_errors$error[i]}"
      ))
    }
  }

  other_errors <- meta_df %>%
    filter(!is.na(error)) %>%
    distinct(error, .keep_all = TRUE) %>%
    slice_max(time, n = top_n)

  # Print each error message, along with the parent target.
  if (nrow(other_errors) > 0) {
    cat("Other errors:\n")
    for (i in 1:nrow(other_errors)) {
      cli::cli_inform(c(
        "Target: {other_errors$name[i]}",
        "Time: {other_errors$time[i]}",
        "Error: {other_errors$error[i]}"
      ))
    }
  }

  return(invisible(meta_df %>% slice_max(time, n = top_n)))
}

#' Retry a function.
#'
#' @param max_attempts The maximum number of attempts.
#' @param wait_seconds The number of seconds to wait between attempts.
#' @param fn The function to retry.
#' @param ... Additional arguments to pass to the function.
#'
#' @examples
#' retry_fn(
#'   max_attempts = 10,
#'   wait_seconds = 1,
#'   fn = pub_covidcast,
#'   source = "nssp",
#'   signals = "pct_ed_visits_covid",
#'   geo_type = "state",
#'   geo_values = "*",
#'   time_type = "week"
#' )
retry_fn <- function(max_attempts = 10, wait_seconds = 1, fn, ...) {
  for (attempt in 1:max_attempts) {
    tryCatch(
      {
        result <- fn(...)
        return(result) # Return successful result
      },
      error = function(e) {
        if (attempt == max_attempts) {
          stop("Maximum retry attempts reached. Last error: ", e$message)
        }
        message(sprintf("Attempt %d failed. Retrying in %d second(s)...", attempt, wait_seconds))
        Sys.sleep(wait_seconds)
      }
    )
  }
}

validate_epi_data <- function(epi_data) {
  if (!inherits(epi_data, "epi_df")) {
    epi_data <- epi_data %>% as_epi_df(as_of = max(epi_data$time_value))
  }
  if (is.null(attributes(epi_data)$metadata$as_of)) {
    attributes(epi_data)$metadata$as_of <- max(epi_data$time_value)
  }
  return(epi_data)
}

MIN_TIMESTAMP <- as.POSIXct("2000-01-01 00:00:00S", tz = "UTC")

get_local_file_last_modified <- function(file_path, missing_value = MIN_TIMESTAMP) {
  if (!file.exists(file_path)) {
    return(missing_value)
  }
  file.info(file_path)$mtime %>% as.POSIXct(tz = "UTC")
}

#' Get the last modified date of an S3 object
#'
#' @param bucket The name of the S3 bucket.
#' @param key The key of the S3 object.
#'
#' @return The last modified date of the S3 object in POSIXct format.
get_s3_object_last_modified <- function(key, bucket, missing_value = MIN_TIMESTAMP) {
  metadata <- suppressMessages(head_object(key, bucket = bucket))
  if (!metadata) {
    return(missing_value)
  }
  # Format looks like "Fri, 31 Jan 2025 22:01:16 GMT"
  attr(metadata, "last-modified") %>%
    str_replace_all(" GMT", "") %>%
    as.POSIXct(format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
}

#' Get the last updated date of a Socrata dataset
#'
#' FYI: This hits a cache layer, which is only updated ~every 4 hours.
#'
#' @param dataset_url The URL of the Socrata dataset.
#'
#' @return The last updated date of the Socrata dataset in POSIXct format.
get_socrata_updated_at <- function(dataset_url, missing_value) {
  tryCatch(
    {
      rowsUpdatedAt <- httr::with_config(
        httr::config(timeout = 5),
        httr::RETRY("GET", dataset_url, times = 5, pause_min = 5, pause_cap = 5)
      ) %>%
        httr::content() %>%
        # This field comes in as integer seconds since epoch, so we need to convert it.
        pluck("rowsUpdatedAt")
      if (is.null(rowsUpdatedAt)) {
        return(missing_value)
      }
      rowsUpdatedAt %>% as.POSIXct(origin = "1970-01-01", tz = "UTC")
    },
    error = function(cond) {
      return(missing_value)
    }
  )
}

#' create a list of valid locations x forecast_dates shared among forecasters
#' which have at least `min_locations` and `min_dates`, and create a list of
#' these for each forecaster
get_unique <- function(forecasts, min_locations = 50, min_dates = 40) {
  forecasters <- forecasts %>%
    pull(forecaster) %>%
    unique()
  distinct <- map(
    forecasters,
    \(x) {
      forecasts %>%
        filter(forecaster == x) %>%
        distinct(geo_value, forecast_date, target_end_date)
    }
  )
  # decide which of the forecasters has enough locations
  to_keep <- distinct %>%
    map_lgl(\(x) {
      (nrow(distinct(x, geo_value)) >= min_locations) &
        (nrow(distinct(x, forecast_date)) >= min_dates)
    })
  if (all(!to_keep)) {
    max_geos <- distinct %>%
      map_int(\(x) {
        nrow(distinct(x, geo_value))
      }) %>%
      max()
    max_dates <- distinct %>%
      map_int(\(x) {
        nrow(distinct(x, forecast_date))
      }) %>%
      max()
    cli::cli_abort(
      "there are at most {max_geos} locations and {max_dates} dates. Adjust `min_locations` and/or `min_dates`."
    )
  }
  forecasters <- forecasters[to_keep]
  distinct <- distinct[to_keep]
  distinct_dates <- reduce(
    distinct,
    \(x, y) x %>% inner_join(y, by = c("geo_value", "forecast_date", "target_end_date"))
  )
  distinct_dates %>%
    mutate(
      forecast_date = round_date(forecast_date, unit = "week", week_start = 6)
    ) %>%
    cross_join(
      tibble(forecaster = forecasters),
      .
    )
}

#' filter the external and local forecasts to just the shared dates/geos
#' some forecasters have a limited set of geos; we want to include those
#' anyways, they are `tructated_forecasters`, while the external_forecasts may
#' have previous years forecasts that we definitely want to exclude via
#' `season_start`.
filter_shared_geo_dates <- function(
  local_forecasts,
  external_forecasts,
  season_start = "2024-11-01",
  trucated_forecasters = "windowed_seasonal_extra_sources",
  min_locations = 52,
  min_dates = 12
) {
  # the length is one if we're forecasting this week, in which case we only want the last 12 weeks of forecasts
  if (local_forecasts %>% distinct(forecast_date) %>% length() == 1) {
    viable_dates <-
      external_forecasts %>%
      get_unique(min_locations = min_locations, min_dates = min_dates)
  } else {
    viable_dates <- inner_join(
      local_forecasts %>%
        filter(forecaster %nin% trucated_forecasters) %>%
        get_unique(),
      external_forecasts %>%
        filter(forecast_date > season_start) %>%
        get_unique(),
      by = c("geo_value", "forecast_date", "target_end_date")
    )
  }
  dplyr::bind_rows(
    local_forecasts %>%
      mutate(
        forecast_date = round_date(forecast_date, unit = "week", week_start = 6)
      ) %>%
      inner_join(viable_dates, by = c("forecaster", "geo_value", "forecast_date", "target_end_date")),
    external_forecasts %>%
      inner_join(viable_dates, by = c("forecaster", "geo_value", "forecast_date", "target_end_date"))
  )
}


#' Calculate MD5 hash of a file
#'
#' This function reads a file into memory, calculates an MD5 hash of the
#' binary data, and returns the hash as a character string.
#'
#' @param file The path to the file to hash
#' @param algorithm The hash algorithm to use. Defaults to "md5".
get_file_hash <- function(file, algorithm = "md5") {
  readBin(file, what = "raw", n = file.size(file)) %>%
    digest::digest(algo = algorithm, serialize = FALSE)
}

build_cast_api_query <- function(
  source = c("nssp", "nhsn"),
  signal = NULL,
  geo_type = c("state", "nation"),
  columns = NULL,
  fill_method = NULL,
  limit = NULL,
  offset = NULL,
  report_time_query = NULL,
  geo_value = NULL,
  time_value = NULL
) {
  source <- rlang::arg_match(source)
  if (!is.null(fill_method)) fill_method <- rlang::arg_match(fill_method, c("source", "fill_ave", "fill_zero"))
  geo_type <- rlang::arg_match(geo_type)
  columns <- columns %||% c("geo_value", "time_value", "value", "version")
  columns <- gsub("\\btime_value\\b", "reference_time", columns)
  columns <- gsub("\\bversion\\b", "report_time", columns)
  columns <- paste(columns, collapse = ",")

  httr2::request(EPIDATA_V5_URL) %>%
    httr2::req_url_path_append("archive/") %>%
    httr2::req_url_query(
      source = source,
      signal = signal,
      geo_type = geo_type,
      report_time_query = report_time_query,
      columns = columns,
      limit = limit,
      offset = offset,
      fill_method = fill_method,
      geo_value = geo_value,
      reference_time = time_value,
      format = "csv",
      header = "true",
      .multi = "explode"
    ) %>%
    {
      key <- Sys.getenv("DELPHI_EPIDATA_KEY")
      if (nchar(key) > 0) httr2::req_headers_redacted(., token = key) else .
    }
}

get_cast_api_data <- function(...) {
  req <- build_cast_api_query(...)
  if (Sys.getenv("DEBUG_MODE") == "true") print(req)
  filename <- tempfile(fileext = ".csv")
  req %>% httr2::req_perform(path = filename)
  readr::read_csv(filename, show_col_types = FALSE) %>%
    dplyr::rename(any_of(c(time_value = "reference_time", version = "report_time")))
}

#' Check whether NHSN and NSSP source data is fresh enough to forecast on
#'
#' Builds (or confirms up to date) the `nhsn_archive_data` and
#' `nssp_archive_data` targets for the active `targets` project (selected via
#' `TAR_PROJECT`/`TAR_CONFIG`, see `tar_config_get()`), then checks the latest
#' `time_value` actually present in each archive against the current date.
#' Used by the production forecast cron/systemd job to decide whether to
#' proceed with a forecast run or wait for newer data, rather than submitting
#' a forecast built on stale inputs. Because these targets are `tar_change()`
#' targets keyed on the upstream API's latest-update date, this also takes
#' care of re-fetching them if newer data has appeared upstream.
#'
#' @param max_age_days Maximum allowed age, in days, of the latest time_value
#'   before the data is considered stale.
#' @return TRUE if both NHSN and NSSP archives have a time_value within
#'   `max_age_days`, FALSE otherwise.
check_data_freshness <- function(max_age_days = 7) {
  targets::tar_make(names = targets::any_of(c("nhsn_archive_data", "nssp_archive_data")))
  nhsn_archive <- targets::tar_read(nhsn_archive_data)
  nssp_archive <- targets::tar_read(nssp_archive_data)
  nhsn_latest <- max(nhsn_archive$DT$time_value)
  nssp_latest <- max(nssp_archive$DT$time_value)
  nhsn_age <- as.numeric(Sys.Date() - nhsn_latest)
  nssp_age <- as.numeric(Sys.Date() - nssp_latest)
  cli::cli_inform("NHSN latest time_value: {nhsn_latest} ({nhsn_age} days old)")
  cli::cli_inform("NSSP latest time_value: {nssp_latest} ({nssp_age} days old)")
  nhsn_age <= max_age_days && nssp_age <= max_age_days
}
