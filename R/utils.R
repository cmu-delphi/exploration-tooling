#' Look up forecasters by name
#'
#' Given a (partial) forecaster name, look up all forecasters in the given
#' project which contain part of that name.
#'
#' @param forecaster_grid the forecaster grid to search.
#' @param pattern string to search in the forecaster name.
#'
#' @export
forecaster_lookup <- function(pattern, forecaster_grid = NULL, printing = TRUE) {
  if (is.null(forecaster_grid)) {
    cli::cli_warn("Reading `forecaster_param_combinations` target. If it's not up to date, results will be off. Update with `tar_make(forecaster_parameter_combinations)`.")
    forecaster_grid <- tar_read_raw("forecaster_parameter_combinations") %>%
      map(make_forecaster_grid) %>%
      bind_rows()
  }
  fc_row <- forecaster_grid %>% filter(grepl(pattern, id))
  if (printing) {
    params <- fc_row$params[[1]]
    if (!is.null(params$trainer)) {
      params$trainer <- as_string(params$trainer)
    }
    print(glue::glue("name: {fc_row %>% pull(id)}"))
    print(glue::glue("forecaster: {fc_row$forecaster[[1]]}"))
    print(glue::glue("params:"))
    print(params %>% data.table::as.data.table())
  }
  return(fc_row)
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

#' Make a forecaster grid.
#'
#' Convert a tibble of forecasters and their parameters to a specific format
#' that we can iterate over in targets. Currently only `forecaster` and
#' `trainer` can be symbols.
#'
#' @param tib the tibble of parameters. Must have the forecaster and trainer
#' columns, everything else is optional.
#'
#' @export
make_forecaster_grid <- function(tib) {
  if ("trainer" %in% colnames(tib)) {
    tib$trainer <- rlang::syms(tib$trainer)
  }
  # turns a tibble into a list of named lists
  params_list <-
    tib %>%
    select(-forecaster, -id) %>%
    split(seq_len(nrow(.))) %>%
    unname() %>%
    lapply(as.list)
  # for whatever reason, trainer ends up being a list of lists, which we do not want
  params_list %<>% lapply(function(x) {
    x$trainer <- x$trainer[[1]]
    x$lags <- x$lags[[1]]
    x
  })

  if (length(params_list) == 0) {
    out <- tibble(
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = list(list()),
      param_names = list(list())
    )
  } else {
    out <- tibble(
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = params_list,
      param_names = map(params_list, names)
    )
  }

  return(out)
}

#' Make an ensemble grid.
#'
#' Same as `make_forecaster_grid`, but for ensembles.
#'
#' @param tib the tibble of parameters.
#'
#' @export
make_ensemble_grid <- function(tib) {
  sym_subset <- function(param_list) {
    imap(param_list, \(x, y) if (y %in% list("average_type")) rlang::sym(x) else x)
  }

  tibble(
    id = tib$id,
    children_ids = tib$children_ids %>%
      map(function(x) paste0("forecast_", x)) %>%
      map(rlang::syms),
    ensemble = rlang::syms(tib$ensemble),
    ensemble_args = map(tib$ensemble_args, sym_subset),
    ensemble_args_names = map(tib$ensemble_args, ~ names(.x))
  )
}

#' Get exclusions from a JSON file for a given date
#'
#' @param date A date
#' @param exclusions_json A JSON file with exclusions in the format:
#' @param forecaster the forecaster whose exclusions to look up; global means to be excluded from the submitted forecast, and otherwise corresponds to the name of the forecaster in forecaster_fns
#'
#'    {"exclusions": {"2024-03-24": "ak,hi"}}
#'
#' @export
get_exclusions <- function(
    date,
    forecaster,
    exclusions_json = here::here("scripts", "geo_exclusions.json")) {
  if (!file.exists(exclusions_json)) {
    return("")
  }

  res <- jsonlite::read_json(exclusions_json)$exclusions[[as.character(date)]]
  if (!is.null(res[[forecaster]])) {
    return(strsplit(res[[forecaster]], ",")[[1]])
  }
  return("")
}

`%nin%` <- function(x, y) !(x %in% y)

get_population_data <- function() {
  readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv", show_col_types = FALSE) %>%
    rename(population = pop)
}

# TODO:
scale_pop <- function(df) {}

unscale_pop <- function(df) {}

filter_forecast_geos <- function(forecasts, truth_data) {
  subset_geos <- unique(forecasts$geo_value)
  # Bad forecast filters
  c(
    # 1. Filter out forecasts that trend down
    tibble(
      geo_value = subset_geos,
      trend_down = map(subset_geos, ~ lm(value ~ target_end_date, data = forecasts %>% filter(geo_value == .x))$coefficients[2] < 0) %>% unlist()
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
        forecasts %>% filter(near(quantile, 0.75), target_end_date == MMWRweek2Date(epiyear(forecast_date), epiweek(forecast_date)) + 6),
        by = "geo_value"
      ) %>%
      left_join(
        truth_data %>% group_by(geo_value) %>% summarize(pp = max(value, na.rm = TRUE)),
        by = "geo_value"
      ) %>%
      filter(value >= pp) %>%
      pull(geo_value)
  ) %>% unique()
}


#' Write a submission file. pred is assumed to be in the correct submission format.
write_submission_file <- function(pred, forecast_reference_date, submission_directory) {
  if (!file.exists(submission_directory)) {
    cli::cli_abort("Submission directory does not exist.", call = rlang::current_call())
  }
  file_path <- file.path(submission_directory, sprintf("%s-CMU-TimeSeries.csv", forecast_reference_date))
  if (file.exists(file_path)) {
    cli::cli_warn(c("Overwriting existing file in", file_path), call = rlang::current_call())
    file.remove(file_path)
  }
  readr::write_csv(pred, file_path)
}

#' Utility to get the reference date for a given date. This is the last day of
#' the epiweek that the date falls in.
get_forecast_reference_date <- function(date) {
  MMWRweek::MMWRweek2Date(epiyear(date), epiweek(date)) + 6
}

copy_report <- function(report_date = NULL) {
  library(fs)
  # Define the directories
  reports_dir <- "reports"
  site_dir <- "_site"

  # Create the _site directory if it doesn't exist
  if (!dir_exists(site_dir)) {
    dir_create(site_dir)
  }

  if (is.null(report_date)) {
    # Get the list of files in the reports directory
    report_files <- dir_ls(reports_dir, regexp = "covid_forecast_report_.*\\.html")

    # Find the most recent report file
    report_file <- report_files[which.max(file_info(report_files)$modification_time)]
  } else {
    # Define the report file path
    report_file <- path(reports_dir, sprintf("covid_forecast_report_%s.html", report_date))

    # Check if the report file exists
    if (!file_exists(report_file)) {
      stop("Report file does not exist.")
    }
  }

  # Define the destination path
  destination_path <- path(site_dir, "index.html")

  # Copy the most recent report to the _site directory as index.html
  file_copy(report_file, destination_path, overwrite = TRUE)
}
