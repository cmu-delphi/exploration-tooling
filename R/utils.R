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

data_substitutions <- function(dataset, disease, forecast_generation_date) {
  disease <- "flu"
  forecast_generation_date <- as.Date("2025-01-08")
  substitutions <- readr::read_csv(
    glue::glue("{disease}_data_substitutions.csv"),
    comment = "#",
    show_col_types = FALSE
  ) %>%
    filter(forecast_date == forecast_generation_date) %>%
    select(-forecast_date) %>%
    rename(new_value = value)
  dataset %>%
    left_join(substitutions) %>%
    mutate(value = ifelse(!is.na(new_value), new_value, value)) %>%
    select(-new_value)
}

parse_prod_weights <- function(filename = here::here("covid_geo_exclusions.csv"),
                               gen_forecast_date) {
  all_states <- c(
    unique(readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv", show_col_types = FALSE)$state_id),
    "usa", "us"
  )
  all_prod_weights <- readr::read_csv(filename, comment = "#", show_col_types = FALSE)
  # if we haven't set specific weights, use the overall defaults
  useful_prod_weights <- filter(all_prod_weights, forecast_date == gen_forecast_date)
  if (nrow(useful_prod_weights) == 0) {
    useful_prod_weights <- all_prod_weights %>%
      filter(forecast_date == min(forecast_date)) %>%
      mutate(forecast_date = gen_forecast_date)
  }
  useful_prod_weights %>%
    mutate(
      geo_value = ifelse(geo_value == "all", list(all_states), geo_value),
    ) %>%
    unnest_longer(geo_value) %>%
    mutate(
      forecaster = ifelse(forecaster == "all", list(names(forecaster_fns)), forecaster),
    ) %>%
    unnest_longer(forecaster) %>%
    group_by(forecast_date, forecaster, geo_value) %>%
    summarize(weight = min(weight), .groups = "drop") %>%
    mutate(forecast_date = as.Date(forecast_date)) %>%
    group_by(forecast_date, geo_value) %>%
    mutate(weight = ifelse(near(weight, 0), 0, weight / sum(weight)))
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
  readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/refs/heads/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv", show_col_types = FALSE) %>%
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
  MMWRweek::MMWRweek2Date(epiyear(date), epiweek(date)) + 6
}

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

  # Sync the reports directory with the S3 bucket
  aws.s3::s3sync(path = reports_dir, bucket = "forecasting-team-data", prefix = "reports-2024/", verbose = FALSE)

  # Read the template file
  if (!file_exists(template_path)) {
    stop("Template file does not exist.")
  }
  report_md_content <- readLines(template_path)
  # Get the list of files in the reports directory
  report_files <- dir_ls(reports_dir, regexp = ".*_prod_on_.*.html")
  report_table <- tibble(filename = report_files,
         dates = str_match_all(filename, "[0-9]{4}-..-..")) %>%
    unnest_wider(dates, names_sep = "_") %>%
    rename(forecast_date = dates_1, generation_date = dates_2) %>%
    mutate(
      forecast_date = ymd(forecast_date),
      generation_date = ymd(generation_date)
    )

  # use the most recently generated forecast, and sort descending on the
  # forecast date
  used_reports <- report_table %>%
    group_by(forecast_date) %>%
    arrange(generation_date) %>%
    filter(generation_date == max(generation_date)) %>%
    ungroup() %>%
    arrange(forecast_date)

  # Process each report file
  for (report_file in used_reports$filename) {
    file_name <- path_file(report_file)
    file_parts <- str_split(fs::path_ext_remove(file_name), "_", simplify = TRUE)
    date <- file_parts[1]
    disease <- file_parts[2]

    report_link <- sprintf("- [%s Forecasts %s](%s)", str_to_title(disease), date, file_name)

    # Insert into Production Reports section, skipping a line
    prod_reports_index <- which(grepl("## Production Reports", report_md_content)) + 1
    report_md_content <- append(report_md_content, report_link, after = prod_reports_index)
  }

  # Write the updated content to report.md
  report_md_path <- path(reports_dir, "report.md")
  writeLines(report_md_content, report_md_path)

  # Convert the markdown file to HTML
  system("pandoc reports/report.md -s -o reports/index.html --css=reports/style.css --mathjax='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js' --metadata pagetitle='Delphi Reports'")
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
