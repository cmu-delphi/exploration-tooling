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
    out %>% glimpse()
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
make_forecaster_grid <- function(tib, family) {
  if ("trainer" %in% colnames(tib)) {
    tib$trainer <- rlang::syms(tib$trainer)
  }
  # turns a tibble into a list of named lists
  params_list <- tib %>%
    select(-forecaster, -id) %>%
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
      params = list(list()),
      param_names = list(list())
    )
  } else {
    out <- tibble(
      family = family,
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
  exclusions_json = here::here("scripts", "geo_exclusions.json")
) {
  if (!file.exists(exclusions_json)) {
    return("")
  }

  res <- jsonlite::read_json(exclusions_json)$exclusions[[as.character(date)]]
  if (!is.null(res[[forecaster]])) {
    return(strsplit(res[[forecaster]], ",")[[1]])
  }
  return("")
}

data_substitutions <- function(dataset, substitutions_path, forecast_generation_date) {
  # Get the substitutions from the table, matched by forecast generation date
  substitutions <- readr::read_csv(
    substitutions_path,
    comment = "#",
    show_col_types = FALSE
  ) %>%
    filter(forecast_date == forecast_generation_date) %>%
    select(-forecast_date) %>%
    rename(new_value = value)
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
  # if we haven't set specific weights, use the overall defaults
  useful_prod_weights <- filter(all_prod_weights, forecast_date == forecast_date_val)
  if (nrow(useful_prod_weights) == 0) {
    useful_prod_weights <- all_prod_weights %>%
      filter(forecast_date == min(forecast_date)) %>%
      mutate(forecast_date = forecast_date_val)
  }
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
#'
#' @param sync_to_s3 Whether to sync the reports to the S3 bucket.
update_site <- function(sync_to_s3 = TRUE) {
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
  if (sync_to_s3) {
    aws.s3::s3sync(path = reports_dir, bucket = "forecasting-team-data", prefix = "reports-2024/", verbose = FALSE)
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

  # Process each report file
  for (report_file in used_reports$filename) {
    file_name <- path_file(report_file)
    file_parts <- str_split(fs::path_ext_remove(file_name), "_", simplify = TRUE)
    date <- file_parts[1]
    disease <- file_parts[2]
    generation_date <- file_parts[5]

    report_link <- sprintf(
      "- [%s Forecasts %s, Rendered %s](%s)",
      str_to_title(disease),
      date,
      generation_date,
      file_name
    )

    # Insert into Production Reports section, skipping a line
    prod_reports_index <- which(grepl("## Weekly Fanplots 2024-2025 Season", report_md_content)) + 1
    report_md_content <- append(report_md_content, report_link, after = prod_reports_index)
  }

  # Write the updated content to report.md
  report_md_path <- path(reports_dir, "report.md")
  writeLines(report_md_content, report_md_path)

  # Convert the markdown file to HTML
  system(
    "pandoc reports/report.md -s -o reports/index.html --css=reports/style.css --mathjax='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js' --metadata pagetitle='Delphi Reports'"
  )
}

#' Delete unused reports from the S3 bucket.
#'
#' @param dry_run List files that would be deleted if `dry_run` is `FALSE`.
delete_extra_s3_files <- function(dry_run = TRUE) {
  local_path <- "reports"
  bucket <- "forecasting-team-data"
  prefix <- "reports-2024/"
  # Get list of local files (relative paths)
  local_files <- list.files(local_path, recursive = TRUE)

  # Get list of S3 files
  s3_objects <- aws.s3::get_bucket(bucket, prefix = prefix)
  s3_files <- sapply(s3_objects, function(x) x$Key)

  # Find files that exist in S3 but not locally
  # Remove prefix from s3_files for comparison
  s3_files_clean <- gsub(prefix, "", s3_files)
  files_to_delete <- s3_files[!(s3_files_clean %in% local_files)]

  if (dry_run) {
    message("Would delete ", length(files_to_delete), " files from S3")
    message("Files: ", paste(files_to_delete, collapse = ", "))
    return(invisible(files_to_delete))
  }

  # Delete each extra file
  if (length(files_to_delete) > 0) {
    message("Deleting ", length(files_to_delete), " files from S3")
    for (file in files_to_delete) {
      message("Deleting: ", file)
      aws.s3::delete_object(file, bucket)
    }
  } else {
    message("No files to delete")
  }
}

#' Find unused report files in index.html.
find_unused_report_files <- function() {
  library(rvest)
  library(fs)
  library(stringr)

  # Read all files in reports directory
  all_files <- dir_ls("reports", recurse = TRUE) %>%
    path_file() # just get filenames, not full paths

  # Read index.html and extract all href links
  index_html <- read_html("reports/index.html")
  used_files <- index_html %>%
    html_elements("a") %>%
    html_attr("href") %>%
    # Add known required files like CSS
    c("style.css", "template.md", "report.md", "index.html", .) %>%
    # Remove links like "https://" from the list
    keep(~ !grepl("^https?://", .))

  # Find files that exist but aren't referenced
  unused_files <- setdiff(all_files, used_files)

  if (length(unused_files) > 0) {
    cat("The following files in 'reports' are not referenced in index.html:\n")
    cat(paste("-", unused_files), sep = "\n")
  } else {
    cat("All files in 'reports' are referenced in index.html\n")
  }

  return(invisible(unused_files))
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

#' Convenience wrapper for working with Delphi S3 bucket.
get_bucket_df_delphi <- function(prefix = "", bucket = "forecasting-team-data", ...) {
  aws.s3::get_bucket_df(prefix = prefix, bucket = bucket, ...) %>% tibble()
}

#' Remove duplicate files from S3
#'
#' Removes duplicate files from S3 by keeping only the earliest LastModified
#' file for each ETag. You can modify the logic of keep_df, if this doesn't suit
#' your needs.
#'
#' @param bucket The name of the S3 bucket.
#' @param prefix The prefix of the files to remove duplicates from.
#' @param dry_run Whether to actually delete the files.
#' @param .progress Whether to show a progress bar.
delete_duplicates_from_s3_by_etag <- function(bucket, prefix, dry_run = TRUE, .progress = TRUE) {
  # Get a list of all new dataset snapshots from S3
  files_df <- aws.s3::get_bucket_df(bucket = bucket, prefix = prefix) %>% as_tibble()

  # Create a list of all the files to keep by keeping the earliest timestamp file for each ETag
  keep_df <- files_df %>%
    group_by(ETag) %>%
    slice_min(LastModified) %>%
    ungroup()

  # Create a list of all the files to delete by taking the complement of keep_df
  delete_df <- files_df %>%
    anti_join(keep_df, by = "Key")

  if (nrow(delete_df) == 0) {
    return(invisible(delete_df))
  }

  if (dry_run) {
    cli::cli_alert_info("Would delete {nrow(delete_df)} files from {bucket} with prefix {prefix}")
    print(delete_df)
    return(invisible(delete_df))
  }

  # Delete
  delete_files_from_s3(bucket = bucket, keys = delete_df$Key, .progress = .progress)

  return(invisible(delete_df))
}

#' Delete files from S3
#'
#' Faster than aws.s3::delete_object, when there are many files to delete (thousands).
#'
#' @param bucket The name of the S3 bucket.
#' @param keys The keys of the files to delete, as a character vector.
#' @param batch_size The number of files to delete in each batch.
#' @param .progress Whether to show a progress bar.
delete_files_from_s3 <- function(keys, bucket, batch_size = 500, .progress = TRUE) {
  split(keys, ceiling(seq_along(keys) / batch_size)) %>%
    purrr::walk(~ aws.s3::delete_object(bucket = bucket, object = .x), .progress = .progress)
}


MIN_TIMESTAMP <- as.POSIXct("2000-01-01 00:00:00S", tz = "UTC")
MAX_TIMESTAMP <- as.POSIXct("2040-01-01 00:00:00S", tz = "UTC")

#' Get the last time a covidcast signal was updated.
#'
#' @param source The source of the signal.
#' @param signal The signal of the signal.
#' @param geo_type The geo type of the signal.
#' @param missing_value The value to return if the signal is not found.
#'
#' @return The last time the signal was updated in POSIXct format.
get_covidcast_signal_last_update <- function(source, signal, geo_type, missing_value = MAX_TIMESTAMP) {
  tryCatch(
    {
      pub_covidcast_meta() %>%
        filter(source == !!source, signal == !!signal, geo_type == !!geo_type) %>%
        pull(last_update) %>%
        as.POSIXct()
    },
    error = function(cond) {
      return(missing_value)
    }
  )
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
get_socrata_updated_at <- function(dataset_url, missing_value = MAX_TIMESTAMP) {
  tryCatch(
    {
      httr::with_config(httr::config(timeout = 5), httr::RETRY("GET", dataset_url, times = 5, pause_min = 5, pause_cap = 5)) %>%
        httr::content() %>%
        # This field comes in as integer seconds since epoch, so we need to convert it.
        pluck("rowsUpdatedAt") %>%
        as.POSIXct(origin = "1970-01-01", tz = "UTC")
    },
    error = function(cond) {
      return(missing_value)
    }
  )
}



#' get the unique shared (geo_value, forecast_date, target_end_date) tuples present for each forecaster in `forecasts`
get_unique <- function(forecasts) {
  forecasters <- forecasts %>%
    pull(forecaster) %>%
    unique()
  distinct <- map(
    forecasters,
    \(x) forecasts %>%
      filter(forecaster == x) %>%
      select(geo_value, forecast_date, target_end_date) %>%
      distinct()
  )
  distinct_dates <- reduce(distinct, \(x, y) x %>% inner_join(y, by = c("geo_value", "forecast_date", "target_end_date")))
  mutate(
    distinct_dates,
    forecast_date = round_date(forecast_date, unit = "week", week_start = 6)
  )
}

#' filter the external and local forecasts to just the shared dates/geos
#' some forecasters have a limited set of geos; we want to include those
#' anyways, they are `tructated_forecasters`, while the external_forecasts may
#' have previous years forecasts that we definitely want to exclude via
#' `season_start`.
filter_shared_geo_dates <- function(local_forecasts, external_forecasts, season_start = "2024-11-01", trucated_forecasters = "windowed_seasonal_extra_sources") {
  viable_dates <- inner_join(
    local_forecasts %>%
      filter(forecaster %nin% trucated_forecasters) %>%
      get_unique(),
    external_forecasts %>%
      filter(forecast_date > season_start) %>%
      get_unique(),
    by = c("geo_value", "forecast_date", "target_end_date")
  )
  dplyr::bind_rows(
    local_forecasts %>%
      mutate(
        forecast_date = round_date(forecast_date, unit = "week", week_start = 6)
      ) %>%
      inner_join(viable_dates, by = c("geo_value", "forecast_date", "target_end_date")),
    external_forecasts %>%
      inner_join(viable_dates, by = c("geo_value", "forecast_date", "target_end_date"))
  )
}
