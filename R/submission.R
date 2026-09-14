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
  insert_after_section <- function(content, header, link) {
    idx <- which(grepl(header, content, fixed = TRUE))
    if (length(idx) == 0) stop(glue("Template is missing section '{header}' — add it to reports/template.md"))
    append(content, link, after = idx[[1]] + 1L)
  }
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
  max_prod_gen_date <- max(used_reports$generation_date, na.rm = TRUE)
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

      report_md_content <- insert_after_section(report_md_content, glue("## Weekly Fanplots {season_name} Season"), report_link)
      if (as.Date(generation_date) == max_prod_gen_date) {
        report_md_content <- insert_after_section(report_md_content, "## Most recent week", report_link)
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
  max_score_gen_date <- max(score_table$generation_date, na.rm = TRUE)
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

    report_md_content <- insert_after_section(report_md_content, "## Score notebooks", report_link)
    if (as.Date(generation_date) == max_score_gen_date) {
      report_md_content <- insert_after_section(report_md_content, "## Most recent week", report_link)
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
      report_md_content <- insert_after_section(report_md_content, section_header, report_link)
    }
  }

  # Handle season-stamped explore notebooks ({disease}-[overall-]notebook-{YYYY_YYYY}.html)
  explore_overall_files <- dir_ls(reports_dir, regexp = ".*-overall-notebook-\\d{4}_\\d{4}\\.html")
  explore_family_files <- dir_ls(reports_dir, regexp = ".*-notebook-[^0-9].*-\\d{4}_\\d{4}\\.html")
  explore_files <- c(explore_overall_files, explore_family_files)
  if (length(explore_files) > 0) {
    explore_table <- tibble(filename = explore_files) %>%
      mutate(
        file_name = path_file(filename),
        disease = str_extract(file_name, "^(flu|covid)"),
        season_slug = str_extract(file_name, "\\d{4}_\\d{4}"),
        season_name = str_replace(season_slug, "_", "-"),
        is_overall = grepl("overall", file_name),
        family = if_else(
          is_overall, "Overall",
          str_remove(str_remove(file_name, glue("^{disease}-notebook-")), "-\\d{4}_\\d{4}\\.html$")
        )
      ) %>%
      arrange(disease, season_name, desc(is_overall), family)

    section_header <- "## Explore Notebooks"
    all_explore_lines <- character(0)
    for (season_name in sort(unique(explore_table$season_name), decreasing = TRUE)) {
      season_rows <- explore_table %>%
        filter(season_name == .env$season_name) %>%
        arrange(disease, desc(is_overall), family)
      all_explore_lines <- c(all_explore_lines, "", glue("### {season_name}"), "")
      for (ii in seq_len(nrow(season_rows))) {
        row <- season_rows[ii, ]
        all_explore_lines <- c(
          all_explore_lines,
          sprintf("- [%s %s](%s)", str_to_title(row$disease), row$family, row$file_name)
        )
      }
    }
    if (length(all_explore_lines) > 0) {
      report_md_content <- insert_after_section(report_md_content, section_header, all_explore_lines)
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
