#' Write a submission file. pred is assumed to be in the correct submission format.
write_submission_file <- function(pred, forecast_reference_date, submission_directory, file_name = "CMU-TimeSeries") {
  # The hub's model-output directory must exist; a model's first submission
  # creates its subdirectory, since git does not keep empty directories.
  if (!dir.exists(dirname(submission_directory))) {
    cli::cli_abort("Submission directory {.path {dirname(submission_directory)}} does not exist.", call = rlang::current_call())
  }
  dir.create(submission_directory, showWarnings = FALSE)
  file_path <- file.path(submission_directory, sprintf("%s-%s.csv", forecast_reference_date, file_name))
  if (file.exists(file_path)) {
    cli::cli_warn(c("Overwriting existing file in", file_path), call = rlang::current_call())
    file.remove(file_path)
  }
  readr::write_csv(pred, file_path)
}

#' Run the hub validations on a submission file and abort on failures.
#' The error message lists each failing check, so the targets metadata, the
#' health notebook, and the Slack alert all carry the full reason. targets keeps
#' only the first 2048 characters of an error, so each check message is cut to
#' `max_chars` to keep every failing check visible.
validate_submission_or_abort <- function(hub_path, file_path, max_chars = 300) {
  validation <- hubValidations::validate_submission(hub_path, file_path = file_path)
  # The result groups the checks by where they ran (hub config, then the file).
  checks <- unlist(unname(validation), recursive = FALSE)
  failing <- purrr::keep(checks, hubValidations::not_pass)
  if (length(failing) > 0) {
    messages <- purrr::map_chr(failing, "message")
    messages <- ifelse(nchar(messages) > max_chars, paste0(substr(messages, 1, max_chars - 3), "..."), messages)
    details <- sprintf("%s: %s", names(failing), messages)
    rlang::abort(c(
      sprintf("Hub validation failed for %s:", file_path),
      rlang::set_names(details, rep("x", length(details)))
    ))
  }
  validation
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
    if (length(idx) == 0) stop(glue("Template is missing section '{header}' — add it to rendered_reports/template.md"))
    append(content, link, after = idx[[1]] + 1L)
  }
  # Define the directories
  reports_dir <- "rendered_reports"
  template_path <- "rendered_reports/template.md"

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
  # Season a date belongs to; dates outside the table go to the nearest end.
  season_of <- function(date) {
    seasons$season_name[pmax(findInterval(as.Date(date), seasons$season_start), 1L)]
  }
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

    report_md_content <- insert_after_section(report_md_content, glue("## Score notebooks {season_of(generation_date)} Season"), report_link)
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
  explore_nowcast_files <- dir_ls(reports_dir, regexp = ".*-nowcast-notebook-\\d{4}_\\d{4}\\.html")
  explore_files <- c(explore_overall_files, explore_family_files, explore_nowcast_files)
  if (length(explore_files) > 0) {
    explore_table <- tibble(filename = explore_files) %>%
      mutate(
        file_name = path_file(filename),
        disease = str_extract(file_name, "^(flu|covid|rsv)"),
        season_slug = str_extract(file_name, "\\d{4}_\\d{4}"),
        season_name = str_replace(season_slug, "_", "-"),
        is_overall = grepl("overall", file_name),
        family = case_when(
          is_overall ~ "Overall",
          grepl("-nowcast-notebook-", file_name) ~ "h−1 nowcasts",
          TRUE ~ str_remove(str_remove(file_name, glue("^{disease}-notebook-")), "-\\d{4}_\\d{4}\\.html$")
        )
      ) %>%
      arrange(disease, season_name, desc(is_overall), family)

    # Pages are by the season's start year: 2024-2025 and 2024-2026 are 2024 experiments.
    for (year_group in c(2024, 2025)) {
      group_rows <- explore_table %>%
        filter(if (year_group == 2024) as.integer(substr(season_name, 1, 4)) <= 2024 else as.integer(substr(season_name, 1, 4)) >= 2025)
      lines <- character(0)
      for (season_name in sort(unique(group_rows$season_name), decreasing = TRUE)) {
        season_rows <- group_rows %>%
          filter(season_name == .env$season_name) %>%
          arrange(disease, desc(is_overall), family)
        lines <- c(lines, "", glue("### {season_name}"), "")
        for (ii in seq_len(nrow(season_rows))) {
          row <- season_rows[ii, ]
          lines <- c(lines, sprintf("- [%s %s](%s)", str_to_title(row$disease), row$family, row$file_name))
        }
      }
      if (length(lines) > 0) {
        report_md_content <- insert_after_section(report_md_content, glue("## {year_group} Explore Notebooks"), lines)
      }
    }
  }

  # Health checks: newest render per (forecast date, disease). The latest
  # week's are inserted last into "Most recent week" so they land at its top.
  health_files <- dir_ls(reports_dir, regexp = ".*_health_on_.*\\.html")
  if (length(health_files) > 0) {
    health_parts <- str_match(path_file(health_files), "(\\d{4}-\\d{2}-\\d{2})_(.*)_health_on_(\\d{4}-\\d{2}-\\d{2})\\.html")
    health_table <- tibble(
      file_name = health_parts[, 1],
      forecast_date = ymd(health_parts[, 2]),
      disease = health_parts[, 3],
      generation_date = ymd(health_parts[, 4])
    ) %>%
      filter(!is.na(file_name)) %>%
      group_by(forecast_date, disease) %>%
      slice_max(generation_date, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(forecast_date, desc(disease))
    health_link <- function(row) {
      sprintf("- [%s health check, forecast %s (rendered %s)](%s)", str_to_title(row$disease), row$forecast_date, row$generation_date, row$file_name)
    }
    for (ii in seq_len(nrow(health_table))) {
      row <- health_table[ii, ]
      report_md_content <- insert_after_section(
        report_md_content, glue("## Weekly Health Checks {season_of(row$forecast_date)} Season"), health_link(row)
      )
    }
    latest_health <- health_table %>% filter(forecast_date == max(forecast_date))
    for (ii in seq_len(nrow(latest_health))) {
      report_md_content <- insert_after_section(report_md_content, "## Most recent week", health_link(latest_health[ii, ]))
    }
  }

  # The template is one document; each top-level section belongs to one page.
  # Anything not listed (the title, Overview, Most recent week) stays on the index.
  page_sections <- list(
    "weekly_2025-2026" = c("Weekly Health Checks 2025-2026 Season", "Score notebooks 2025-2026 Season", "Weekly Fanplots 2025-2026 Season"),
    "weekly_2024-2025" = c("Weekly Health Checks 2024-2025 Season", "Score notebooks 2024-2025 Season", "Weekly Fanplots 2024-2025 Season"),
    "experiments_2026" = c("2026 Experiments", "Calibration Experiments", "2025-2026 Season Backtesting"),
    "experiments_2025" = c("2025 Explore Notebooks"),
    "experiments_2024" = c("2024-2025 Season Reports", "2024 Explore Notebooks", "2024-2025 Season Backtesting", "2023-2024 Season Backtesting"),
    "forecasters" = c("Description of Forecaster Families", "Exploration Summary 2024-2025")
  )
  page_titles <- c(
    index = "Home",
    "weekly_2025-2026" = "Weekly reports 2025-2026",
    "weekly_2024-2025" = "Weekly reports 2024-2025",
    experiments_2026 = "2026 experiments",
    experiments_2025 = "2025 experiments",
    experiments_2024 = "2024 experiments",
    forecasters = "Forecasters"
  )
  is_header <- startsWith(report_md_content, "## ")
  section_id <- cumsum(is_header)
  section_title <- sub("^## ", "", report_md_content[is_header])
  page_of_section <- vapply(c("", section_title), function(title) {
    hit <- names(page_sections)[vapply(page_sections, \(x) title %in% x, logical(1))]
    if (length(hit) == 0) "index" else hit
  }, character(1), USE.NAMES = FALSE)
  line_page <- page_of_section[section_id + 1L]
  unassigned <- setdiff(unlist(page_sections), section_title)
  if (length(unassigned) > 0) stop("Template is missing sections: ", paste(unassigned, collapse = ", "))

  nav <- paste(
    sprintf("[%s](%s.html)", page_titles, names(page_titles)),
    collapse = " | "
  )
  for (page in names(page_titles)) {
    body <- report_md_content[line_page == page]
    if (page == "index") {
      # The index carries the title block, then the page list under the overview.
      page_list <- c("", "## Pages", "", sprintf("- [%s](%s.html)", page_titles[-1], names(page_titles)[-1]), "")
      overview_end <- which(startsWith(body, "## Most recent week"))[1]
      body <- append(body, page_list, after = overview_end - 1L)
    } else {
      body <- c(paste0("# ", page_titles[[page]]), "", nav, "", body)
    }
    md_path <- path(reports_dir, paste0(if (page == "index") "report" else page, ".md"))
    writeLines(body, md_path)
    system2("pandoc", c(
      md_path, "-s", "-o", path(reports_dir, paste0(page, ".html")), "--css=style.css",
      "--mathjax=https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js",
      "--metadata", shQuote(paste0("pagetitle=Delphi Reports: ", page_titles[[page]]))
    ))
  }
}
