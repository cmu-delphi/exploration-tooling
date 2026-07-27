# Compare NSSP ED-visit data from the two upstream sources (the CDC
# covid19-forecast-hub GitHub mirror vs the Socrata API) to check they agree.
# Run when you suspect the two feeds have diverged. Prints the rows whose
# values differ by more than 1e-6.
#
# Was previously the dead get_nssp_upstream/check_nssp_socrata_github_diff pair
# in R/; nothing in the pipeline called it, so it lives here as a manual check.

suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

disease <- "covid" # or "influenza"

state_map <- get_population_data() %>% filter(state_id != "usa")

process_nssp <- function(raw_file) {
  raw_file %>%
    filter(county == "All") %>%
    left_join(state_map, by = join_by(geography == state_name)) %>%
    select(
      geo_value = state_id,
      time_value = week_end,
      value = starts_with(glue::glue("percent_visits_{disease}"))
    ) %>%
    mutate(time_value = as.Date(floor_date(time_value, "week", week_start = 7) + 3)) %>%
    mutate(version = as.Date(floor_date(Sys.Date(), "week", week_start = 7) + 3)) %>%
    arrange(desc(time_value))
}

# GitHub mirror (a single latest.parquet snapshot).
github_file <- tempfile(fileext = ".parquet")
httr2::request(
  "https://raw.githubusercontent.com/CDCgov/covid19-forecast-hub/refs/heads/main/auxiliary-data/nssp-raw-data/latest.parquet"
) %>%
  httr2::req_perform(path = github_file)
github_df <- process_nssp(nanoparquet::read_parquet(github_file))

# Socrata API.
socrata_df <- read_csv(
  glue::glue(
    "https://data.cdc.gov/resource/rdmq-nq56.csv?$limit=1000000&$select=geography,week_end,county,percent_visits_{disease}"
  ),
  show_col_types = FALSE
) %>%
  process_nssp()

diffs <- full_join(github_df, socrata_df, by = c("geo_value", "time_value", "version")) %>%
  filter(abs(value.x - value.y) > 1e-6)

print(diffs)
