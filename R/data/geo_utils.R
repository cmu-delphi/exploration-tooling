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

#' Append the state population and state population density, taken from the census and interpolated in the most straightforward way.
#' apportionment data taken from here: https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html
#' @param original_dataset tibble or epi_df, should have states as 2 letter lower case
add_pop_and_density <-
  function(
    original_dataset,
    apportion_filename = here::here("aux_data", "flusion_data", "apportionment.csv"),
    state_code_filename = here::here("aux_data", "flusion_data", "state_codes_table.csv"),
    hhs_code_filename = here::here("aux_data", "flusion_data", "state_code_hhs_table.csv")
  ) {
    pops_by_state_hhs <- gen_pop_and_density_data(apportion_filename, state_code_filename, hhs_code_filename)
    # if the dataset uses "usa" instead of "us", substitute that
    if ("usa" %in% unique(original_dataset)$geo_value) {
      pops_by_state_hhs %<>%
        mutate(
          geo_value = ifelse(geo_value == "us", "usa", geo_value),
          agg_level = ifelse(
            grepl("[0-9]{2}", geo_value),
            "hhs_region",
            ifelse(("us" == geo_value) | ("usa" == geo_value), "nation", "state")
          )
        )
    }
    if (!("agg_level" %in% names(original_dataset))) {
      original_dataset %<>% add_agg_level()
    }
    original_dataset %>%
      mutate(year = year(time_value)) %>%
      left_join(
        pops_by_state_hhs,
        by = join_by(year, geo_value, agg_level)
      ) %>%
      # virgin islands data too limited for now
      filter(geo_value != "vi") %>%
      arrange(geo_value, time_value) %>%
      ungroup() %>%
      fill(population, density)
  }

add_agg_level <- function(data) {
  data %>%
    mutate(
      agg_level = case_when(
        grepl("[0-9]{2}", geo_value) ~ "hhs_region",
        geo_value %in% c("us", "usa") ~ "nation",
        .default = "state"
      )
    )
}

gen_pop_and_density_data <-
  function(
    apportion_filename = here::here("aux_data", "flusion_data", "apportionment.csv"),
    state_code_filename = here::here("aux_data", "flusion_data", "state_codes_table.csv"),
    hhs_code_filename = here::here("aux_data", "flusion_data", "state_code_hhs_table.csv")
  ) {
    apportionment_data <- readr::read_csv(apportion_filename, show_col_types = FALSE) %>% as_tibble()
    imputed_pop_data <- apportionment_data %>%
      filter(`Geography Type` %in% c("State", "Nation")) %>%
      select(Name, Year, `Resident Population`, `Resident Population Density`) %>%
      group_by(Name) %>%
      reframe(
        population = spline(Year, `Resident Population`, n = 2020 - 1910 + 1)$y,
        density = spline(Year, `Resident Population Density`, n = 2020 - 1910 + 1)$y,
        Year = seq(1910, 2020, by = 1)
      )
    # converting names and adding to hhs_regions
    state_codes <- readr::read_csv(state_code_filename, show_col_types = FALSE) %>%
      mutate(state_code = as.character(as.integer(state_code)))

    hhs_codes <- readr::read_csv(hhs_code_filename, show_col_types = FALSE) %>%
      mutate(state_code = as.character(as.integer(state_code)))

    # switching the names to codes, getting the hhs region sums
    pops_by_state_hhs <-
      state_codes %>%
      left_join(hhs_codes, by = join_by(state_code)) %>%
      mutate(hhs = as.character(hhs)) %>%
      right_join(imputed_pop_data, by = join_by(state_name == Name)) %>%
      select(-state_name, -state_code) %>%
      rename(state = state_id, hhs_region = hhs, year = Year) %>%
      pivot_longer(
        cols = c(state, hhs_region),
        values_to = "geo_value",
        names_to = "agg_level"
      )
    # remove hhs_region na geo_values (this is national, and should only be
    # present once)
    pops_by_state_hhs %<>%
      filter(!(is.na(geo_value) & (agg_level == "hhs_region"))) %>%
      group_by(year, agg_level, geo_value) %>%
      summarize(
        area = sum(population / density),
        population = sum(population),
        density = population / area,
        .groups = "drop"
      ) %>%
      select(-area)
    # deal with us missing from the state_codes/ hhs_codes tables
    pops_by_state_hhs %<>%
      mutate(
        geo_value = ifelse(is.na(geo_value), "us", geo_value)
      )
    # "project" populations forward into 2024 (should probably find the real data for this)
    pops_by_state_hhs %<>%
      bind_rows(
        expand_grid(
          year = c(2021, 2022, 2023, 2024),
          pops_by_state_hhs %>%
            select(agg_level, geo_value) %>%
            distinct()
        )
      ) %>%
      arrange(geo_value, year) %>%
      fill(population, density)
    # add us as both a nation and state
    pops_by_state_hhs %>%
      bind_rows(
        (.) %>% filter(geo_value == "us") %>% mutate(agg_level = "nation")
      )
  }

#' add a column summing the values in the hhs region
#' @param hhs_region_table the region table
add_hhs_region_sum <- function(archive_data_raw, hhs_region_table) {
  need_agg_level <- !("agg_level" %in% names(archive_data_raw))
  if (need_agg_level) {
    archive_data_raw %<>% mutate(agg_level = "state")
  }
  hhs_region_agg_state <-
    archive_data_raw %>%
    left_join(hhs_region_table, by = "geo_value") %>%
    filter(agg_level == "state") %>%
    as_tibble() %>%
    group_by(across(c(setdiff(data.table::key(archive_data_raw), "geo_value"), "hhs_region"))) %>%
    reframe(hhs_region = sum(hhs, na.rm = TRUE), across(everything(), ~.x)) %>%
    relocate(version, time_value, geo_value)

  archive_data_raw %<>%
    filter(agg_level != "state") %>%
    mutate(hhs_region = hhs) %>%
    bind_rows(hhs_region_agg_state)
  if (need_agg_level) {
    archive_data_raw %<>% select(-agg_level)
  }
  archive_data_raw
}

#' Append a national aggregate to a dataframe
#'
#' Computes national values by summing all the values per group_keys.
#' Removes pre-existing national values.
#'
#' @param df A dataframe with a `geo_value` column.
#' @param cols A character vector of column names to aggregate.
#' @param group_keys A character vector of column names to group by.
#' @return A dataframe with a `geo_value` column.
append_us_aggregate <- function(df, cols = NULL, group_keys = c("time_value")) {
  if (!(is.data.frame(df))) {
    cli::cli_abort("df must be a data.frame", call = rlang::caller_env())
  }
  national_col_names <- c("us", "usa", "national", "nation", "US", "USA")
  df1 <- df %>% filter(geo_value %nin% national_col_names)
  if (is.null(cols)) {
    df2 <- df1 %>%
      summarize(geo_value = "us", across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .by = all_of(group_keys))
  } else {
    df2 <- df1 %>%
      summarize(geo_value = "us", across(all_of(cols), ~ sum(.x, na.rm = TRUE)), .by = all_of(group_keys))
  }
  bind_rows(df1, df2)
}
