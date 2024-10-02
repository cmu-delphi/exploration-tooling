convert_epiweek_to_season <- function(epiyear, epiweek) {
  # Convert epiweek to season
  update_inds <- epiweek <= 39
  epiyear <- ifelse(update_inds, epiyear - 1, epiyear)

  season <- paste0(epiyear, "/", substr((epiyear + 1), 3, 4))
  return(season)
}

epiweeks_in_year <- function(year) {
  last_week_of_year <- seq.Date(as.Date(paste0(year, "-12-24")),
    as.Date(paste0(year, "-12-31")),
    by = 1
  )
  return(max(as.numeric(MMWRweek(last_week_of_year)$MMWRweek)))
}

convert_epiweek_to_season_week <- function(epiyear, epiweek, season_start = 39) {
  season_week <- epiweek - 39

  update_inds <- season_week <= 0
  # last year's # of epiweeks determines which week in the season we're at at
  # the beginning of the year
  season_week[update_inds] <- season_week[update_inds] +
    sapply(epiyear[update_inds] - 1, epiweeks_in_year)

  return(season_week)
}


#' Append the state population and state population density, taken from the census and interpolated in the most straightforward way.
#' apportionment data taken from here: https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html
#' there's probably a better way of doing this buried in
#' https://www.census.gov/data/developers/data-sets/popest-popproj/popest.html,
#' but for now it's not worth the time
#' @param original_dataset tibble or epi_df, should have states as 2 letter lower case
add_pop_and_density <- function(original_dataset,
                                apportion_filename = here::here("aux_data", "flusion_data", "apportionment.csv"),
                                state_code_filename = here::here("aux_data", "flusion_data", "state_codes_table.csv"),
                                hhs_code_filename = here::here("aux_data", "flusion_data", "state_code_hhs_table.csv")) {
  browser()
  apportionment_data <- read_csv(apportion_filename) %>% as_tibble()
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
  state_codes <- read_csv(state_code_filename) %>%
    mutate(state_code = as.character(as.integer(state_code)))

  hhs_codes <- read_csv(hhs_code_filename) %>%
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
    ) %>%
    # remove hhs_region na geo_values (this is national, and should only be present once)
    filter(!(is.na(geo_value) & (agg_level == "hhs_region"))) %>%
    group_by(year, agg_level, geo_value) %>%
    summarize(
      area = sum(population / density), population = sum(population),
      density = population / area, .groups = "drop"
    ) %>%
    select(-area) %>%
    # deal with us missing from the state_codes/ hhs_codes tables
    mutate(
      geo_value = ifelse(is.na(geo_value), "us", geo_value)
    )

  original_dataset %>%
    mutate(year = year(time_value)) %>%
    left_join(
      pops_by_state_hhs,
      by = join_by(year, geo_value)
    ) %>%
    # virgin islands data too limited for now
    filter(geo_value != "vi") %>%
    arrange(geo_value, time_value) %>%
    fill(population, density)
}
