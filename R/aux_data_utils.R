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
  return(max(as.numeric(MMWRweek::MMWRweek(last_week_of_year)$MMWRweek)))
}

convert_epiweek_to_season_week <- function(epiyear, epiweek, season_start = 39) {
  season_week <- epiweek - 39
  update_inds <- season_week <= 0
  if (!any(update_inds)) {
    # none need to be updated
    return(season_week)
  }
  # last year's # of epiweeks determines which week in the season we're at at
  # the beginning of the year
  season_week[update_inds] <- season_week[update_inds] +
    sapply(epiyear[update_inds] - 1, epiweeks_in_year)

  return(season_week)
}

#' add a sine and half sine component; it is zero after `season` (by default 35, which roughly corresponds to epiweek 23)
step_season_week_sine <- function(preproc, season = 35) {
  preproc %<>%
    step_mutate(
      season_half_sine = sinpi((pmin(season_week, !!season + 1) - 1) / !!season),
      season_sine = sinpi(2 * (pmin(season_week, !!season + 1) - 1) / !!season),
      role = "pre-predictor"
    )
}




#' Append the state population and state population density, taken from the census and interpolated in the most straightforward way.
#' apportionment data taken from here: https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html
#' there's probably a better way of doing this buried in
#' https://www.census.gov/data/developers/data-sets/popest-popproj/popest.html,
#' but for now it's not worth the time
#' @param original_dataset tibble or epi_df, should have states as 2 letter lower case
add_pop_and_density <-
  function(original_dataset,
           apportion_filename = here::here("aux_data", "flusion_data", "apportionment.csv"),
           state_code_filename = here::here("aux_data", "flusion_data", "state_codes_table.csv"),
           hhs_code_filename = here::here("aux_data", "flusion_data", "state_code_hhs_table.csv")) {
    pops_by_state_hhs <- gen_pop_and_density_data(apportion_filename, state_code_filename, hhs_code_filename)
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

gen_pop_and_density_data <-
  function(apportion_filename = here::here("aux_data", "flusion_data", "apportionment.csv"),
           state_code_filename = here::here("aux_data", "flusion_data", "state_codes_table.csv"),
           hhs_code_filename = here::here("aux_data", "flusion_data", "state_code_hhs_table.csv")) {
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
    pops_by_state_hhs
  }
daily_to_weekly <- function(epi_df, agg_method = c("sum", "mean"), day_of_week = 4L, day_of_week_end = 7L, keys = "geo_value", values = c("value")) {
  epi_df %>%
    mutate(epiweek = epiweek(time_value), year = epiyear(time_value)) %>%
    group_by(across(any_of(c(keys, "epiweek", "year")))) %>%
    summarize(
      across(all_of(values), ~ sum(.x, na.rm = TRUE)),
      time_value = floor_date(max(time_value), "weeks", week_start = 7) + 3,
      .groups = "drop"
    ) %>%
    select(-epiweek, -year)
}

daily_to_weekly_archive <- function(epi_arch,
                                    agg_columns,
                                    agg_method = c("sum", "mean"),
                                    day_of_week = 4L,
                                    day_of_week_end = 7L) {
  agg_method <- arg_match(agg_method)
  keys <- key_colnames(epi_arch, exclude = "time_value")
  ref_time_values <- epi_arch$DT$version %>%
    unique() %>%
    sort()
  if (agg_method == "sum") {
    slide_fun <- epi_slide_sum
  } else if (agg_method == "mean") {
    slide_fun <- epi_slide_mean
  }
  too_many_tibbles <- epix_slide(
    epi_arch,
    .before = 99999999L,
    .versions = ref_time_values,
    function(x, group, ref_time) {
      x %>%
        group_by(across(all_of(keys))) %>%
        slide_fun(agg_columns, .window_size = 7L) %>%
        select(-all_of(agg_columns)) %>%
        rename_with(~ gsub("slide_value_", "", .x)) %>%
        # only keep 1/week
        filter(wday(time_value) == day_of_week_end) %>%
        # switch time_value to the designated day of the week
        mutate(time_value = time_value - 7L + day_of_week) %>%
        as_tibble()
    }
  )
  too_many_tibbles %>%
    as_epi_archive(compactify = TRUE)
}




#' for training, we don't want off-season times or anomalous seasons, but for
#' prediction we do
drop_non_seasons <- function(epi_data, min_window = 12) {
  forecast_date <- attributes(epi_data)$metadata$as_of %||% max(epi_data$time_value)
  if (!("season_week" %in% names(epi_data))) {
    epi_data %<>%
      mutate(
        epiweek = epiweek(time_value),
        year = epiyear(time_value),
        season_week = convert_epiweek_to_season_week(year, epiweek)
      )
  }
  epi_data %>%
    filter(
      (season_week < 35) |
        (forecast_date - time_value < as.difftime(min_window, units = "weeks")),
      season != "2020/21",
      # season != "2021/22", # keeping this because whitening otherwise gets really bad with the single season of data
      (season != "2019/20") | (time_value < "2020-03-01"),
      season != "2008/09"
    )
}
