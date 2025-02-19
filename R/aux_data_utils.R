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

#' Adds epiweek, epiyear, season_week, season columns to the dataset.
#'
#' Assumes that the dataset has a time_value column that is a date. If
#' season_week or season already exist, they will be dropped and replaced.
add_season_info <- function(data) {
  if (!("time_value" %in% names(data))) {
    cli::cli_abort("'time_value' column not found in data", call = rlang::caller_fn())
  }

  data %>%
    select(-any_of(c("season", "season_week", "epiweek", "epiyear"))) %>%
    mutate(
      epiweek = epiweek(time_value),
      epiyear = epiyear(time_value)
    ) %>%
    left_join(
      (.) %>%
        distinct(epiweek, epiyear) %>%
        mutate(
          season = convert_epiweek_to_season(epiyear, epiweek),
          season_week = convert_epiweek_to_season_week(epiyear, epiweek)
        ),
      by = c("epiweek", "epiyear")
    )
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
    # if the dataset uses "usa" instead of "us", substitute that
    if ("usa" %in% unique(original_dataset)$geo_value) {
      pops_by_state_hhs %<>%
        mutate(
          geo_value = ifelse(geo_value == "us", "usa", geo_value),
          agg_level = ifelse(grepl("[0-9]{2}", geo_value),
            "hhs_region",
            ifelse(("us" == geo_value) | ("usa" == geo_value), "nation", "state")
          )
        )
    }
    if (!("agg_level" %in% names(original_dataset))) {
      original_dataset %<>%
        mutate(agg_level = ifelse(grepl("[0-9]{2}", geo_value), "hhs_region", ifelse(("us" == geo_value) | ("usa" == geo_value), "nation", "state")))
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
    # add us as both a nation and state
    pops_by_state_hhs %>%
      bind_rows(
        (.) %>% filter(geo_value == "us") %>% mutate(agg_level = "nation")
      )
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
  keys <- key_colnames(epi_arch, exclude = c("time_value", "version"))
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
      ref_time_last_week_end <-
        floor_date(ref_time, "week", day_of_week_end - 1) # this is over by 1
      max_time <- max(x$time_value)
      valid_slide_days <- seq.Date(
        from = ceiling_date(min(x$time_value), "week", week_start = day_of_week_end - 1),
        to = floor_date(max(x$time_value), "week", week_start = day_of_week_end - 1),
        by = 7L
      )
      if (wday(max_time) != day_of_week_end) {
        valid_slide_days <- c(valid_slide_days, max_time)
      }
      slid_result <- x %>%
        group_by(across(all_of(keys))) %>%
        slide_fun(
          agg_columns,
          .window_size = 7L,
          na.rm = TRUE,
          .ref_time_values = valid_slide_days
        ) %>%
        select(-all_of(agg_columns)) %>%
        rename_with(~ gsub("slide_value_", "", .x)) %>%
        # only keep 1/week
        # group_by week, keep the largest in each week
        # alternatively
        # switch time_value to the designated day of the week
        mutate(time_value = round_date(time_value, "week", day_of_week - 1)) %>%
        as_tibble()
    }
  )
  too_many_tibbles %>%
    pull(time_value) %>%
    max()
  too_many_tibbles %>%
    as_epi_archive(compactify = TRUE)
}


#' for training, we don't want off-season times or anomalous seasons, but for
#' prediction we do
drop_non_seasons <- function(epi_data, min_window = 12) {
  forecast_date <- attributes(epi_data)$metadata$as_of %||% max(epi_data$time_value)
  if ("season_week" %nin% names(epi_data)) {
    epi_data %<>% add_season_info()
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
    bind_rows(
      hhs_region_agg_state
    )
  if (need_agg_level) {
    archive_data_raw %<>% select(-agg_level)
  }
  archive_data_raw
}

#' hhs data in covidcast currently
get_health_data <- function(as_of, disease = c("covid", "flu")) {
  as_of <- as.Date(as_of)
  disease <- arg_match(disease)
  checkmate::assert_date(as_of, min.len = 1, max.len = 1)

  cache_path <- here::here("aux_data", "healthdata")
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }

  metadata_path <- here::here(cache_path, "metadata.csv")
  if (!file.exists(metadata_path)) {
    meta_data <- readr::read_csv("https://healthdata.gov/resource/qqte-vkut.csv?$query=SELECT%20update_date%2C%20days_since_update%2C%20user%2C%20rows%2C%20row_change%2C%20columns%2C%20column_change%2C%20metadata_published%2C%20metadata_updates%2C%20column_level_metadata%2C%20column_level_metadata_updates%2C%20archive_link%20ORDER%20BY%20update_date%20DESC%20LIMIT%2010000", show_col_types = FALSE)
    readr::write_csv(meta_data, metadata_path)
  } else {
    meta_data <- readr::read_csv(metadata_path, show_col_types = FALSE)
  }

  most_recent_row <- meta_data %>%
    # update_date is actually a time, so we need to filter for the day after.
    filter(update_date <= as_of + 1) %>%
    arrange(desc(update_date)) %>%
    slice(1)

  if (nrow(most_recent_row) == 0) {
    cli::cli_abort("No data available for the given date.")
  }

  data_filepath <- here::here(cache_path, sprintf("g62h-syeh-%s.csv", as.Date(most_recent_row$update_date)))
  if (!file.exists(data_filepath)) {
    data <- readr::read_csv(most_recent_row$archive_link, show_col_types = FALSE)
    readr::write_csv(data, data_filepath)
  } else {
    data <- readr::read_csv(data_filepath, show_col_types = FALSE)
  }
  if (disease == "covid") {
    data %<>% mutate(
      hhs = previous_day_admission_adult_covid_confirmed +
        previous_day_admission_adult_covid_suspected +
        previous_day_admission_pediatric_covid_confirmed +
        previous_day_admission_pediatric_covid_suspected
    )
  } else if (disease == "flu") {
    data %<>% mutate(hhs = previous_day_admission_influenza_confirmed)
  }
  # Minor data adjustments and column renames. The date also needs to be dated
  # back one, since the columns we use report previous day hospitalizations.
  data %>%
    mutate(
      geo_value = tolower(state),
      time_value = date - 1L,
      hhs = hhs,
      .keep = "none"
    ) %>%
    # API seems to complete state level with 0s in some cases rather than NAs.
    # Get something sort of compatible with that by summing to national with
    # na.omit = TRUE. As otherwise we have some NAs from probably territories
    # propagated to US level.
    bind_rows(
      (.) %>%
        group_by(time_value) %>%
        summarize(geo_value = "us", hhs = sum(hhs, na.rm = TRUE))
    )
}

calculate_burden_adjustment <- function(flusurv_latest) {
  # get burden data
  burden <- readr::read_csv(here::here("aux_data", "flusion_data", "flu_burden.csv"), show_col_types = FALSE) %>%
    separate(Season, into = c("StartYear", "season"), sep = "-") %>%
    select(season, contains("Estimate")) %>%
    mutate(season = as.double(season)) %>%
    mutate(season = paste0(
      as.character(season - 1), "/", substr(season, 3, 4)
    ))
  # get population data
  us_population <- readr::read_csv(here::here("aux_data", "flusion_data", "us_pop.csv"), show_col_types = FALSE) %>%
    rename(us_pop = POPTOTUSA647NWDB) %>%
    mutate(season = year(DATE)) %>%
    filter((season >= 2011) & (season <= 2020)) %>%
    select(season, us_pop) %>%
    mutate(season = paste0(as.character(season - 1), "/", substr(season, 3, 4)))
  # renormalize so that the total burden according to hhs matches the total
  # burden according to flusurv
  flusurv_latest %>%
    filter((geo_value == "us") & (start_year >= 2011) & (start_year <= 2020)) %>%
    group_by(season) %>%
    summarise(total_hosp_rate = sum(hosp_rate, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(burden, by = "season") %>%
    left_join(us_population, by = "season") %>%
    mutate(burden_est = total_hosp_rate * us_pop / 100000) %>%
    mutate(adj_factor = `Hospitalizations Estimate` / burden_est) %>%
    select(season, adj_factor)
}

generate_flusurv_adjusted <- function(day_of_week = 1) {
  flusurv_all <- pub_flusurv(
    locations = "CA,CO,CT,GA,MD,MI,MN,NM,NY_albany,NY_rochester,OH,OR,TN,UT,network_all",
    issues = epirange(123401, 345601)
  ) %>%
    select(geo_value = location, time_value = epiweek, hosp_rate = rate_overall, version = issue) %>%
    drop_na() %>%
    mutate(agg_level = case_when(
      geo_value == "network_all" ~ "nation",
      TRUE ~ "state"
    )) %>%
    mutate(
      geo_value = if_else(agg_level == "nation",
        str_replace_all(geo_value, "network_all", "us"),
        tolower(geo_value)
      )
    ) %>%
    mutate(
      geo_value = if_else(
        geo_value %in% c("ny_rochester", "ny_albany"),
        "ny",
        geo_value
      )
    )
  # sum the two ny regions and reappend to the original dataframe
  flusurv_all <- flusurv_all %>%
    filter(geo_value == "ny") %>%
    group_by(time_value, version) %>%
    summarize(
      geo_value = first(geo_value),
      agg_level = first(agg_level),
      hosp_rate = sum(hosp_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    bind_rows(
      flusurv_all %>% filter(geo_value != "ny")
    ) %>%
    arrange(geo_value, time_value, version)
  flusurv_all <-
    flusurv_all %>%
    mutate(
      epiyear = epiyear(time_value),
      epiweek = MMWRweek(time_value)$MMWRweek
    ) %>%
    left_join(
      (.) %>%
        distinct(epiyear, epiweek) %>%
        mutate(season = convert_epiweek_to_season(epiyear, epiweek)) %>%
        mutate(
          season_week = convert_epiweek_to_season_week(epiyear, epiweek),
          time_value = MMWRweek2Date(epiyear, epiweek, day_of_week)
        )
    ) %>%
    as_epi_archive(compactify = TRUE)
  # create a latest epi_df
  flusurv_all_latest <- flusurv_all %>%
    epix_as_of(version = max(.$DT$version)) %>%
    as_tibble() %>%
    mutate(start_year = as.numeric(substr(season, 1, 4)))
  adj_factor <- calculate_burden_adjustment(flusurv_all_latest)
  flusurv_lat <- flusurv_all$DT %>%
    left_join(adj_factor, by = "season") %>%
    drop_na() %>%
    mutate(adj_hosp_rate = hosp_rate * adj_factor, source = "flusurv")
  flusurv_lat %>%
    mutate(
      geo_value = if_else(geo_value %in% c("ny_rochester", "ny_albany"),
        "ny",
        geo_value
      )
    ) %>%
    group_by(geo_value, time_value, version, agg_level) %>%
    summarise(
      hosp_rate = mean(hosp_rate, na.rm = TRUE),
      adj_factor = mean(adj_factor, na.rm = TRUE),
      adj_hosp_rate = mean(adj_hosp_rate, na.rm = TRUE),
      epiyear = first(epiyear),
      epiweek = first(epiweek),
      season = first(season),
      season_week = first(season_week),
      .groups = "drop"
    ) %>%
    as_epi_archive(compactify = TRUE)
}


process_who_nrevss <- function(filename1, filename2, filename3) {
  clinical_lab_pos <- readr::read_csv(
    here::here("aux_data", "flusion_data", filename1),
    skip = 1, show_col_types = FALSE
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "PERCENT POSITIVE")
  combined_pos <- readr::read_csv(
    here::here("aux_data", "flusion_data", filename2),
    skip = 1, show_col_types = FALSE
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "PERCENT POSITIVE")
  pos_state <- bind_rows(clinical_lab_pos, combined_pos)
  ili_state <- readr::read_csv(
    here::here("aux_data", "flusion_data", filename3),
    skip = 1, show_col_types = FALSE
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "% WEIGHTED ILI", "%UNWEIGHTED ILI")
  merge(pos_state, ili_state, by = c("REGION TYPE", "REGION", "YEAR", "WEEK")) %>%
    mutate(across(all_of("PERCENT POSITIVE"), as.numeric)) %>%
    mutate(across(any_of("% UNWEIGHTED ILI"), as.numeric)) %>%
    mutate(across(any_of("%UNWEIGHTED ILI"), as.numeric)) %>%
    mutate(across(any_of("% WEIGHTED ILI"), as.numeric)) %>%
    as_tibble()
}

gen_ili_data <- function(default_day_of_week = 1) {
  ili_plus_nation <- process_who_nrevss(
    "WHO_NREVSS_Clinical_Labs_Nation.csv",
    "WHO_NREVSS_Combined_prior_to_2015_16_Nation.csv",
    "ILINet_Nation.csv"
  )
  ili_plus_HHS <- process_who_nrevss(
    "WHO_NREVSS_Clinical_Labs_HHS.csv",
    "WHO_NREVSS_Combined_prior_to_2015_16_HHS.csv",
    "ILINet_HHS.csv"
  )
  ili_plus_state <- process_who_nrevss(
    "WHO_NREVSS_Clinical_Labs_State.csv",
    "WHO_NREVSS_Combined_prior_to_2015_16_State.csv",
    "ILINet_State.csv"
  ) %>%
    mutate(`% WEIGHTED ILI` = `%UNWEIGHTED ILI`)

  ili_plus <- bind_rows(ili_plus_HHS, ili_plus_nation, ili_plus_state) %>%
    mutate(across(c(`PERCENT POSITIVE`, `% WEIGHTED ILI`), as.numeric)) %>%
    select(-`%UNWEIGHTED ILI`) %>%
    mutate(value = `PERCENT POSITIVE` * `% WEIGHTED ILI` / 100, source = "ILI+") %>%
    rename(agg_level = `REGION TYPE`, geo_value = REGION) %>%
    mutate(agg_level = str_replace_all(agg_level, "HHS Regions", "hhs_region")) %>%
    mutate(agg_level = str_replace_all(agg_level, "National", "nation")) %>%
    mutate(agg_level = str_replace_all(agg_level, "States", "state")) %>%
    mutate(geo_value = if_else(agg_level == "hhs_region",
      str_replace_all(geo_value, "Region (\\d+)", "\\1"),
      geo_value
    )) %>%
    mutate(geo_value = if_else(agg_level == "nation",
      str_replace_all(geo_value, "X", "us"),
      geo_value
    )) %>%
    rename(epiyear = YEAR, epiweek = WEEK) %>%
    left_join(
      (.) %>%
        distinct(epiyear, epiweek) %>%
        mutate(season = convert_epiweek_to_season(epiyear, epiweek)) %>%
        mutate(
          season_week = convert_epiweek_to_season_week(epiyear, epiweek),
          time_value = MMWRweek2Date(epiyear, epiweek, default_day_of_week),
          version = time_value
        )
    )
  # map names to lower case
  name_map <- tibble(abb = state.abb, name = state.name) %>%
    bind_rows(
      tribble(
        ~name, ~abb,
        "District of Columbia", "DC",
        "American Samoa", "AS",
        "Guam", "GU",
        "Northern Mariana Islands", "MP",
        "Puerto Rico", "PR",
        "Virgin Islands", "VI",
        "Trust Territories", "TT",
        "us", "US",
        "New York City", "ny"
      )
    ) %>%
    mutate(abb = tolower(abb))
  ili_states <- ili_plus %>%
    filter(agg_level == "state") %>%
    left_join(name_map, by = join_by(geo_value == name)) %>%
    select(
      geo_value = abb, time_value, version, agg_level, value, season,
      season_week, `PERCENT POSITIVE`, `% WEIGHTED ILI`, source, epiyear, epiweek
    )

  # aggregate NYC and NY state
  ili_plus <- ili_states %>%
    filter(geo_value == "ny") %>%
    group_by(time_value, version) %>%
    summarize(
      geo_value = first(geo_value),
      agg_level = first(agg_level),
      season = first(season),
      season_week = first(season_week),
      `PERCENT POSITIVE` = mean(`PERCENT POSITIVE`, na.rm = TRUE),
      `% WEIGHTED ILI` = mean(`% WEIGHTED ILI`, na.rm = TRUE),
      source = first(source),
      epiweek = first(epiweek),
      epiyear = first(epiyear),
      .groups = "drop"
    ) %>%
    bind_rows(
      ili_states %>% filter(geo_value != "ny"),
      ili_plus %>% filter(agg_level != "state")
    ) %>%
    rename(hhs = value) %>%
    as_epi_archive(compactify = TRUE)
}

process_nhsn_data <- function(raw_nhsn_data) {
  # These are exception dates when the data was available on a different day
  # than usual. In these two cases, it was the Thursday after. But to keep
  # the rest of the pipeline the same, we pretend it was available on Wednesday.
  remap_exceptions <- list(
    "2024-12-26" = "2024-12-25",
    "2025-01-02" = "2025-01-01"
  )
  fixed_version <- remap_exceptions[[as.character(Sys.Date())]] %||% Sys.Date()
  raw_nhsn_data %>%
    mutate(
      geo_value = tolower(jurisdiction),
      time_value = as.Date(weekendingdate),
      nhsn_covid = totalconfc19newadm,
      nhsn_flu = totalconfflunewadm
    ) %>%
    add_season_info() %>%
    select(-weekendingdate, -jurisdiction, -starts_with("totalconf")) %>%
    pivot_longer(cols = starts_with("nhsn"), names_to = "disease") %>%
    filter(!is.na(value)) %>%
    mutate(version = fixed_version) %>%
    relocate(geo_value, disease, time_value, version)
}


# for filenames of the form nhsn_data_2024-11-19_16-29-43.191649.rds
get_version_timestamp <- function(filename) ymd_hms(str_match(filename, "[0-9]{4}-..-.._..-..-..\\.[^.^_]*"))

#' all in one function to get and cache a nhsn archive from raw files
#' @description
#' This takes in all of the raw data files for the nhsn data, creates a
#'   quasi-archive (it keeps one example per version-day, rather than one per
#'   change), and puts it in the bucket. The stored value has the columns
#'   geo_value, time_value, disease, endpoint (either basic or prelim), version,
#'   version_timestamp (to enable keeping the most recent value), and value.
#'   The returned value on the other hand is an actual epi_archive, only
#'   containing the data for `disease_name`.
create_nhsn_data_archive <- function(disease_name) {
  if (aws.s3::head_object("archive_timestamped.parquet", bucket = "forecasting-team-data")) {
    aws.s3::save_object("archive_timestamped.parquet", bucket = "forecasting-team-data", file = here::here("cache/archive_timestamped.parquet"))
    previous_archive <- qs::qread(here::here("cache/archive_timestamped.parquet"))
    last_timestamp <- max(previous_archive$version_timestamp)
  } else {
    # there is no remote
    previous_archive <- NULL
    last_timestamp <- as.Date("1000-01-01")
  }
  new_data <- aws.s3::get_bucket_df(bucket = "forecasting-team-data", prefix = "nhsn_data_") %>%
    filter(get_version_timestamp(Key) > last_timestamp) %>%
    pull(Key) %>%
    lapply(
      function(filename) {
        version_timestamp <- get_version_timestamp(filename)
        res <- NULL
        tryCatch(
          {
            s3load(object = filename, bucket = "forecasting-team-data")
            if (grepl("prelim", filename)) {
              res <- epi_data_raw_prelim
              endpoint_val <- "prelim"
            } else {
              res <- epi_data_raw
              endpoint_val <- "basic"
            }
            res <- res %>%
              process_nhsn_data() %>%
              select(geo_value, disease, time_value, value) %>%
              mutate(version_timestamp = version_timestamp, endpoint = endpoint_val)
          },
          error = function(cond) {}
        )
        res
      }
    )
  # drop any duplicates on the same day
  compactified <-
    new_data %>%
    bind_rows()
  if (nrow(compactified) == 0) {
    one_per_day <- previous_archive
  } else {
    compactified <-
      compactified %>%
      arrange(geo_value, time_value, disease, endpoint, version_timestamp) %>%
      mutate(version = as.Date(version_timestamp)) %>%
      filter(if_any(
        c(everything(), -endpoint, -version_timestamp), # all non-version, non-endpoint columns
        ~ !epiprocess:::is_locf(., .Machine$double.eps^0.5)
      ))

    unchanged <- previous_archive %>% filter(!(version %in% unique(compactified$version)))
    # only keep the last value for a given version (so across version_timestamps)
    # we only need to do this for the versions in compactified, as the other versions can't possibly change
    one_per_day <-
      previous_archive %>%
      filter(version %in% unique(compactified$version)) %>%
      bind_rows(compactified) %>%
      group_by(geo_value, disease, time_value, version) %>%
      arrange(version_timestamp) %>%
      filter(row_number() == n()) %>%
      ungroup() %>%
      bind_rows(unchanged)
    qs::qsave(one_per_day, here::here("cache/archive_timestamped.parquet"))
    aws.s3::put_object(here::here("cache/archive_timestamped.parquet"), "archive_timestamped.parquet", bucket = "forecasting-team-data")
  }
  one_per_day %>%
    filter(disease == disease_name) %>%
    select(-version_timestamp, -endpoint, -disease) %>%
    mutate(
      geo_value = ifelse(geo_value == "usa", "us", geo_value)
    ) %>%
    filter(geo_value != "mp") %>%
    as_epi_archive(compactify = TRUE)
}


up_to_date_nssp_state_archive <- function(disease = c("covid", "influenza")) {
  disease <- arg_match(disease)
  nssp_state <- pub_covidcast(
    source = "nssp",
    signal = glue::glue("pct_ed_visits_{disease}"),
    time_type = "week",
    geo_type = "state",
    geo_values = "*",
    issues = "*"
  )
  nssp_state %>%
    select(geo_value, time_value, issue, nssp = value) %>%
    as_epi_archive(compactify = TRUE) %>%
    `$`("DT") %>%
    # End of week to midweek correction.
    mutate(time_value = time_value + 3) %>%
    as_epi_archive(compactify = TRUE)
}
