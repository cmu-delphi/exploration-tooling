library(epidatr)
library(epiprocess)
library(epipredict)
library(dplyr)
library(tsibble)
library(tidyverse)
library(purrr)
## library(fable)
## library(fabletools)
library(ggplot2)
library(scoringutils)
library(feasts)
# library(forecast)
library(lubridate)
library(ISOweek)
library(readr)
library(MMWRweek)
library(rlang)
library(vctrs)
library(testthat)
# which day of the week should represent that mmwr week?
default_day_of_week <- 1


convert_epiweek_to_season <- function(epiyear, epiweek) {
  # Convert epiweek to season
  update_inds <- epiweek <= 39
  epiyear <- ifelse(update_inds, epiyear - 1, epiyear)

  season <- paste0(epiyear, "/", substr((epiyear + 1), 3, 4))
  return(season)
}

epiweeks_in_year <- function(year) {
  last_day_of_year <- as.Date(paste0(year, "-12-31"))
  return(as.numeric(MMWRweek("2021-12-31")$MMWRweek))
}

convert_epiweek_to_season_week <- function(epiyear, epiweek) {
  season_week <- epiweek - 39

  update_inds <- season_week <= 0

  season_week[update_inds] <- season_week[update_inds] +
    sapply(epiyear[update_inds], epiweeks_in_year)

  return(season_week)
}

convert_epiweek_to_date <- function(epiyear, epiweek,
                                    day_of_week = default_day_of_week) {
  end_date <- MMWRweek2Date(epiyear, epiweek, day_of_week)

  return(end_date)
}
convert_epiweek_to_date(c(2003, 2003), c(3, 4))



################# HHS ##########################
# this is more or less redundant
################# HHS ##########################
nhsn_state <- pub_covidcast(
  source = "hhs",
  signals = "confirmed_admissions_influenza_1d_prop",
  time_type = "day",
  geo_type = "state",
  issues = epirange(20000101, 20251231)
) %>%
  select(geo_value, time_value, admission_rate = value, version = issue) %>%
  mutate(agg_level = "state")
# as_epi_archive(compactify = FALSE)

nhsn_hhs_region <- pub_covidcast(
  source = "hhs",
  signals = "confirmed_admissions_influenza_1d_prop",
  time_type = "day",
  geo_type = "hhs",
  issues = epirange(20000101, 20251231)
) %>%
  select(geo_value, time_value, admission_rate = value, version = issue) %>%
  mutate(agg_level = "hhs_region")
# as_epi_archive(compactify = FALSE)

nhsn_nation <- pub_covidcast(
  source = "hhs",
  signals = "confirmed_admissions_influenza_1d_prop",
  time_type = "day",
  geo_type = "nation",
  issues = epirange(20000101, 20251231)
) %>%
  select(geo_value, time_value, admission_rate = value, version = issue) %>%
  mutate(agg_level = "nation")
# as_epi_archive(compactify = FALSE)

# Combine agg_level
nhsn <- bind_rows(nhsn_state, nhsn_hhs_region, nhsn_nation) %>% drop_na()
# Additional column: season, season_week, week_end_date
nhsn <- nhsn %>%
  mutate(epiyear = epiyear(time_value), epiweek = epiweek(time_value)) %>%
  mutate(season = convert_epiweek_to_season(epiyear, epiweek)) %>%
  mutate(season_week = convert_epiweek_to_season_week(epiyear, epiweek)) %>%
  mutate(week_end_date = convert_epiweek_to_week_end_date(epiyear, epiweek))

turn_nhsn_weekly <- function(x, gk = 0, rtv = 0) {
  x <- drop_na(x)

  x %>%
    group_by(geo_value, agg_level, week_end_date) %>%
    summarize(weekly_admission_rate = sum(admission_rate, na.rm = TRUE), .groups = "drop") %>%
    right_join(x, by = c("geo_value", "agg_level", "week_end_date"))
}
nhsn

################# FluSurv ##########################
# this is novel data
# its also weekly

calculate_burden_adjustment <- function(flusurv_latest) {
  # get burden data
  burden <- read_csv(here::here("aux_data", "flusion_data", "flu_burden.csv")) %>%
    separate(Season, into = c("StartYear", "season"), sep = "-") %>%
    select(season, contains("Estimate")) %>%
    mutate(season = as.double(season)) %>%
    mutate(season = paste0(
      as.character(season - 1), "/", substr(season, 3, 4)
    ))
  # get population data
  us_population <- read_csv(here::here("aux_data", "flusion_data", "us_pop.csv")) %>%
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
generate_flusurv_adjusted <- function(day_of_week = default_day_of_week) {
  flusurv_all <- pub_flusurv(
    locations = "CA,CO,CT,GA,MD,MI,MN,NM,NY_albany,NY_rochester,OH,OR,TN,UT,network_all",
    issues = epirange(123401, 345601)
  ) %>%
    select(geo_value = location, time_value = epiweek, hosp_rate = rate_overall, version = issue) %>%
    drop_na() %>%
    mutate(agg_level = case_when(
      geo_value == "network_all" ~ "nation",
      TRUE ~ "state"
    ))  %>%
    mutate(
      geo_value = if_else(agg_level == "nation",
                          str_replace_all(geo_value, "network_all", "us"),
                          tolower(geo_value))
    ) %>%
    mutate(
      geo_value = if_else(
        geo_value %in% c("ny_rochester", "ny_albany"),
        "ny",
        geo_value))
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
  flusurv_all <- flusurv_all %>%
    mutate(
      epiyear = epiyear(time_value),
      epiweek = MMWRweek(time_value)$MMWRweek,
      season = convert_epiweek_to_season(epiyear, epiweek),
      season_week = convert_epiweek_to_season_week(epiyear, epiweek)
    ) %>%
    as_epi_archive(compactify = TRUE)
  # create a latest epi_df
  flusurv_all_latest <- flusurv_all %>%
    epix_as_of(max_version = max(.$DT$version)) %>%
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
                          geo_value)) %>%
    group_by(geo_value, time_value, version, agg_level) %>%
    summarise(
      hosp_rate = mean(hosp_rate, na.rm = TRUE),
      adj_factor = mean(adj_factor, na.rm=TRUE),
      adj_hosp_rate = mean(adj_hosp_rate, na.rm=TRUE),
      epiyear = first(epiyear),
      epiweek = first(epiweek),
      season = first(season),
      season_week = first(season_week),
      .groups = "drop") %>%
    as_epi_archive(compactify = TRUE)
}

flusurv_adjusted <- generate_flusurv_adjusted()
# everything from flusurv is on sunday
expect_equal(flusurv_adjusted$DT$time_value %>% wday %>% unique, 1)
flusurv_adjusted$DT %>% group_by(season) %>% filter(epiyear == min(epiyear)) %>% summarize(minn = min(season_week), maxx = max(season_week)) %>% summarize(max(minn), min(maxx))

################# FluView ##########################
# this is novel data
# its also weekly
# https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html
process_who_nrevss <- function(filename1, filename2, filename3) {
  clinical_lab_pos <- read_csv(
    here::here("aux_data", "flusion_data", filename1),
    skip = 1
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "PERCENT POSITIVE")
  combined_pos <- read_csv(
    here::here("aux_data", "flusion_data", filename2),
    skip = 1
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "PERCENT POSITIVE")
  pos_state <- bind_rows(clinical_lab_pos, combined_pos)
  ili_state <- read_csv(
    here::here("aux_data", "flusion_data", filename3),
    skip = 1
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "% WEIGHTED ILI", "%UNWEIGHTED ILI")
  merge(pos_state, ili_state, by = c("REGION TYPE", "REGION", "YEAR", "WEEK")) %>%
    mutate(across(all_of("PERCENT POSITIVE"), as.numeric)) %>%
    mutate(across(any_of("% UNWEIGHTED ILI"), as.numeric)) %>%
    mutate(across(any_of("%UNWEIGHTED ILI"), as.numeric)) %>%
    mutate(across(any_of("% WEIGHTED ILI"), as.numeric)) %>%
    as_tibble()
}
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
) %>% mutate(abb = tolower(abb))

ili_plus <- bind_rows(ili_plus_HHS, ili_plus_nation, ili_plus_state) %>%
  mutate(across(c(`PERCENT POSITIVE`, `% WEIGHTED ILI`), as.numeric)) %>%
  select(-`%UNWEIGHTED ILI`) %>%
  mutate(value = `PERCENT POSITIVE` * `% WEIGHTED ILI` / 100, source = "ILI+") %>%
  rename(agg_level = `REGION TYPE`, geo_value = REGION) %>%
  mutate(agg_level = str_replace_all(agg_level, "HHS Regions", "hhs_region")) %>%
  mutate(agg_level = str_replace_all(agg_level, "National", "nation")) %>%
  mutate(agg_level = str_replace_all(agg_level, "States", "state")) %>%
  mutate(geo_value = if_else(agg_level == "hhs_region",
    str_replace_all(geo_value, "Region (\\d+)", "region_\\1"),
    geo_value
  )) %>%
  mutate(geo_value = if_else(agg_level == "nation",
    str_replace_all(geo_value, "X", "us"),
    geo_value
  )) %>%
  rename(epiyear = YEAR, epiweek = WEEK) %>%
  mutate(
    season = convert_epiweek_to_season(epiyear, epiweek),
    season_week = convert_epiweek_to_season_week(epiyear, epiweek),
    time_value = MMWRweek2Date(epiyear, epiweek, default_day_of_week),
    version = time_value
  )
ili_plus %>% filter(is.na(geo_value))
# map names to lower case
ili_states <- ili_plus %>%
  filter(agg_level == "state") %>%
  left_join(name_map, by=join_by(geo_value == name)) %>%
  select(
    geo_value = abb, time_value, version, agg_level, value, season,
    season_week, `PERCENT POSITIVE`, `% WEIGHTED ILI`, source, epiyear, epiweek)
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
  as_epi_archive(compactify = TRUE)

# is ili_plus always on a Sunday?
expect_equal(ili_plus$DT$time_value %>% wday %>% unique, 1)

# is the seasonweek actually starting at the right time? yes, the season always starts at 1, and not week 52, or week 2
ili_plus$DT %>% group_by(season) %>% filter(epiyear == min(epiyear)) %>% summarize(minn = min(season_week), maxx = max(season_week)) %>% summarize(max(minn), min(maxx))

ili_plus_nation_latest <- ili_plus %>%
  epix_as_of(max_version = max(.$DT$version)) %>%
  as_tibble() %>%
  filter(agg_level == "nation")

ggplot(ili_plus_nation_latest, aes(x = time_value)) +
  geom_line(aes(y = value, color = "adjusted"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `% WEIGHTED ILI`, color = "raw"), linetype = "dashed", size = 0.5) +
  scale_color_manual(name = "Legend", values = c("raw" = "orange", "adjusted" = "blue")) +
  labs(x = "Time", y = "ILI+ Rate", title = "Raw ILI Rate vs Adjusted ILI+ Rate over Time") +
  theme_minimal()

ggplot(ili_plus_nation_latest, aes(x = time_value)) +
  geom_line(aes(y = value, color = "adjusted"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = `% WEIGHTED ILI`, color = "raw"), linetype = "dashed", size = 0.5) +
  scale_color_manual(name = "Legend", values = c("raw" = "orange", "adjusted" = "blue")) +
  labs(x = "Time", y = "ILI+ Rate", title = "Raw ILI Rate vs Adjusted ILI+ Rate over Time") +
  theme_minimal()



# some revision stats:
rev_sum <- flusurv_adjusted %>%
  as_epi_archive(compactify = TRUE) %>%
  revision_summary(adj_hosp_rate)
rev_sum %>%
  filter(time_value > "2020-01-01") %>%
  pull(min_lag) %>%
  max()
# there's definitely still lag in the most recent version
rev_sum %>%
  filter(time_value > "2020-01-01") %>%
  pull(max_lag) %>%
  median()

count_single_column <- function(col) {
  max(which(!is.na(col)))
}


seq_null_swap <- function(from, to, by) {
  if (from > to) {
    return(NULL)
  }
  seq(from = from, to = to, by = by)
}


#' fill in values between, and divide any numeric values equally between them
#' @param to_complete epi_archive
#' @param groups to be grouped by. Should include both time_value and version, and any epi_keys
#' @param columns_to_complete any columns that need their values extended
#' @param aggregate_columns any columns which have numerical data that is a sum
#'   across days, and thus needs to be divided into equal parts distributed
#'   accross days
convert_to_period <- function(to_complete, groups, columns_to_complete,
                              aggregate_columns, source_period = 7, target_period = 1) {
  to_complete_datatable <- to_complete$DT
  completed_time_values <-
    to_complete_datatable %>%
    group_by(across(all_of(groups))) %>%
    reframe(
      time_value = seq(from = time_value, to = time_value + source_period - 1, by = target_period)
    ) %>%
    unique()
  completed <- to_complete_datatable %>%
    full_join(
      completed_time_values,
      by = join_by(season, geo_value, version, time_value)
    ) %>%
    arrange(geo_value, version, time_value) %>%
    fill(all_of(columns_to_complete), .direction = "down") %>%
    mutate(across(all_of(aggregate_columns), \(x) x / 7))
  completed %>% arrange(geo_value, time_value) %>% as_epi_archive(compactify = TRUE)
}


nhsn_final <- nhsn %>%
  select(geo_value, time_value, version, agg_level, value = admission_rate, season, season_week) %>%
  mutate(source = "nhsn")

flusurv_final <-
  flusurv_adjusted %>%
  convert_to_period(
    .,
    c("season", "geo_value", "version", "time_value"),
    setdiff(names(.$DT), c("season", "geo_value", "version", "time_value")),
    c("hosp_rate", "adj_hosp_rate"))
flusurv_final <- flusurv_final$DT %>%
  mutate(source = "flusurv") %>%
  select(geo_value, time_value, version, value = adj_hosp_rate, source, agg_level, season, season_week)

ili_final <-
  ili_plus %>%
  convert_to_period(
    .,
    c("season", "geo_value", "version", "time_value"),
    setdiff(names(.$DT), c("season", "geo_value", "version", "time_value")),
    NULL)
ili_final <- ili_final$DT %>% select(geo_value, time_value, version, value, source, agg_level, season, season_week)


flusion_merged <- bind_rows(ili_final, flusurv_final, nhsn_final) %>% as_epi_archive(compactify = TRUE, other_keys = "source")
flusion_merged

flusion_merged_latest <- flusion_merged %>%
  epix_as_of(max_version = max(.$DT$version))

# NHSN
ggplot(filter(flusion_merged_latest, agg_level == "nation", source == "nhsn"),
       aes(x = time_value, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Admission Rate Over Time", x = "Time", y = "Admission Rate") +
  theme_minimal()

# FluSurv
ggplot(filter(flusion_merged_latest, agg_level == "nation", source == "flusurv"),
       aes(x = time_value, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Admission Rate Over Time", x = "Time", y = "Admission Rate") +
  theme_minimal()
# ILI+
ggplot(filter(flusion_merged_latest, agg_level == "nation", source == "ILI+"),
       aes(x = time_value, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Admission Rate Over Time", x = "Time", y = "Admission Rate") +
  theme_minimal()


flusion_merged %>% saveRDS("aux_data")
