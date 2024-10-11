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
library(readr)
# TODO consider switching this to https://github.com/brookslogan/epicalendar
library(MMWRweek)
library(rlang)
library(vctrs)
library(testthat)
# which day of the week should represent that mmwr week?
default_day_of_week <- 1



exhaustive <- expand_grid(year = seq(2000, 2023, by = 1), week = seq(1, 53, by = 1))
full_list <- convert_epiweek_to_season_week(exhaustive$year, exhaustive$week)
# check that this long list is basically sequential other than jumps down across year boundaries
expect_equal(
  full_list %>%
    diff() %>% unique(),
  c(1, -51, 0, -52)
)
# check that its all positive
expect_true(all(full_list > 0))
convert_epiweek_to_date <- function(epiyear, epiweek,
                                    day_of_week = default_day_of_week) {
  end_date <- MMWRweek2Date(epiyear, epiweek, day_of_week)

  return(end_date)
}



################# HHS ##########################
# this is more or less redundant
################# HHS ##########################
nhsn_state <- pub_covidcast(
  source = "hhs",
  signals = "confirmed_admissions_influenza_1d_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(10990101, 20990101),
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
  mutate(season_week = convert_epiweek_to_season_week(epiyear, epiweek)) # %>%
# mutate(week_end_date = convert_epiweek_to_week_end_date(epiyear, epiweek))

# are there any season_weeks with more than 7 days? no
expect_equal(
  nhsn %>% group_by(geo_value, version, season, season_week) %>% count() %>% pull(n) %>% max(),
  7
)
# is the start of every season week a Sunday?
expect_equal(
  nhsn %>% group_by(geo_value, version, season, season_week) %>% summarize(first_day = wday(min(time_value), label = TRUE), n = n()) %>% filter(n == 7) %>% pull(first_day) %>% unique(),
  wday("2020-12-06", label = TRUE)
)

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

flusurv_adjusted <- generate_flusurv_adjusted()
# everything from flusurv is on sunday
expect_equal(flusurv_adjusted$DT$time_value %>% wday() %>% unique(), 1)
flusurv_adjusted$DT %>%
  group_by(season) %>%
  filter(epiyear == min(epiyear)) %>%
  summarize(minn = min(season_week), maxx = max(season_week)) %>%
  summarize(max(minn), min(maxx))

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
  mutate(
    season = convert_epiweek_to_season(epiyear, epiweek),
    season_week = convert_epiweek_to_season_week(epiyear, epiweek),
    time_value = MMWRweek2Date(epiyear, epiweek, default_day_of_week),
    version = time_value
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
  as_epi_archive(compactify = TRUE)

# is ili_plus always on a Sunday?
expect_equal(ili_plus$DT$time_value %>% wday() %>% unique(), 1)

# is the seasonweek actually starting at the right time? yes, the season always starts at 1, and not week 52, or week 2
ili_plus$DT %>%
  group_by(season) %>%
  filter(epiyear == min(epiyear)) %>%
  summarize(minn = min(season_week), maxx = max(season_week)) %>%
  summarize(max(minn), min(maxx))

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

#' convert an archive from daily data to weekly data, summing where appropriate
#' @details
#' this function is slow, so make sure you are calling it correctly, and
#'   consider testing it on a small portion of your archive first
#' @param day_of_week integer, day of the week, starting from Sunday, select the
#'   date to represent the week in the time_value column, based on it's
#'   corresponding day of the week. The default value represents the week using
#'   Wednesday.
#' @param day_of_week_end integer, day of the week starting on Sunday.
#'   Represents the last day, so the week consists of data summed to this day.
#'   The default value `6` means that the week is summed from Sunday through
#'   Saturday.
daily_to_weekly <- function(epi_arch,
                            agg_columns,
                            agg_method = c("total", "mean", "median"),
                            day_of_week = 4L,
                            day_of_week_end = 6L) {
  keys <- key_colnames(epi_arch, exclude = "time_value")
  too_many_tibbles <- epix_slide(
    epi_arch,
    before = 99999999L,
    ref_time_values = ref_time_values,
    function(x, group, ref_time) {
      x %>%
        group_by(across(all_of(keys))) %>%
        epi_slide_sum(agg_columns, .window_size = 5L) %>%
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
    rename(version = time_value) %>%
    rename_with(~ gsub("slide_value_", "", .x)) %>%
    as_epi_archive(compactify = TRUE)
}

nhsn_weekly <- nhsn %>%
  as_epi_archive(compactify = TRUE) %>%
  daily_to_weekly("admission_rate")

nhsn_final <- nhsn_weekly$DT %>%
  select(geo_value, time_value, version, agg_level, value = admission_rate, season, season_week) %>%
  mutate(source = "nhsn")

flusurv_final <-
  flusurv_adjusted$DT %>%
  mutate(time_value = time_value + 3) %>%
  mutate(source = "flusurv") %>%
  select(geo_value, time_value, version, value = adj_hosp_rate, source, agg_level, season, season_week)

ili_final <-
  ili_plus$DT %>%
  mutate(time_value = time_value + 3) %>%
  select(geo_value, time_value, version, value, source, agg_level, season, season_week)


flusion_merged <- bind_rows(ili_final, flusurv_final, nhsn_final) %>% as_epi_archive(compactify = TRUE, other_keys = "source")

flusion_merged_latest <- flusion_merged %>%
  epix_as_of(max_version = max(.$DT$version))

# NHSN
ggplot(
  filter(flusion_merged_latest, agg_level == "nation", source == "nhsn"),
  aes(x = time_value, y = value)
) +
  geom_line(color = "blue") +
  labs(title = "Weekly Admission Rate Over Time", x = "Time", y = "Admission Rate") +
  theme_minimal()

# FluSurv
ggplot(
  filter(flusion_merged_latest, agg_level == "nation", source == "flusurv"),
  aes(x = time_value, y = value)
) +
  geom_line(color = "blue") +
  labs(title = "Weekly Admission Rate Over Time", x = "Time", y = "Admission Rate") +
  theme_minimal()
# ILI+
ggplot(
  filter(flusion_merged_latest, agg_level == "nation", source == "ILI+"),
  aes(x = time_value, y = value)
) +
  geom_line(color = "blue") +
  labs(title = "Weekly Admission Rate Over Time", x = "Time", y = "Admission Rate") +
  theme_minimal()

flusion_merged

# adding population and density data

flusion_merged <-
  flusion_merged$DT %>%
  add_pop_and_density()



flusion_merged %>% qs::qsave(here::here("aux_data/flusion_data/flusion_merged"))
source(here::here("R", "load_all.R"))
# dropping "as" (American Samoa) because the data is terrible; there's a single point that's 80 and the rest are zero
flusion_merged <- qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
  filter(geo_value != "as") %>%
  as_epi_archive(other_keys = "source", compactify = TRUE)
options(error=NULL)
# given that 2020 is the earliest we have nhsn, let's just drop any versions
# older than that to avoid making computing the quantiles difficult
epi_data <- flusion_merged %>%
  epix_as_of(as.Date("2022-10-26"))
flusion_merged$DT %>% filter(!is.na(value)) %>% group_by(source) %>% count()
flusion_merged$DT %>% filter(time_value == "2022-10-26")
flusion_merged %>%
  epix_as_of(as.Date("2022-10-26")) %>% filter(!is.na(value), source=="flusurv")
epi_data %>% filter(!is.na(value), source =="flusurv")
last_value <- flusion_merged %>%
  epix_as_of(as.Date("2024-01-01")) %>%
  filter(time_value =="2022-10-26")
true_value <- flusion_merged %>%
  epix_as_of(as.Date("2024-01-01"))
source(here::here("R", "load_all.R"))
pred_final <- flusion(epi_data, "value", adjust_latency = "extend_lags", derivative_estimator = "growth_rate")

# various plots used after
ggplot(epi_data, aes(x=time_value, y=value, color = source)) + geom_line()
ggplot(full_data, aes(x=time_value, y=value, color = source)) + geom_line()
ggplot(season_data, aes(x=time_value, y=value, color = geo_value)) + geom_line() + facet_wrap(~source,ncol=1)
ggplot(full_data, aes(x=time_value, y=value, color = geo_value)) + geom_line() + facet_wrap(~source,ncol=1, scales="free_y")
full_data %>% filter(geo_value == "as") %>% ggplot(aes(x=time_value, y = value))
ggplot(season_data %>% filter(year > 2019), aes(x=time_value, y=value, color = geo_value)) + geom_line() + facet_wrap(~source,ncol=1, scales="free_y")
# end various plots

pred_final %>%
    filter(!is.na(value)) %>%
    pull(source) %>%
    unique()
true_value %>% filter(source =="nhsn") %>% pull(value) %>% median
pred_final
pred_final %>% filter(quantile == .5) %>% left_join(
    true_value %>%
      select(-agg_level, -season, -season_week, -year, -population, -density),
    by = join_by(geo_value, source, target_end_date == time_value)
  ) %>% ggplot(aes(x = value.x, y = value.y, color = source)) + geom_point() + geom_abline()

calculate

pred_final %>%
  filter(quantile == .5) %>%
  left_join(
    true_value %>%
      select(-agg_level, -season, -season_week, -year, -population, -density),
    by = join_by(geo_value, source, forecast_date == time_value)
  )


pred_final %>%
  left_join(
    true_value %>%
      select(-agg_level, -season, -season_week, -year, -population, -density),
    by = join_by(geo_value, source, target_end_date == time_value)
  ) %>%
  rename(prediction = value.x, true_value = value.y, model = source) %>%
  arrange(geo_value, model, forecast_date, target_end_date, quantile) %>%
  scoringutils::score(metrics = c("interval_score", "ae_median")) %>%
  scoringutils::summarize_scores(across = c("geo_value"))
pred_final %>%
  left_join(true_value %>% select(-agg_level, -season, -season_week, -year, -population, -density), by = join_by(geo_value, source, target_end_date == time_value)) %>%
  mutate(off_by = value.x - value.y, score = abs(0.5 - quantile) * abs(off_by) / value.y) %>%
  group_by(geo_value, source, forecast_date, target_end_date) %>%
  summarize(net_score = sum(score)) %>%
  arrange(net_score)
# round 2
source(here::here("R", "load_all.R"))
epi_data2 <- flusion_merged %>%
  epix_as_of(as.Date("2024-01-03"))
epi_data2
flusion_merged$DT %>% filter(time_value > "2024-01-01")
true_value2 <- flusion_merged %>%
  epix_as_of(as.Date("2024-07-05")) %>%
  filter(time_value == "2024-01-10")
pred_final2 <- flusion(epi_data2, "value")
pred_final2
pred_final2 %>%
  left_join(true_value2 %>% select(-agg_level, -season, -season_week, -year, -population, -density), by = join_by(geo_value, source, target_end_date == time_value)) %>%
  mutate(off_by = value.x - value.y, score = abs(0.5 - quantile) * abs(off_by) / value.y) %>%
  select(-value.x, -value_center, -value_scale) %>%
  group_by(geo_value, source, forecast_date, target_end_date) %>%
  summarize(net_score = sum(score)) %>%
  arrange(net_score)
pred_final2 %>%
  pull(source) %>%
  unique()
pred_final2 %>%
  left_join(
    true_value2 %>%
      select(-agg_level, -season, -season_week, -year, -population, -density),
    by = join_by(geo_value, source, target_end_date == time_value)
  ) %>%
  rename(prediction = value.x, true_value = value.y, model = source) %>%
  scoringutils::score(metrics = c("interval_score", "ae_median")) %>%
  scoringutils::summarize_scores(across = c("geo_value"))
true_value2 %>%
  select(-agg_level, -season, -season_week, -year, -population, -density) %>%
  group_by(source, time_value) %>%
  drop_na() %>%
  summarise(min = min(value), max = max(value), mean = mean(value), median = median(value))
pred_final2 %>% filter(quantile==0.5) %>% arrange(desc(value))
pred_final2 %>% arrange(desc(value))
epi_data2 %>% arrange(desc(value))
true_value2 %>% filter(`source` == "ILI+") %>% arrange(desc(value))
true_value2 %>%
   %>%
  pull(value) %>%
  summary()
outcome() <- "value"
extra_sources <- ""
ahead <- 7
pop_scaling <- FALSE
trainer <- rand_forest(engine = "grf_quantiles", mode = "regression")
quantile_levels <- covidhub_probs()
scale_method <- c("quantile")
center_method <- c("median")
sources_to_pop_scale <- c()
args_input <- list()

epi_data2$time_value %>% max()
epi_data2 %>%
  filter(source == "flusurv") %>%
  pull(value) %>%
  summary()
epi_data2 %>%
  filter(source == "nhsn") %>%
  pull(value) %>%
  summary()
epi_data2 %>%
  filter(source == "ILI+") %>%
  pull(value) %>%
  summary()
epi_data2 %>%
  pull(source) %>%
  unique()

# actually just moving this inside the forecaster
flusion_stabilized_quantile <- flusion_merged %>%
  as_epi_archive(other_keys = "source", compactify = TRUE) %>%
  epix_as_of(as.Date("2020-01-01")) %>%
  mutate(version = as.Date("2020-01-01")) %>%
  rbind(flusion_merged %>% filter(version > "2020-01-01")) %>%
  # only keep data actually in the season
  filter(season_week < 35) %>%
  # fourth root transform to stabilize variability
  mutate(value_transformed = (value + 0.01)^0.25) %>%
  group_by(source, geo_value) %>%
  # scale so that the 95th quantile for each source is 1
  mutate(quantile_scale_factor = quantile(value_transformed, 0.95, na.rm = TRUE)) %>%
  mutate(value_transformed = value_transformed / (quantile_scale_factor + 0.01)) %>%
  # subtract the mean for each source
  mutate(mean_value = mean(value_transformed, na.rm = TRUE)) %>%
  mutate(value_transformed = value_transformed - mean_value)

flusion_merged$DT %>% filter(season == "2021/22", season_week == 1)
library(epipredict)
epi_recipe(flusion_merged %>% epix_as_of(as.Date("2021-10-03"))) %>%
  step_mutate()

flusion_stabilized_quantile %>% select(-agg_level, -season)

flusion_merged %>% filter(source == "nhsn")

# Now I make the above transformation archive to archive
# Problem: Too big in size during operation (exceeding R's limit)
# Solution: Calculate transform factors using the latest snapshot
flusion_merged_transform_factor <- flusion_merged %>%
  as_epi_archive(compactify = TRUE, other_keys = "source") %>%
  epix_as_of(max_version = max(.$DT$version)) %>%
  mutate(value_transformed = (value + 0.01)^0.25) %>%
  mutate(
    value_transformed_in_season =
      ifelse(season_week < 10 | season_week > 45, NA, value_transformed)
  ) %>%
  group_by(source, geo_value) %>%
  mutate(
    value_transform_scale_factor =
      quantile(value_transformed_in_season, 0.95, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(value_transformed_cs = value_transformed / (value_transform_scale_factor + 0.01)) %>%
  mutate(
    value_transformed_cs_in_season =
      ifelse(season_week < 10 | season_week > 45, NA, value_transformed_cs)
  ) %>%
  group_by(source, geo_value) %>%
  mutate(
    value_transformed_center_factor =
      mean(value_transformed_cs_in_season, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(geo_value, source, value_transform_scale_factor, value_transformed_center_factor) %>%
  distinct()

flusion_final <- inner_join(flusion_merged, flusion_merged_transform_factor,
  by = c("geo_value", "source")
)

## flusion_merged_latest_transformed <- flusion_merged_latest %>%
##   mutate(value_transformed = (value + 0.01) ^ 0.25)
## # # divide by location- and source- specific 95th percentile
## flusion_merged_latest_transformed <- flusion_merged_latest_transformed %>%
##   # Calculate the scale factor for inc_trans within the season
##   mutate(value_transformed_in_season =
##            ifelse(season_week < 10 | season_week > 45, NA, value_transformed)) %>%
##   group_by(source, geo_value) %>%
##   mutate(value_transform_scale_factor = quantile(value_transformed_in_season, 0.95, na.rm = TRUE)) %>%
##   ungroup() %>%
##   # Scale inc_trans by the scale factor
##   mutate(value_transformed_cs = value_transformed / (value_transform_scale_factor + 0.01))

# center relative to location- and source- specific mean
## flusion_merged_latest_transformed <- flusion_merged_latest_transformed %>%
##   # Calculate the center factor for inc_trans_cs within the season
##   mutate(value_transformed_cs_in_season =
##            ifelse(season_week < 10 | season_week > 45, NA, value_transformed_cs)) %>%
##   group_by(source, geo_value) %>%
##   mutate(value_transformed_center_factor = mean(value_transformed_cs_in_season, na.rm = TRUE)) %>%
##   ungroup() %>%
##   # Center inc_trans_cs by subtracting the center factor
##   mutate(value_transformed_cs = value_transformed_cs - value_transformed_center_factor)

