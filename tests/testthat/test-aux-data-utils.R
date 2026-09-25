## source(here::here("R", "load_all.R"))
## tribble(
##   ~geo_value, ~time_value, ~version, ~value,
##   "us", "2024-11-09", "2024-11-09", 3,
##   "us", "2024-11-15", "2024-11-16", 2,
##   "us", "2024-11-02", "2024-11-02", 5,
##   "us", "2024-11-08", "2024-11-09", 4,
##   "us", "2024-11-01", "2024-11-02", 6,
##   "us", "2024-11-16", "2024-11-16", 1,
##   ) %>%
##   mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
##   as_epi_archive(.versions_end = as.Date("2024-11-16"), compactify = TRUE) %>%
##   daily_to_weekly_archive("value")

## tribble(
##   ~geo_value, ~time_value, ~version, ~value,
##   "us", "2024-11-13", "2024-11-13", 11,
##   "us", "2024-11-12", "2024-11-13", 10,
##   "us", "2024-11-11", "2024-11-13", 9,
##   "us", "2024-11-10", "2024-11-13", 8,
##   "us", "2024-11-09", "2024-11-13", 7,
##   "us", "2024-11-08", "2024-11-13", 1,
##   "us", "2024-11-07", "2024-11-13", 2,
##   "us", "2024-11-06", "2024-11-13", 3,
##   "us", "2024-11-05", "2024-11-13", 4,
##   "us", "2024-11-04", "2024-11-13", 5,
##   "us", "2024-11-03", "2024-11-13", 6,
## ) %>%
##   mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
##   as_epi_archive(.versions_end = as.Date("2024-11-13"), compactify = TRUE) %>%
##   daily_to_weekly_archive("value")


## nhsn_state <- pub_covidcast(
##   source = "hhs",
##   signals = "confirmed_admissions_influenza_1d_prop",
##   time_type = "day",
##   geo_type = "state",
##   time_values = epirange(10990101, 20990101),
##   issues = epirange(20000101, 20251231)
## ) %>%
##   select(geo_value, time_value, admission_rate = value, version = issue) %>%
##   mutate(agg_level = "state")

## nhsn_weekly <- nhsn_state %>% as_epi_archive(compactify = TRUE) %>% daily_to_weekly_archive("admission_rate")


## nhsn_state_weekly_sum <- nhsn_state %>%
##   as_epi_archive(compactify = TRUE) %>%
##   epix_as_of(as.Date("2024-05-04")) %>%
##   filter(!is.na(admission_rate)) %>%
##   left_join(
##     targets::tar_read(evaluation_data, store = "flu_hosp_explore") %>% distinct(geo_value, population)
##   ) %>%
##   as_epi_df() %>%
##   group_by(geo_value) %>%
##   epi_slide_sum(admission_rate, .window_size = 7) %>%
##   mutate(admission_rate = slide_value_admission_rate,
##          time_value = time_value - 3)

## nhsn_weekly %>%
##   epix_as_of(nhsn_weekly$versions_end) %>%
##   left_join(
##     nhsn_state_weekly_sum,
##     by = c("geo_value", "time_value" = "time_value")
##   ) %>%
##   rename(
##     new_sum = admission_rate.x,
##     daily_to_weekly_archive = admission_rate.y
##   ) %>%
##   pivot_longer(
##     cols = c(new_sum, daily_to_weekly_archive),
##     names_to = "source"
##   ) %>%
##   filter(geo_value == "dc") %>%
##   ggplot(aes(x = time_value, y = value)) +
##   geom_line(aes(y = value, color = source)) +
##   labs(title = "HHS vs NHSN") +
##   theme_minimal()
