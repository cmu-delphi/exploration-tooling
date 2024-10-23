start_date <- as.Date("2023-10-04")
end_date <- as.Date("2024-04-24")
ref_time_values <- as.Date(c("2023-10-11","2023-12-27", "2024-01-24", "2024-04-10"))
source(here::here("scripts/flu_hosp_explore.R"))
if (FALSE) {
  # the plot used to decide on the particular days chosen and shouldn't actually get run outside that context
  flusion_merged %>%
    epix_as_of(flusion_merged$versions_end) %>%
    filter(time_value >= start_date, time_value <=end_date, source == "nhsn", agg_level == "state") %>%
    ggplot(aes(x = time_value, y = value, color = geo_value)) +
    geom_line() +
    scale_x_date(breaks="1 weeks") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
