source(here::here("R", "load_all.R"))
flusion_merged <- qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
  filter(geo_value != "as") %>%
  as_epi_archive(other_keys = "source", compactify = TRUE)
flusion_merged_nonprop <- qs::qread(here::here("aux_data/flusion_data/flusion_merged_nonprop")) %>%
  filter(geo_value != "as") %>%
  as_epi_archive(other_keys = "source", compactify = TRUE)
umass_flusion_predict <- read_csv(here::here("scripts/one_offs/2024-02-17-UMass-flusion.csv"))

umass_location_names <- read_csv(here::here("scripts/one_offs/flusion_model_location_naming.csv"))
umass_location_names
umass_reformat <- umass_flusion_predict %>%
  left_join(umass_location_names, by = "location") %>%
  mutate(value = value / population * 100000) %>%
  select(abbreviation, target_end_date, output_type_id, value) %>%
  rename(geo_value = abbreviation, quantile = output_type_id) %>%
  mutate(geo_value = tolower(geo_value), target_end_date = as.Date(target_end_date) - 3)
umass_reformat

# flusion's version of the values
flusion_merged_nonprop %>%
  epix_as_of(flusion_merged_nonprop$versions_end) %>%
  filter(time_value == "2024-02-21", source == "nhsn", !(geo_value %in% as.character(1:10)))

debugonce(flusion)

res
# nhsn's version of the values
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
nhsn_nation <- pub_covidcast(
  source = "hhs",
  signals = "confirmed_admissions_influenza_1d_prop",
  time_type = "day",
  geo_type = "nation",
  issues = epirange(20000101, 20251231)
) %>%
  select(geo_value, time_value, admission_rate = value, version = issue) %>%
  mutate(agg_level = "nation")
nhsn <- bind_rows(nhsn_state, nhsn_nation) %>% as_epi_archive(compactify = TRUE)
finalized_values <- nhsn %>%
  epix_as_of(nhsn$versions_end) %>%
  group_by(geo_value) %>%
  epi_slide_sum(admission_rate, .window_size = 7L) %>%
  drop_na() %>%
  filter(time_value == "2024-02-21") %>%
  select(geo_value, time_value, `value.true` = slide_value_admission_rate)

# running our version of the flusion model
very_latent_locations <- list(
  geo_value = c("ak", "dc", "de", "id", "me", "nh", "nv", "nd", "nc", "ut", "wy"),
  source = c("flusurv", "ILI+")
)
debugonce(run_workflow_and_format)
flusion_merged_nonprop$DT %>% filter(source == "nhsn", time_value == "2024-02-14")
flusion_merged$DT %>% filter(source == "nhsn", time_value == "2024-02-14")
res_nonprop <- flusion_merged_nonprop %>%
  epix_as_of(as.Date("2024-02-14")) %>%
  flusion("value", ahead = 7, keys_to_ignore = very_latent_locations)
res_nonprop_adj <- res_nonprop %>%
  filter(!(geo_value %in% as.character(1:10))) %>%
  left_join(umass_location_names %>% mutate(geo_value = tolower(abbreviation)), by = "geo_value") %>%
  mutate(value = value / population * 100000) %>%
  select(geo_value, source, forecast_date, target_end_date, quantile, value)
res <- flusion_merged %>%
  epix_as_of(as.Date("2024-02-14")) %>%
  flusion("value", ahead = 7, keys_to_ignore = very_latent_locations)
res_full <- flusion_merged %>%
  epix_as_of(as.Date("2024-02-14")) %>%
  flusion("value", ahead = 7, keys_to_ignore = very_latent_locations, derivative_estimator = "quadratic_regression")

# comparing with umass and the actual value
comp_table <- res %>%
  left_join(umass_reformat, by = join_by(geo_value, target_end_date, quantile), suffix = c(".prop", ".theirs")) %>%
  left_join(res_nonprop_adj, by = join_by(geo_value, source, forecast_date, target_end_date, quantile)) %>%
  rename(value.nonprop = value) %>%
  left_join(res_full, by = join_by(geo_value, source, forecast_date, target_end_date, quantile)) %>%
  rename(value.quad_reg = value) %>%
  select(-forecast_date) %>%
  filter(!is.na(value.theirs)) %>%
  left_join(finalized_values, by = join_by(geo_value, target_end_date == time_value))
comp_table %>%
  pivot_longer(cols = c("value.prop", "value.nonprop", "value.quad_reg", "value.theirs")) %>%
  filter(quantile == 0.5) %>%
  ggplot(aes(x = value.true, y = value, color = name)) +
  geom_point() +
  geom_abline()

scores <-
  comp_table %>%
  pivot_longer(cols = c("value.prop", "value.nonprop", "value.quad_reg", "value.theirs")) %>%
  rename(true_value = value.true, prediction = value, model = name) %>%
  group_by(geo_value) %>%
  check_forecasts() %>%
  scoringutils::score()
scores %>% as_tibble()
scores %>%
  ggplot(aes(x = model, y = interval_score)) +
  geom_boxplot()
ggsave(here::here("scripts/one_offs/comparing_various_Feb.png"))
scores %>% filter(!is.na(interval_score))
