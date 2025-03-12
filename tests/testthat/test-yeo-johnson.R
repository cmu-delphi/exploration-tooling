suppressPackageStartupMessages(source(here::here("R", "load_all.R")))


# Real data test
Sys.setenv(TAR_PROJECT = "flu_hosp_explore")


# Transform with Yeo-Johnson
data <- tar_read(joined_archive_data) %>%
  epix_as_of(as.Date("2023-11-01"))
state_geo_values <- data %>% filter(source == "nhsn") %>% pull(geo_value) %>% unique()
filtered_data <- data %>%
  filter(geo_value %in% state_geo_values) %>%
  select(geo_value, source, time_value, hhs)
r <- epi_recipe(filtered_data) %>%
  step_YeoJohnson2(hhs) %>%
  prep(filtered_data)
r
# Inspect the lambda values (a few states have default lambda = 0.25, because
# they have issues)
r$steps[[1]]$lambdas %>% print(n = 55)
out1 <- r %>% bake(filtered_data)

# Transform with manual whitening (quarter root scaling)
# learned_params <- calculate_whitening_params(filtered_data, "hhs", scale_method = "none", center_method = "none", nonlin_method = "quart_root")
out2 <- filtered_data %>%
  mutate(hhs = (hhs + 0.01)^(1 / 4))

out1 %>%
  left_join(out2, by = c("geo_value", "source", "time_value")) %>%
  mutate(hhs_diff = hhs.x - hhs.y) %>%
  ggplot(aes(time_value, hhs_diff)) +
  geom_line() +
  facet_wrap(~geo_value, scales = "free_y") +
  theme_minimal() +
  labs(title = "Yeo-Johnson transformation", x = "Time", y = "HHS")

# Plot the real data before and after transformation
geo_filter <- "ca"
filtered_data %>%
  filter(geo_value == geo_filter, source == "nhsn") %>%
  mutate(hhs = log(hhs)) %>%
  ggplot(aes(time_value, hhs)) +
  geom_line(color = "blue") +
  geom_line(data = out1 %>% filter(geo_value == geo_filter, source == "nhsn") %>% mutate(hhs = log(hhs)), aes(time_value, hhs), color = "green") +
  geom_line(data = out2 %>% filter(geo_value == geo_filter, source == "nhsn") %>% mutate(hhs = log(hhs)), aes(time_value, hhs), color = "red") +
  theme_minimal() +
  labs(title = "Yeo-Johnson transformation", x = "Time", y = "HHS")


# TODO: Test this.
## Layer Yeo-Johnson2
postproc <- frosting() %>%
  layer_YeoJohnson2()

wf <- epi_workflow(r) %>%
  fit(data) %>%
  add_frosting(postproc)
