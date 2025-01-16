source(here::here("R", "load_all.R"))

data <- tribble(
  ~geo_value, ~time_value, ~version, ~value1,
  "us", "2024-11-08", "2024-11-13", 1,
  "us", "2024-11-07", "2024-11-13", 2,
  "us", "2024-11-06", "2024-11-13", 3,
  "us", "2024-11-05", "2024-11-13", 4,
  "us", "2024-11-04", "2024-11-13", 5,
  "us", "2024-11-03", "2024-11-13", 6,
  "us", "2024-11-02", "2024-11-13", 7,
  "us", "2024-11-01", "2024-11-13", 8,
  "us", "2024-10-31", "2024-11-13", 9,
  "us", "2024-10-30", "2024-11-13", 10,
  "us", "2024-10-29", "2024-11-13", 11,
  "us", "2024-10-28", "2024-11-13", 12,
  "us", "2024-10-27", "2024-11-13", 13
) %>%
  mutate(value2 = value1 * 11) %>%
  bind_rows((.) %>% mutate(geo_value = "ca", value1 = value1 * 3 + 1, value2 = value2 + 50)) %>%
  mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
  as_epi_df()

r <- epi_recipe(data) %>%
  step_YeoJohnson2(value1, value2) %>%
  prep(data)
r
r$steps[[1]]$lambdas
outcome <- r %>% bake(data)

httpgd::hgd()
data %>%
  pivot_longer(c(value1, value2), names_to = "variable", values_to = "value") %>%
  ggplot(aes(time_value, value, color = variable)) +
  geom_line() +
  geom_line(
    data = outcome %>% pivot_longer(c(value1, value2), names_to = "variable", values_to = "value"),
    aes(time_value, value, color = variable),
  ) +
  facet_wrap(~geo_value, scales = "free_y") +
  theme_minimal() +
  labs(title = "Yeo-Johnson transformation", x = "Time", y = "Value")


data <- tar_read(joined_archive_data) %>%
  epix_as_of(as.Date("2023-11-01")) %>%
  filter(source == "nhsn") %>%
  rename(value = hhs)
r <- epi_recipe(data) %>%
  step_YeoJohnson2(value) %>%
  prep(data)
r
r$steps[[1]]$lambdas %>% print(n = 55)
outcome <- r %>% bake(data)

httpgd::hgd()
data %>%
  ggplot(aes(time_value, value)) +
  geom_line(color = "blue") +
  geom_line(data = outcome, aes(time_value, value), color = "green") +
  facet_wrap(~geo_value, scales = "free_y") +
  theme_minimal() +
  labs(title = "Yeo-Johnson transformation", x = "Time", y = "Value")


# TODO: Test this.
## Layer Yeo-Johnson2
postproc <- frosting() %>%
  layer_YeoJohnson2()

wf <- epi_workflow(r, linear_reg()) %>%
  fit(data) %>%
  add_frosting(postproc)
