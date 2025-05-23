suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

data <- tribble(
  ~geo_value,
  ~time_value,
  ~version,
  ~value,
  "ak",
  "2024-11-08",
  "2024-11-13",
  1,
  "ak",
  "2024-11-07",
  "2024-11-13",
  2,
  "ak",
  "2024-10-08",
  "2024-11-13",
  2,
  "ak",
  "2024-10-07",
  "2024-11-13",
  2,
) %>%
  mutate(time_value = as.Date(time_value), version = as.Date(version)) %>%
  bind_rows((.) %>% mutate(geo_value = "ca", value = value * 3 + 1)) %>%
  bind_rows((.) %>% filter(geo_value == "ca") %>% mutate(time_value = time_value - 365)) %>%
  as_epi_df()

# debugonce(bake.step_epi_training_window)
epi_recipe(data) %>%
  step_epi_training_window(seasonal_backward_window = 5, seasonal_forward_window = 3, seasonal = TRUE) %>%
  prep(data) %>%
  bake(new_data = NULL)

# Seems fine
