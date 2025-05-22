# On flu ------------------------------------------------------------------
library(epidatr)
library(tidyverse)
library(epipredict)
source(here::here("R", "load_all.R"))
httpgd::hgd()

episeason <- function(time_value) {
  time_value <- time_value - lubridate::dmonths(6)
  paste0(
    strftime(time_value, "%Y"),
    "/",
    strftime(time_value + lubridate::years(1), "%y")
  )
}

season_week <- function(time_value, season_start_epiweek = 39) {
  stopifnot(season_start_epiweek >= 0L, season_start_epiweek <= 53L)
  time_value <- time_value - lubridate::weeks(season_start_epiweek)
  lubridate::epiweek(time_value)
}


flu <- pub_covidcast(
  "hhs",
  "confirmed_admissions_influenza_1d_prop_7dav",
  geo_type = "state",
  time_type = "day",
  time_values = epirange(20220701, 20240701)
)
wflu <- flu |>
  select(geo_value, time_value, value) |>
  filter(
    lubridate::wday(time_value, week_start = 7L) == 7L,
    !(geo_value %in% c("as", "vi"))
  ) |>
  mutate(
    value = value * 7,
    sw = season_week(time_value),
    season = episeason(time_value)
  )
X <- wflu |>
  filter(season == "2022/23") |>
  mutate(value = value^(1 / 4)) |>
  select(geo_value, sw, value) |>
  mutate(value = case_when(value <= 0 ~ NA, TRUE ~ value)) |>
  pivot_wider(names_from = geo_value, values_from = value) |>
  fill(-sw, .direction = "downup")

Xmat <- as.matrix(X[, -1])
mypc25 <- prcomp(Xmat^(1 / 4))
mypc25_two <- tibble(
  season_week = 1:52, PC1 = predict(mypc25)[, 1],
  PC2 = predict(mypc25)[, 2]
)

wflupc <- wflu |>
  mutate(ww = week(time_value - dmonths(6))) |>
  left_join(mypc25_two, by = join_by(ww == season_week)) |>
  filter(!is.na(PC1), !is.na(PC2)) |>
  select(geo_value, time_value, value, PC1, PC2)

fds <- seq(from = ymd("2023-10-07"), to = ymd("2024-03-30"), by = "1 month")

forecaster <- function(alldata, fd, ahead = 7, use_pc = FALSE) {
  if (use_pc) {
    pcs <- alldata |>
      select(geo_value, time_value, starts_with("PC")) |>
      mutate(time_value = time_value - days(ahead))
    alldata <- alldata |>
      select(-starts_with("PC")) |>
      left_join(pcs, by = join_by(geo_value, time_value))
  }
  training <- alldata |>
    filter(time_value <= fd) |>
    mutate(value = pmax(value^(1 / 4), 0))
  rec <- epi_recipe(training) |>
    step_epi_lag(value, lag = c(0, 7, 14)) |>
    step_epi_ahead(value, ahead = ahead)

  if (use_pc) {
    rec <- rec |> step_epi_lag(PC1, PC2, lag = 0)
  }
  rec <- rec |> step_epi_naomit()

  f <- frosting() |>
    layer_predict() |>
    layer_quantile_distn(quantile_levels = c(.1, .25, .5, .75, .9)) |>
    layer_threshold(starts_with(".pred"))

  ewf <- epi_workflow(
    rec,
    quantile_reg(quantile_levels = c(.1, .25, .5, .75, .9)),
    f
  )
  ewf <- ewf |> fit(training)

  # Debug
  cols <- ewf %>%
    extract_fit_engine() %>%
    coefficients() %>%
    row.names() %>%
    setdiff("(Intercept)")
  v <- ewf %>%
    extract_fit_engine() %>%
    coefficients() %>%
    as_tibble() %>%
    pull(`tau= 0.50`)
  X <- rec %>%
    prep(training) %>%
    bake(training) %>%
    filter(time_value == max(time_value), geo_value == "ca") %>%
    select(all_of(cols)) %>%
    mutate(intercept = 1) %>%
    relocate(intercept) %>%
    as.matrix()
  prediction_components <- sweep(X, 2, v, "*")
  prediction_components
  # End Debug

  # preds <- forecast(ewf) |> mutate(forecast_date = fd, target_date = fd + ahead)
  # preds |>
  #   select(geo_value, forecast_date, target_date, .pred_distn) |>
  #   as_tibble()
}


with_pc <- list()
no_pc <- list()
epipredict_seasonal <- list()
aheads <- c(7, 14, 21, 28)
all_data <- wflupc |> as_epi_df(as_of = max(wflupc$time_value))
for (ii in seq_along(fds)) {
  fd <- fds[ii]
  with_pc[[ii]] <- map(aheads, ~ forecaster(all_data, fd, .x, TRUE))
  # no_pc[[ii]] <- map(aheads, ~ forecaster(all_data, fd, .x, FALSE)) |> list_rbind()
  # epipredict_seasonal[[ii]] <- map(
  #   aheads,
  #   function(x) {
  #     training <- all_data |>
  #       select(-starts_with("PC")) |>
  #       left_join(
  #         all_data |>
  #           select(geo_value, time_value, starts_with("PC")) |>
  #           mutate(time_value = time_value - days(x)),
  #         by = join_by(geo_value, time_value)
  #       ) |>
  #       filter(time_value <= fd) |>
  #       mutate(value = pmax(value^(1 / 4), 0)) %>%
  #       # select(-PC1, -PC2) %>%
  #       as_tibble() %>%
  #       as_epi_df(as_of = max(.$time_value))
  #     scaled_pop_seasonal(
  #       training,
  #       "value",
  #       ahead = x,
  #       seasonal_method = "flu",
  #       pop_scaling = FALSE,
  #       scale_method = "none"
  #     )
  #   }
  # ) |> list_rbind()
}

map(with_pc, function(x) {
  map(x, ~ as_tibble(.x)) |> bind_rows()
}) %>%
  bind_rows() %>%
  mutate(across(everything(), abs)) %>%
  mutate(s = rowSums(.)) %>%
  mutate(across(everything(), ~ .x / s)) %>%
  summarize(across(everything(), list(median)))

map(with_pc, function(x) {
  map(x, ~ as_tibble(.x)) |> bind_rows()
}) %>%
  bind_rows() %>%
  mutate(s = rowSums(.)) %>%
  mutate(across(everything(), ~ .x / s)) %>%
  filter(near(lag_0_PC1, 0.373, tol = 0.1))


with_pc_df <- list_rbind(with_pc)
no_pc_df <- list_rbind(no_pc)
epipredict_seasonal_df <- list_rbind(epipredict_seasonal) %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  select(geo_value, forecast_date, target_end_date, `0.1`, `0.25`, `0.5`, `0.75`, `0.9`) %>%
  mutate(forecaster = "epipredict_seasonal") %>%
  rename(target_date = target_end_date)
all_forecasts <- bind_rows(with_pc = with_pc_df, no_pc = no_pc_df, .id = "forecaster") |>
  pivot_quantiles_wider(.pred_distn) |>
  bind_rows(epipredict_seasonal_df) |>
  mutate(across(`0.1`:`0.9`, ~ .x^4))

# Plotting them -----------------------------------------------------------

geos <- c("ca", "ga", "sd", "or")

ggplot(
  data = all_forecasts |> filter(geo_value %in% geos),
  aes(x = target_date, group = interaction(forecaster, forecast_date))
) +
  geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`, fill = forecaster), alpha = 0.2) +
  geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`, fill = forecaster), alpha = 0.4) +
  geom_line(aes(y = `0.5`, color = forecaster)) +
  geom_point(aes(y = `0.5`, color = forecaster), size = 0.75) +
  geom_line(
    data = wflu %>% filter(geo_value %in% geos),
    aes(x = time_value, y = value),
    inherit.aes = FALSE, na.rm = TRUE,
    color = "black", linetype = 1
  ) +
  coord_cartesian(xlim = ymd(c("2023-07-01", NA))) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~geo_value, scales = "free_y") +
  labs(x = "Reference Date", y = "Weekly Sums of Hospitalizations", title = "Monthly Forecasts and Truth Data") +
  theme(legend.position = "bottom") +
  theme_bw()
