G3

relevant_nowcasts  <-
nhsn_snapshot |>
  left_join(nowcasts)


patch_boundary <- function(snapshot, nowcasts_at_d, n_lags = 2L) {
  latest_time <- max(snapshot$time_value)
  snapshot |>
    left_join(nowcasts_at_d, by = c("geo_value", "time_value")) |>
    mutate(
      value = if_else(
        time_value >= latest_time - weeks(n_lags) & !is.na(nowcast_median),
        nowcast_median,
        value
      )
    )
}

training_snapshots <- map(training_dates, \(d) {
  patch_boundary(
    epix_as_of(nhsn_archive, d),
    retro_nowcasts |> filter(nowcast_date == d) |>
    select(geo_value, time_value = reference_date, nowcast_median = median)
  )
}) |> list_rbind()


as_of_lags <- nhsn_archive |>
  mutate(lag_days = as.integer(difftime(nowcast_date, reference_date, units = "days"))) |>
  # need a new function to guarantee that these rows are actually present
  archive_fill_lags(lag_days) |>
  pivot_wider(
    id_cols = c(geo_value, nowcast_date),
    names_from = lag_days,
    names_prefix = "lag_",
    values_from = median
  ) |>
  rename(time_value = nowcast_date)

# y: outcome is one of the variants below
y_values <- nhsn_archive |>
  epix_as_of(forecast_date) |>
  select(geo_value, time_value, y = value)





# forecast_date: single Date; ahead: single integer
current_snapshot <- epix_as_of(nhsn_archive, forecast_date)

latest_nowcast <- retro_nowcasts |>
  filter(nowcast_date == forecast_date) |>
  select(geo_value, time_value = reference_date, nowcast_median = median)

corrected_snapshot <- current_snapshot |>
  left_join(latest_nowcast, by = c("geo_value", "time_value")) |>
  mutate(value = coalesce(nowcast_median, value)) |>
  select(-nowcast_median)

scaled_pop(
  epi_data = corrected_snapshot,
  outcome = "value",
  ahead = ahead
)
