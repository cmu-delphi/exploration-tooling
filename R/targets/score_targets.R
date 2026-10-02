get_external_forecasts <- function(external_object_name) {
  # try-catch in case that particular date is a 404
  tryCatch(
    {
      external_values <- s3read_using(
        arrow::read_parquet,
        object = external_object_name,
        bucket = "forecasting-team-data"
      )
      locations_crosswalk <- get_population_data() %>%
        select(state_id, state_code) %>%
        filter(state_id != "usa")
      external_values <- external_values %>%
        filter(output_type == "quantile") %>%
        select(target, forecaster, geo_value = location, forecast_date, target_end_date, quantile = output_type_id, value) %>%
        inner_join(locations_crosswalk, by = c("geo_value" = "state_code")) %>%
        mutate(geo_value = state_id) %>%
        select(target, forecaster, geo_value, forecast_date, target_end_date, quantile, value)
      return(external_values)
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("NoSuchKey|404|does not exist|not found", msg, ignore.case = TRUE)) {
        return(tibble(
          target = character(),
          forecaster = character(),
          geo_value = character(),
          forecast_date = as.Date(character()),
          target_end_date = as.Date(character()),
          quantile = numeric(),
          value = numeric()
        ))
      }
      stop(e)
    }
  )
}

# Hub locations score_forecasts() never scores: American Samoa, Guam, US Virgin Islands.
SCORING_EXCLUDED_LOCATIONS <- c("60", "66", "78")

# Scores quantile forecasts against latest_data with hubEvals. Returns one row per
# (forecaster, forecast_date, ahead, geo_value); geo_level is "nation" for us and
# "state" otherwise, so cross-geo means should filter to states. Aborts if a forecast
# location with truth for its target dates is neither scored nor excluded.
score_forecasts <- function(latest_data, forecasts, target, excluded_locations = SCORING_EXCLUDED_LOCATIONS) {
  if (length(forecasts) == 0) {
    return(tibble())
  }
  truth_data <-
    latest_data %>%
    select(geo_value, target_end_date = time_value, oracle_value = value) %>%
    left_join(
      get_population_data() %>%
        select(state_id, state_code),
      by = c("geo_value" = "state_id")
    ) %>%
    drop_na() %>%
    rename(location = state_code) %>%
    select(-geo_value) %>%
    mutate(
      target_end_date = round_date(target_end_date, unit = "week", week_start = 6)
    )
  # limit the forecasts to the same set of forecasting times
  max_forecast_date <-
    forecasts %>%
    group_by(forecaster) %>%
    summarize(max_forecast = max(forecast_date)) %>%
    pull(max_forecast) %>%
    min()
  forecasts_formatted <-
    forecasts[forecasts$forecast_date <= max_forecast_date, ]
  if ("target" %in% names(forecasts_formatted)) {
    forecasts_formatted <-
      forecasts_formatted[forecasts_formatted$target == target, ]
  }
  # no forecasts for that target for these forecast dates
  if (nrow(forecasts_formatted) == 0) {
    return(tibble())
  }
  # format_scoring_utils() drops geos it can't map to a hub location.
  unmapped_geos <- setdiff(unique(forecasts_formatted$geo_value), get_population_data()$state_id)
  if (length(unmapped_geos) > 0) {
    cli::cli_abort("score_forecasts: forecast geos with no hub location: {.val {unmapped_geos}}")
  }
  forecasts_formatted %<>%
    format_scoring_utils(target)
  if (target == "wk inc covid prop ed visits") {
    forecasts_formatted %<>% mutate(value = value * 100)
  }
  scores <- tryCatch(
    {
      forecasts_formatted %>%
        filter(output_type == "quantile") %>%
        filter(location %nin% excluded_locations) %>%
        hubEvals::score_model_out(
          truth_data,
          metrics = c("wis", "ae_median", "interval_coverage_50", "interval_coverage_90"),
          summarize = TRUE,
          by = c("model_id", "target", "reference_date", "location", "horizon")
        )
    },
    error = function(e) {
      if (grepl("no forecasts", rlang::cnd_message(e))) {
        return(tibble())
      } else {
        e
      }
    }
  )
  if (!is.data.frame(scores)) {
    return(scores)
  }
  # A location must be scored if truth exists (for any geo) on one of its target dates.
  expected_locations <- forecasts_formatted %>%
    filter(target_end_date %in% truth_data$target_end_date, location %nin% excluded_locations) %>%
    distinct(location) %>%
    pull()
  dropped <- setdiff(expected_locations, scores$location)
  if (length(dropped) > 0) {
    cli::cli_abort("score_forecasts: locations neither scored nor excluded: {.val {dropped}}")
  }
  if (nrow(scores) == 0) {
    return(scores)
  }
  scores <- scores %>%
    left_join(hub_location_crosswalk(), by = "location") %>%
    rename(
      forecaster = model_id,
      forecast_date = reference_date,
      ahead = horizon
    ) %>%
    select(-location) %>%
    mutate(geo_level = if_else(geo_value == "us", "nation", "state"))
  n_dup <- scores %>% count(forecaster, forecast_date, ahead, geo_value) %>% filter(n > 1) %>% nrow()
  if (n_dup > 0) {
    cli::cli_abort("score_forecasts: {n_dup} duplicate (forecaster, forecast_date, ahead, geo_value) keys")
  }
  scores
}

render_score_plot <- function(score_report_rmd, scores, forecast_dates, disease, target) {
  season_start <- format(min(forecast_dates), "%Y")
  season_end <- format(max(forecast_dates), "%Y")
  season <- glue::glue("{season_start}_{season_end}")
  rmarkdown::render(
    score_report_rmd,
    params = list(
      scores = scores,
      forecast_dates = forecast_dates,
      disease = disease,
      target = target
    ),
    output_file = here::here(
      "rendered_reports",
      glue::glue("{disease}_{target}_backtesting_{season}_on_{as.Date(Sys.Date())}")
    )
  )
}
