convert_epiweek_to_season <- function(epiyear, epiweek) {
  # Convert epiweek to season
  update_inds <- epiweek <= 39
  epiyear <- ifelse(update_inds, epiyear - 1, epiyear)

  season <- paste0(epiyear, "/", substr((epiyear + 1), 3, 4))
  return(season)
}

epiweeks_in_year <- function(year) {
  last_week_of_year <- seq.Date(as.Date(paste0(year, "-12-24")), as.Date(paste0(year, "-12-31")), by = 1)
  return(max(as.numeric(MMWRweek::MMWRweek(last_week_of_year)$MMWRweek)))
}

convert_epiweek_to_season_week <- function(epiyear, epiweek, season_start = 39) {
  season_week <- epiweek - 39
  update_inds <- season_week <= 0
  if (!any(update_inds)) {
    # none need to be updated
    return(season_week)
  }
  # last year's # of epiweeks determines which week in the season we're at at
  # the beginning of the year
  season_week[update_inds] <- season_week[update_inds] +
    sapply(epiyear[update_inds] - 1, epiweeks_in_year)

  return(season_week)
}

#' Adds epiweek, epiyear, season_week, season columns to the dataset.
#'
#' Assumes that the dataset has a time_value column that is a date. If
#' season_week or season already exist, they will be dropped and replaced.
add_season_info <- function(data) {
  if (!("time_value" %in% names(data))) {
    cli::cli_abort("'time_value' column not found in data", call = rlang::caller_fn())
  }

  data %>%
    select(-any_of(c("season", "season_week", "epiweek", "epiyear"))) %>%
    mutate(
      epiweek = epiweek(time_value),
      epiyear = epiyear(time_value)
    ) %>%
    left_join(
      (.) %>%
        distinct(epiweek, epiyear) %>%
        mutate(
          season = convert_epiweek_to_season(epiyear, epiweek),
          season_week = convert_epiweek_to_season_week(epiyear, epiweek)
        ),
      by = c("epiweek", "epiyear")
    )
}

#' add a sine and half sine component; it is zero after `season` (by default 35, which roughly corresponds to epiweek 23)
step_season_week_sine <- function(preproc, season = 35) {
  preproc %<>%
    step_mutate(
      season_half_sine = sinpi((pmin(season_week, !!season + 1) - 1) / !!season),
      season_sine = sinpi(2 * (pmin(season_week, !!season + 1) - 1) / !!season),
      role = "pre-predictor"
    )
}

#' Aggregate a daily archive to a weekly archive.
#'
#' By default, aggregates from Sunday to Saturday and labels with the Wednesday
#' of that week.
#'
#' @param epi_df the archive to aggregate.
#' @param agg_method the method to use to aggregate the data, one of "sum" or "mean".
#' @param keys the columns to group by.
#' @param values the columns to aggregate.
daily_to_weekly <- function(epi_df, agg_method = c("sum", "mean"), keys = "geo_value", values = c("value")) {
  agg_method <- arg_match(agg_method)
  epi_df %>%
    arrange(across(all_of(c(keys, "time_value")))) %>%
    mutate(epiweek = epiweek(time_value), year = epiyear(time_value)) %>%
    group_by(across(any_of(c(keys, "epiweek", "year")))) %>%
    summarize(
      across(all_of(values), ~ sum(.x, na.rm = TRUE)),
      time_value = floor_date(max(time_value), "weeks", week_start = 7) + 3,
      .groups = "drop"
    ) %>%
    arrange(across(all_of(c(keys, "time_value")))) %>%
    select(-epiweek, -year)
}

#' Aggregate a daily archive to a weekly archive.
#'
#' @param epi_arch the archive to aggregate.
#' @param agg_columns the columns to aggregate.
#' @param agg_method the method to use to aggregate the data, one of "sum" or "mean".
#' @param week_reference the day of the week to use as the reference day (Wednesday is default).
#'   Note that this is 1-indexed, so 1 = Sunday, 2 = Monday, ..., 7 = Saturday.
#' @param week_start the day of the week to use as the start of the week (Sunday is default).
#'   Note that this is 1-indexed, so 1 = Sunday, 2 = Monday, ..., 7 = Saturday.
daily_to_weekly_archive <- function(
  epi_arch,
  agg_columns,
  agg_method = c("sum", "mean"),
  week_reference = 4L,
  week_start = 7L
) {
  # How to aggregate the windowed data.
  agg_method <- arg_match(agg_method)
  # The columns we will later group by when aggregating.
  keys <- key_colnames(epi_arch, exclude = c("time_value", "version"))
  # The versions we will slide over.
  ref_time_values <- epi_arch$DT$version %>%
    unique() %>%
    sort()
  # Choose a fast function to use to slide and aggregate.
  if (agg_method == "sum") {
    # If the week is complete, this is equivalent to the sum. If the week is not
    # complete, this is equivalent to 7/(number of days in the week) * the sum,
    # which should be a decent approximation.
    agg_fun <- \(x) 7 * mean(x, na.rm = TRUE)
  } else if (agg_method == "mean") {
    agg_fun <- \(x) mean(x, na.rm = TRUE)
  }
  # Slide over the versions and aggregate.
  epix_slide(
    epi_arch,
    .versions = ref_time_values,
    function(x, group_keys, ref_time) {
      # Slide over the days and aggregate.
      x %>%
        mutate(week_start = ceiling_date(time_value, "week", week_start = week_start) - 1) %>%
        summarize(across(all_of(agg_columns), agg_fun), .by = all_of(c(keys, "week_start"))) %>%
        mutate(time_value = round_date(week_start, "week", week_reference - 1)) %>%
        select(-week_start) %>%
        as_tibble()
    }
  ) %>%
    # Always convert to data.frame after dplyr operations on data.table.
    # https://github.com/cmu-delphi/epiprocess/issues/618
    as.data.frame() %>%
    as_epi_archive(compactify = TRUE)
}

#' for training, we don't want off-season times or anomalous seasons, but for
#' prediction we do
drop_non_seasons <- function(epi_data, min_window = 12) {
  forecast_date <- attributes(epi_data)$metadata$as_of %||% max(epi_data$time_value)
  if ("season_week" %nin% names(epi_data)) {
    epi_data %<>% add_season_info()
  }
  epi_data %>%
    filter(
      (season_week < 35) |
        (forecast_date - time_value < as.difftime(min_window, units = "weeks")),
      season != "2020/21",
      # season != "2021/22", # keeping this because whitening otherwise gets really bad with the single season of data
      (season != "2019/20"),
      season != "2008/09"
    )
}
