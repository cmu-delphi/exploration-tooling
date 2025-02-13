# Reusable transforms to apply before handing to epipredict

#' Extract the non-key, non-smoothed columns from epi_data
#'
#' @param epi_data the `epi_df`
#' @param cols vector of column names to use. If `NULL`, fill with all non-key columns
get_trainable_names <- function(epi_data, cols) {
  if (is.null(cols)) {
    cols <- get_nonkey_names(epi_data)
    # exclude anything with the same naming schema as the rolling average/sd created below
    cols <- cols[!grepl("_\\w{1,2}\\d+", cols)]
  }
  return(cols)
}

#' Just the names which aren't keys for an epi_df
#'
#' Names, but it excludes keys
#'
#' @param epi_data the epi_df
get_nonkey_names <- function(epi_data) {
  cols <- names(epi_data)
  cols <- cols[!(cols %in% c("geo_value", "time_value", attr(epi_data, "metadata")$other_keys))]
  return(cols)
}

#' Get a rolling average for the named columns
#'
#' Add column(s) that are the rolling means of the specified columns, as
#' implemented by slider. Defaults to the previous 7 days. Currently only
#' group_by's on the geo_value. Should probably extend to more keys if you have
#' them.
#'
#' @param epi_data the dataset
#' @param width the number of days (or examples, the sliding isn't time-aware) to use
#' @param cols_to_mean the non-key columns to take the mean over. `NULL` means all
#'
#' @importFrom slider slide_dbl
#' @importFrom epiprocess epi_slide
#' @export
rolling_mean <- function(epi_data, width = 7L, cols_to_mean = NULL) {
  cols_to_mean <- get_trainable_names(epi_data, cols_to_mean)
  epi_data %>%
    group_by(across(key_colnames(epi_data, exclude = "time_value"))) %>%
    epi_slide_mean(all_of(cols_to_mean), .window_size = width, .new_col_names = paste0("slide_", cols_to_mean, "_m", width)) %>%
    ungroup()
}

#' Get a rolling standard deviation for the named columns
#'
#' A rolling standard deviation, based off of a rolling mean. First it
#' calculates a rolling mean with width `mean_width`, and then squares the
#' difference between that and the actual value, averaged over `sd_width`.
#'
#' @param epi_data the dataset
#' @param sd_width the number of days (or examples, the sliding isn't
#'   time-aware) to use for the standard deviation calculation
#' @param mean_width like `sd_width`, but it governs the mean. Should be less
#'   than the `sd_width`, and if `NULL` (the default) it is half of `sd_width`
#'   (so 14 in the complete default case)
#' @param cols_to_sd the non-key columns to take the sd over. `NULL` means all
#' @param keep_mean bool, if `TRUE`, it retains keeps the mean column
#'
#' @importFrom epiprocess epi_slide
#' @export
rolling_sd <- function(epi_data, sd_width = 29L, mean_width = NULL, cols_to_sd = NULL, keep_mean = FALSE) {
  if (is.null(mean_width)) {
    mean_width <- as.integer(ceiling(sd_width / 2))
  }
  cols_to_sd <- get_trainable_names(epi_data, cols_to_sd)
  result <- epi_data
  result %<>% group_by(across(key_colnames(epi_data, exclude = "time_value")))
  for (col_name in cols_to_sd) {
    mean_name <- glue::glue("slide_{col_name}_m{mean_width}")
    sd_name <- glue::glue("slide_{col_name}_sd{sd_width}")

    result %<>%
      epi_slide_mean(all_of(col_name), .window_size = mean_width, .new_col_names = mean_name)

    result %<>%
      mutate(.temp = (.data[[mean_name]] - .data[[col_name]])^2) %>%
      epi_slide_mean(all_of(".temp"), .window_size = sd_width, .new_col_names = sd_name) %>%
      mutate(!!sd_name := sqrt(.data[[sd_name]])) %>%
      select(-.temp)

    if (!keep_mean) {
      result %<>% select(-{{ mean_name }})
    }
  }

  result %<>% ungroup()
  result
}

#' Temporary patch that pulls `NA`'s out of an epi_df
#'
#' Just delete rows that have NA's in them. eventually epipredict should
#' directly handle this so we don't have to
#'
#' @param epi_data the epi_df to be fixed
#' @param outcome the column name containing the target variable
#' @param extra_sources any other columns used as predictors
#'
#' @importFrom tidyr drop_na
#' @importFrom epiprocess as_epi_df
#' @export
clear_lastminute_nas <- function(epi_data, outcome, extra_sources) {
  meta_data <- attr(epi_data, "metadata")
  if (extra_sources == c("")) {
    extra_sources <- character(0L)
  }
  as_of <- attributes(epi_data)$metadata$as_of
  other_keys <- attributes(epi_data)$metadata$other_keys %||% character()
  epi_data %<>%
    drop_na(c(!!outcome, !!!extra_sources)) %>%
    as_epi_df(as_of = as_of, other_keys = other_keys)
  attr(epi_data, "metadata") <- meta_data
  return(epi_data)
}

#' Only extend the ahead
#'
#' Instead of filling in new values, this just extends how far into the future
#' the model is predicting. For example, if the last data is on the 3rd, the
#' `as_of` is the 5th, and we want an ahead of 4, then this actually sets the
#' ahead to be 6, since the 9th (the target date) is 6 days after the last day
#' of data.
#'
#' @param epi_data the dataset
#' @param ahead how many units (depending on the dataset, normally days or
#' weeks) to predict ahead of the `forecast_date`
#'
#' @export
extend_ahead <- function(epi_data, ahead) {
  time_values <- epi_data$time_value
  if (length(time_values) > 0) {
    as_of <- attributes(epi_data)$metadata$as_of
    max_time <- max(time_values)
    if (is.null(as_of)) {
      as_of <- max_time
    }
    effective_ahead <- as.integer(as.Date(as_of) - max_time + ahead)
  } else {
    effective_ahead <- Inf
  }
  return(list(epi_data, effective_ahead))
}

#' get the Taylor expansion coefficients for a vector of values
#' @param values the vector of values to interpolate
#' @param degree the degree of the polynomial
#' @param the expected length of values (needed b/c epi_slide may return fewer
#'   points)
#' @export
get_poly_coefs <- function(values, degree, n_points) {
  values <- values[!is.na(values)]
  coef_name <- paste0("c", seq(1, degree + 1, by = 1))
  if (length(values) < n_points) {
    # return NA's for all values
    return(
      tibble(coef_name, val = as.double(NA)) %>%
        pivot_wider(values_from = val, names_from = coef_name)
    )
  }
  res <- tibble(time_value = seq(-n_points + 2, 1), value = values) %>%
    lm(value ~ poly(time_value, degree = degree, raw = TRUE), .)
  coefs <- unname(res$coefficients)
  names(coefs) <- coef_name
  as_tibble(t(coefs))
}

#' get the mean and median used to whiten epi_data on a per source-geo_value basis
#' note that we can't just use step_boxcox or step yeo-johnson because it doesn't allow for grouping
calculate_whitening_params <- function(
    epi_data,
    colname,
    scale_method = c("quantile", "quantile_upper", "std", "none"),
    center_method = c("median", "mean", "none"),
    nonlin_method = c("quart_root", "none")) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  if (scale_method == "none") {
    return(NULL)
  }
  if (nonlin_method == "quart_root") {
    scaled_data <- epi_data %>% mutate(across(all_of(colname), \(x) (x + 0.01)^0.25))
  } else if (nonlin_method == "none") {
    scaled_data <- epi_data
  }
  scaled_data <- scaled_data %>% group_by(source, geo_value)
  # center so that either the mean or median is 0
  if (center_method == "mean") {
    fn <- mean
  } else if (center_method == "median") {
    fn <- median
  } else {
    fn <- \(x, na.rm) 0
  }
  learned_params <- scaled_data %>%
    summarize(
      across(all_of(colname), ~ fn(.x, na.rm = TRUE), .names = "{.col}_center"),
      .groups = "drop"
    )
  if (scale_method == "quantile") {
    # scale so that the difference between the 5th and 95th quantiles is 1
    scale_fn <- function(x) {
      diff(quantile(x, c(0.05, 0.95), na.rm = TRUE))[[1]] + .01
    }
  } else if (scale_method == "quantile_upper") {
    # scale so that the 95th quantile is 1
    scale_fn <- function(x) (quantile(x, 0.95, na.rm = TRUE) + 0.01)
  } else {
    # scale so that one standard deviation is 1
    scale_fn <- function(x) (sd(x, 0.95, na.rm = TRUE) + 0.01)
  }
  learned_params %<>% full_join(
    summarize(
      scaled_data,
      across(all_of(colname), ~ scale_fn(.x), .names = "{.col}_scale"),
      .groups = "drop"
    ),
    by = join_by(source, geo_value)
  )
  return(learned_params)
}

#' scale so that every data source has the same 95th quantile
data_whitening <- function(epi_data, colname, learned_params, nonlin_method = c("quart_root", "none"), join_cols = NULL) {
  if (is.null(learned_params)) {
    return(epi_data)
  }
  if (is.null(join_cols)) {
    join_cols <- key_colnames(epi_data, exclude = "time_value")
  }
  nonlin_method <- arg_match(nonlin_method)
  res <- epi_data %>%
    left_join(learned_params, by = join_cols)
  if (nonlin_method == "quart_root") {
    res %<>% mutate(across(all_of(colname), ~ (.x + 0.01)^(1 / 4)))
  }
  res %>%
    mutate(across(all_of(colname), ~ .x - get(paste0(cur_column(), "_center")))) %>%
    mutate(across(all_of(colname), ~ .x / get(paste0(cur_column(), "_scale")))) %>%
    select(-ends_with("_center"), -ends_with("_scale"))
}

#' undo data whitening by multiplying by the scaling and adding the center
data_coloring <- function(epi_data, colname, learned_params, nonlin_method = c("quart_root", "none"), join_cols = NULL) {
  if (is.null(learned_params)) {
    return(epi_data)
  }
  if (is.null(join_cols)) {
    join_cols <- key_colnames(epi_data, exclude = "time_value")
  }
  nonlin_method <- arg_match(nonlin_method)
  res <- epi_data %>%
    left_join(learned_params, by = join_cols) %>%
    mutate(across(all_of(colname), ~ .x * get(paste0(cur_column(), "_scale")))) %>%
    mutate(across(all_of(colname), ~ .x + get(paste0(cur_column(), "_center"))))
  if (nonlin_method == "quart_root") {
    res %<>% mutate(across(all_of(colname), ~ .x^4 - 0.01))
  }
  res %>% select(-ends_with("_center"), -ends_with("_scale"))
}

#' the distance between two integers/dates, mod m e.g. mod_dist(1,9,10) = 2
mod_dist <- function(a, b, m) {
  pmin(as.integer(a - b) %% m, as.integer(b - a) %% m)
}

#' adds a column giving the median value for a window around a given point in
#' time, and around the time `ahead` days ahead of it
#' @param ahead measured in days, regardless of whether we're forecasting weekly or daily data
#' @param epi_data expected columns include
climate_median <- function(epi_data, target, ahead, window_size = 3, recent_window = 3, probs = covidhub_probs(), geo_agg = TRUE, scale_rate = TRUE, normalize = FALSE) {
  # epi_data <- tar_read(joined_archive_data) %>% epix_as_of(as.Date("2023-11-29"))
  as_of <- attributes(epi_data)$metadata$as_of
  last_date_data <- epi_data %>%
    pull(time_value) %>%
    max()
  filtered <-
    epi_data %>%
    filter(if_any(all_of(target), \(x) !is.na(x))) %>%
    filter((season != "2020/21") & (season != "2021/22") & (season != "2019/20"))
  if (!scale_rate) {
    # add the
    if (!("population" %in% names(filtered))) {
      filtered %<>%
        left_join(get_population_data() %>% select(state_id, population), by = join_by(geo_value == state_id))
    }
    filtered %<>% mutate(across(all_of(target), \(x) x / population * 1e5))
  }
  week_ahead <- as.integer(floor((ahead) / 7))
  moving_medians <- lapply(1:53, function(target_week) {
    rel_values <- filtered %>%
      filter(
        (mod_dist(epiweek(time_value), target_week + week_ahead, 53) <= window_size) |
          (mod_dist(epiweek(time_value), target_week, 53) <= recent_window)
      )
    state_medians <- rel_values %>%
      group_by(
        across(
          all_of(key_colnames(rel_values, exclude = "time_value"))
        )
      ) %>%
      summarize(
        across(
          all_of(target),
          \(x) median(x, na.rm = TRUE),
          .names = "climate_median"
        ),
        .groups = "drop"
      )
    if (geo_agg) {
      pooled_median <- rel_values %>%
        group_by(across(all_of(key_colnames(rel_values, exclude = c("geo_value", "time_value"))))) %>%
        summarize(
          across(
            all_of(target),
            \(x) median(x, na.rm = TRUE),
            .names = "pooled_climate_median"
          ),
          .groups = "drop"
        )
      state_medians <- state_medians %>%
        mutate(epiweek = target_week) %>%
        left_join(
          pooled_median,
          by = key_colnames(rel_values, exclude = c("time_value", "geo_value"))
        )
    }
    return(state_medians)
  })
  moving_medians <- moving_medians %>% bind_rows()
  if (normalize) {
    movining_medians <- moving_medians %>% mutate(climate_median = climate_median / max(abs(climate_median)))
  }
  epi_data %>%
    mutate(epiweek = epiweek(time_value)) %>%
    left_join(
      moving_medians,
      by = c("epiweek", key_colnames(epi_data, exclude = "time_value"))
    )
}

#' add the first principal component for each season_week to epi_data, shifted
#' by ahead
#' @description
#' It caches the pc based on the disease and the incoming dataset
#' @param epi_data it is expected to have seasons, season_weeks and hhs as a target
#' @param ahead is measured in days
#' @param filter_time the last day of data to include in the pca computation
compute_pca <- function(epi_data, disease = "flu", ahead = 0, scale_method = "quantile", center_method = "median", nonlin_method = "quart_root", filter_time = "2320-07-01", normalize = FALSE) {
  used_data <- epi_data %>%
    filter(time_value < filter_time) %>%
    select(geo_value, season, source, season_week, hhs)
  cache_file_path <- paste0(
    "aux_data/seasonal_features/",
    disease[grepl("flu|covid", disease)],
    rlang::hash(used_data),
    ".parquet",
    sep = "_"
  )
  if (file.exists(cache_file_path)) {
    seasonal_features <- qs::qread(cache_file_path)
  } else {
    # need to create the pcs
    wide <- used_data %>%
      filter(!is.na(season_week), !is.na(hhs)) %>%
      pivot_wider(names_from = c(geo_value, season, source), values_from = hhs) %>%
      arrange(season_week) %>%
      fill(-season_week, .direction = "downup")

    pca <- wide %>%
      keep(~ !all(near(diff(.x), 0))) %>%
      arrange(season_week) %>%
      select(-season_week) %>%
      as.matrix() %>%
      prcomp()
    seasonal_features <- as_tibble(predict(pca)[, 1:3]) %>% select(PC1)
    seasonal_features$season_week <- 1:nrow(seasonal_features)
    qs::qsave(seasonal_features, cache_file_path)
  }
  if (normalize) {
    # normalize so that the maximum value is between [-1,1]
    seasonal_features <- seasonal_features %>% mutate(PC1 = PC1 / max(abs(PC1)))
  }
  epi_data %>%
    left_join(
      seasonal_features %>% mutate(season_week = shift(season_week, ahead / 7, type = "cyclic")),
      by = "season_week"
    )
}
