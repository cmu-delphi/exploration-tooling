forecaster_lookup <- function(pattern, forecaster_params_grid = NULL) {
  if (is.null(forecaster_params_grid)) {
    if (!exists("g_forecaster_params_grid")) {
      cli::cli_warn(
        "Reading `forecaster_params_grid` target. If it's not up to date, results will be off.
    Update with `tar_make(g_forecaster_params_grid)`."
      )
      forecaster_params_grid <- tar_read_raw("forecaster_params_grid")
    } else {
      forecaster_params_grid <- forecaster_params_grid %||% g_forecaster_params_grid
    }
  }

  # Remove common prefix for convenience.
  if (grepl("forecast_", pattern)) {
    pattern <- gsub("forecast_", "", pattern)
  }
  if (grepl("forecaster_", pattern)) {
    pattern <- gsub("forecaster_", "", pattern)
  }

  out <- forecaster_params_grid %>% filter(grepl(pattern, .data$id))
  if (nrow(out) > 0) {
    out %>% unlist()
    return(out)
  }
}

#' Add a unique id based on the column contents
#'
#' Create a string of `n_adj` that is a hash of the parameters.
#'
#' @param tib the tibble to add a column to. everything should be convertable to a string
#' @param exclude a vector of column names to exclude from the hash
#'
#' @export
add_id <- function(tib, exclude = c()) {
  ids <- tib %>%
    select(-all_of(exclude)) %>%
    purrr::transpose() %>%
    map_chr(get_single_id)
  tib %>%
    mutate(id = ids) %>%
    relocate(id, .before = everything())
}

#' Generate a two-word id from a simple list of parameters.
#'
#' @param param_list a list of parameters.
#'
#' @export
get_single_id <- function(param_list) {
  param_list[sort(names(param_list))] %>%
    paste(sep = "", collapse = "") %>%
    gsub("[[:blank:]]", "", .) %>%
    cli::hash_animal(n_adj = 1) %>%
    purrr::pluck("words", 1) %>%
    paste(sep = ".", collapse = ".")
}

# Per-forecaster wrapping/metadata read by run_forecaster() and scoring rather
# than passed to the forecaster as a modeling parameter. Declared inline in the
# parameter tibbles; make_forecaster_grid() separates them from the params
# list-column and fills these defaults for whatever a forecaster omits, so both
# explore and prod grids carry them uniformly (replacing prod's metadata
# left_join and explore's per-script column stamping).
#   as_of_policy      "asof"/"cheating": train on as-of-generation-date data or
#                     the latest available revision.
#   ahead_multiplier  factor the (weekly) ahead is multiplied by before it
#                     reaches the forecaster: 1 for week-native forecasters
#                     (cdc/linear/climate), 7 for day-native ones (scaled_pop*).
#   target_date_shift days added to target_end_date after forecasting (Wed->Sat).
#   join_extra_data   left-join extra_data before forecasting (drop source after).
#   filter_sources    if non-NULL, keep only these sources in the input data.
#   excluded_geos     geos dropped from the output.
#   sort_quantiles    enforce quantile monotonicity (the flu whitening workaround).
#   output_scale      "count"/"per100k": whether scoring rescales to counts.
#   min_train_date    if non-NULL (declare as list(as.Date(...))), drop training
#                     rows before this absolute date at the snapshot boundary
#                     (covid cdc_baseline uses it to bound quantile spread).
#   needs_archive     TRUE hands the forecaster the truncated epi_archive instead
#                     of an as-of epi_df snapshot, for revision-aware forecasters
#                     (see make_forecast_archive_snapshot / run_forecaster).
FORECASTER_SPEC_DEFAULTS <- list(
  as_of_policy = "asof",
  ahead_multiplier = 1L,
  target_date_shift = 0L,
  join_extra_data = FALSE,
  filter_sources = NULL,
  excluded_geos = NULL,
  sort_quantiles = FALSE,
  output_scale = "count",
  min_train_date = NULL,
  needs_archive = FALSE
)

#' Make a forecaster grid.
#'
#' Convert a tibble of forecasters and their parameters to a specific format
#' that we can iterate over in targets. Currently only `forecaster` and
#' `trainer` can be symbols. Any of the columns in `FORECASTER_SPEC_DEFAULTS`
#' are treated as per-forecaster spec/metadata (kept as top-level grid columns,
#' filled with defaults when absent) rather than forecaster parameters.
#'
#' @param tib the tibble of parameters. Must have the forecaster and trainer
#' columns, everything else is optional.
#'
#' @export
make_forecaster_grid <- function(tib, family) {
  if ("trainer" %in% colnames(tib)) {
    tib$trainer <- rlang::syms(tib$trainer)
  }
  spec_names <- names(FORECASTER_SPEC_DEFAULTS)
  # turns a tibble into a list of named lists (spec columns are not params)
  params_list <- tib %>%
    select(-any_of(c("forecaster", "id", spec_names))) %>%
    split(seq_len(nrow(.))) %>%
    unname() %>%
    lapply(as.list)
  # for whatever reason, trainer ends up being a list of lists, which we do not want
  params_list %<>%
    lapply(function(x) {
      x$trainer <- x$trainer[[1]]
      x$lags <- x$lags[[1]]
      x
    })

  if (length(params_list) == 0) {
    out <- tibble(
      family = family,
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = list(list())
    )
  } else {
    out <- tibble(
      family = family,
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = params_list
    )
  }

  # Attach spec columns, taking each forecaster's inline override when present
  # and the shared default otherwise.
  for (col in spec_names) {
    default <- FORECASTER_SPEC_DEFAULTS[[col]]
    if (col %in% colnames(tib)) {
      out[[col]] <- tib[[col]]
    } else if (is.null(default)) {
      # list-valued spec (filter_sources / excluded_geos): default is NULL
      out[[col]] <- rep(list(NULL), nrow(out))
    } else {
      out[[col]] <- rep(default, nrow(out))
    }
  }

  return(out)
}

data_substitutions <- function(dataset, substitutions_path, forecast_generation_date) {
  # Get the substitutions from the table, matched by forecast generation date
  substitutions <- readr::read_csv(
    substitutions_path,
    comment = "#",
    show_col_types = FALSE,
    col_types = readr::cols(
      geo_value = readr::col_character(),
      forecast_date = readr::col_date(),
      time_value = readr::col_date(),
      value = readr::col_double()
    )
  ) %>%
    filter(forecast_date == forecast_generation_date) %>%
    select(-forecast_date) %>%
    rename(new_value = value) %>%
    mutate(
      time_value = floor_date(time_value, "week", week_start = 7) + 3
    )
  # Replace the most recent values in the appropriate keys with the substitutions
  new_values <- dataset %>%
    inner_join(substitutions, by = join_by(geo_value, time_value)) %>%
    mutate(value = ifelse(!is.na(new_value), new_value, value)) %>%
    select(-new_value)
  # Remove keys from dataset that have been substituted
  dataset %>%
    anti_join(new_values, by = c("geo_value", "time_value")) %>%
    bind_rows(new_values)
}
