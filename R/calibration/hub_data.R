# Reading CMU-TimeSeries' own submitted forecasts and the hub's truth out of
# FluSight-forecast-hub, for online calibration experiments.
#
# Why submitted forecasts rather than a re-run component forecaster: calibrating
# what was actually shipped needs no pipeline at all -- no snapshots, no as-of
# archives, no NSSP availability archaeology for 2023-24 -- and the hub ships a
# single consistent truth series covering every season. That removes essentially
# all of the plumbing risk from the first pass at this method. The cost is that
# the submitted forecast is an ensemble whose composition changed between
# seasons, so a calibration offset carried across an off-season gap is a
# correction for a model that no longer exists (see `hidden_scale` in qt.R).
#
# Hub coordinates, which are cleaner than our internal ones:
#   reference_date  Saturday, one per submission round
#   target_end_date Saturday, == reference_date + 7 * horizon
#   horizon         -1:3 for every CMU-TimeSeries round in every season
# Both dates being Saturdays is what makes the revision lag exactly `h + 2`
# rounds with no rounding.

# The 23 quantile levels FluSight has required in every season we cover. Held as
# a canonical vector so levels are keyed by integer index and never float-joined
# -- `output_type_id` arrives as a string ("0.01", "0.025", ...) and round-trip
# parsing is exactly the kind of thing that silently drops rows.
HUB_QUANTILE_LEVELS <- c(
  0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50,
  0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99
)

HUB_FLU_TARGET <- "wk inc flu hosp"
HUB_COVID_TARGET <- "wk inc covid hosp"


# Parent directory containing the flu hub checkout. Override with DELPHI_HUB_PARENT_DIR
# or HUB_FLU_DIR to point at a different layout.
.hub_parent <- function() {
  Sys.getenv("DELPHI_HUB_PARENT_DIR", path.expand("~/allHail/delphi"))
}
HUB_FLU_DIR <- Sys.getenv("HUB_FLU_DIR", file.path(.hub_parent(), "FluSight-forecast-hub"))


#' Snap parsed quantile levels onto the canonical vector.
#'
#' Returns the integer index into `levels`, erroring on anything that is not one
#' of them. Loud rather than lenient: an unexpected level means the hub schema
#' moved, and silently dropping it would quietly shrink the level set the
#' tracker sees.
#' @keywords internal
hub_level_index <- function(x, levels = HUB_QUANTILE_LEVELS, tol = 1e-8) {
  idx <- vapply(
    as.numeric(x),
    function(v) {
      hit <- which(abs(levels - v) < tol)
      if (length(hit) == 1L) hit else NA_integer_
    },
    integer(1)
  )
  if (anyNA(idx)) {
    cli::cli_abort(
      "Quantile level{?s} {.val {unique(x[is.na(idx)])}} {?is/are} not in the
       canonical hub level set. The hub schema may have changed."
    )
  }
  idx
}


#' Read one model's quantile forecasts for one target out of a hub checkout.
#'
#' @param hub_dir local hub checkout. For flu, use `HUB_FLU_DIR`
#'   (cdcepi/FluSight-forecast-hub); for covid, use `HUB_COVID_DIR`
#'   (cdcepi/covid19-forecast-hub). Only the model-output directory is needed.
#' @param model model-output subdirectory.
#' @param target hub target string. 2023-24 files also carry
#'   `wk flu hosp rate change` pmf rows, so both `target` and
#'   `output_type == "quantile"` are filtered.
#' @param locations optionally restrict to these hub location codes.
#' @return long tibble: `reference_date`, `horizon`, `target_end_date`,
#'   `location`, `level_index`, `level`, `value`.
#' @export
hub_read_forecasts <- function(
  hub_dir = HUB_FLU_DIR,
  model = "CMU-TimeSeries",
  target = HUB_FLU_TARGET,
  locations = NULL
) {
  model_dir <- file.path(path.expand(hub_dir), "model-output", model)
  files <- list.files(model_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0L) {
    cli::cli_abort("No submission csvs found in {.path {model_dir}}.")
  }
  cli::cli_alert_info("Reading {length(files)} round{?s} from {.path {model_dir}}")

  out <- purrr::map(files, function(f) {
    readr::read_csv(
      f,
      col_types = readr::cols(
        reference_date = readr::col_date(),
        target = readr::col_character(),
        horizon = readr::col_integer(),
        target_end_date = readr::col_date(),
        location = readr::col_character(),
        output_type = readr::col_character(),
        output_type_id = readr::col_character(),
        value = readr::col_double()
      ),
      progress = FALSE
    ) %>%
      filter(.data$target == !!target, .data$output_type == "quantile")
  }) %>%
    bind_rows()

  if (nrow(out) == 0L) {
    cli::cli_abort("No {.val {target}} quantile rows in {.path {model_dir}}.")
  }
  out <- out %>%
    mutate(level_index = hub_level_index(.data$output_type_id)) %>%
    mutate(level = HUB_QUANTILE_LEVELS[.data$level_index]) %>%
    select(
      "reference_date", "horizon", "target_end_date", "location",
      "level_index", "level", "value"
    )
  if (!is.null(locations)) {
    out <- out %>% filter(.data$location %in% locations)
  }

  # The hub's own invariant: target_end_date is determined by the other two.
  bad <- out %>% filter(.data$target_end_date != .data$reference_date + 7L * .data$horizon)
  if (nrow(bad) > 0L) {
    cli::cli_abort(
      "{nrow(bad)} row{?s} violate
       {.code target_end_date == reference_date + 7 * horizon}; the hub date
       convention this code relies on does not hold."
    )
  }
  out %>% arrange(.data$reference_date, .data$horizon, .data$location, .data$level_index)
}


#' Convert an internal-format forecast tibble to hub forecast format.
#'
#' The internal format uses geo_value (state abbreviation), forecast_date
#' (Wednesday), and target_end_date (already Saturday when target_date_shift=3
#' is set, as in prod). The hub format uses location (FIPS code), reference_date
#' (Saturday end of epiweek), and integer horizon.
#'
#' @param forecasts tibble with columns geo_value, forecast_date,
#'   target_end_date, quantile, value. All 23 canonical quantile levels must be
#'   present; any extra columns are dropped.
#' @param disease passed to [format_flusight()] for the target string (unused
#'   in the output of this function, but validates the input).
#' @return tibble in [hub_read_forecasts()] format: reference_date, horizon,
#'   target_end_date, location, level_index, level, value.
#' @export
internal_to_hub_forecasts <- function(forecasts, disease = c("flu", "covid", "rsv")) {
  disease <- rlang::arg_match(disease)
  forecasts %>%
    mutate(
      reference_date = get_forecast_reference_date(.data$forecast_date),
      horizon = as.integer(.data$target_end_date - .data$reference_date) %/% 7L
    ) %>%
    left_join(
      get_population_data() %>% select("state_id", "state_code"),
      by = c("geo_value" = "state_id")
    ) %>%
    mutate(
      location = .data$state_code,
      level_index = hub_level_index(as.character(.data$quantile)),
      level = HUB_QUANTILE_LEVELS[.data$level_index]
    ) %>%
    select(
      "reference_date", "horizon", "target_end_date",
      "location", "level_index", "level", "value"
    ) %>%
    arrange(.data$reference_date, .data$horizon, .data$location, .data$level_index)
}


#' Label submission rounds with a season, splitting on the off-season gap.
#'
#' Seasons are found from the data rather than hardcoded to Oct-May, so a
#' schedule change cannot silently merge two seasons. Any gap of at least
#' `gap_weeks` between consecutive rounds starts a new season; FluSight's
#' off-season gaps are 20 and 29 weeks while its largest within-season gap is 5
#' (Oct/Nov 2025), so the default sits well clear of both.
#'
#' @param round_date unique, sorted submission dates.
#' @return tibble of `round_index`, `round_date`, `season`, `season_round`,
#'   `is_season_start`, and `gap_weeks` (weeks since the previous round).
#' @export
hub_label_seasons <- function(round_date, gap_weeks = 10L) {
  round_date <- sort(unique(as.Date(round_date)))
  gap <- c(0, as.numeric(diff(round_date)) / 7)
  season_idx <- cumsum(gap >= gap_weeks) + 1L
  # Name seasons by the calendar years they span, so they are recognisable in
  # plots and stable if earlier seasons are added later.
  season_label <- vapply(
    split(round_date, season_idx),
    function(d) {
      y <- as.integer(format(min(d), "%Y"))
      # A season that starts after June belongs to y/y+1, otherwise y-1/y.
      if (as.integer(format(min(d), "%m")) >= 7L) {
        sprintf("%d-%d", y, y + 1L)
      } else {
        sprintf("%d-%d", y - 1L, y)
      }
    },
    character(1)
  )
  tibble(
    round_index = seq_along(round_date),
    round_date = round_date,
    season = unname(season_label[as.character(season_idx)]),
    gap_weeks = gap
  ) %>%
    group_by(.data$season) %>%
    mutate(
      season_round = row_number(),
      is_season_start = .data$season_round == 1L
    ) %>%
    ungroup()
}
