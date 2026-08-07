#' Generate forecaster predictions on a particular dataset
#'
#' A wrapper that turns a forecaster, parameters, data combination into an
#' actual experiment that outputs a prediction for each day.
#'
#' @param archive the epi_df object
#' @param outcome the name of the target column
#' @param ahead the number of days ahead to forecast
#' @param extra_sources any extra columns used for prediction that aren't
#'   the target
#' @param forecaster a function that does the actual forecasting for a given
#'   day. See `exampleSpec.R` for an example function and its documentation
#'   for the general parameter requirements.
#' @param slide_training a required parameter that governs how much data to
#'   exclude before starting the evaluation.
#' @param n_training_pad a required parameter that determines how many extra
#'   samples for epix_slide to hand to the forecaster to guarantee that at
#'   least `ntraining` examples are available to the forecaster.
#' @param forecaster_args the list of arguments to the forecaster; it must
#'   contain `ahead`
#' @param forecaster_args_names a bit of a hack around targets, it contains
#'   the names of the `forecaster_args`.
#' @param date_range_step_size the step size (in days) to use when generating
#'   the forecast dates.
#' @param cache_key a unique identifier for the cache file
#'
#' @importFrom epiprocess epix_slide
#' @importFrom cli cli_abort
#' @importFrom rlang !!
#' @export
slide_forecaster <- function(
  epi_archive,
  outcome,
  ahead,
  forecaster = scaled_pop,
  slide_training = 0,
  n_training_pad = 5,
  forecaster_args = list(),
  forecaster_args_names = list(),
  ref_time_values = NULL,
  start_date = NULL,
  end_date = NULL,
  date_range_step_size = 1L,
  cache_key = NULL
) {
  if (length(forecaster_args) > 0) {
    names(forecaster_args) <- forecaster_args_names
  }
  forecaster_args$ahead <- ahead
  if (!is.numeric(forecaster_args$n_training) && !is.null(forecaster_args$n_training)) {
    n_training <- as.numeric(forecaster_args$n_training)
    net_slide_training <- max(slide_training, n_training) + n_training_pad
  } else {
    n_training <- Inf
    net_slide_training <- slide_training + n_training_pad
  }
  if (is.null(ref_time_values)) {
    # restrict the dataset to areas where training is possible
    if (is.null(start_date)) {
      start_date <- min(epi_archive$DT$time_value) + net_slide_training
    }
    if (is.null(end_date)) {
      end_date <- max(epi_archive$DT$time_value) - forecaster_args$ahead
    }
    ref_time_values <- seq.Date(from = start_date, to = end_date, by = date_range_step_size)
  }

  # first generate the forecasts
  before <- n_training + n_training_pad - 1
  forecaster_args <- rlang::dots_list(
    !!!list(
      outcome = outcome
    ),
    !!!forecaster_args,
    .homonyms = "last"
  )
  forecaster_wrapper <- function(x) {
    rlang::inject(forecaster(epi_data = x, !!!forecaster_args))
  }
  epix_slide_simple(
    epi_archive,
    forecaster_wrapper,
    ref_time_values,
    before,
    cache_key = cache_key
  )
}


#' Take an as-of snapshot of an archive for a single forecast date.
#'
#' The one place both pipelines build the data a forecaster sees. Explore's
#' `epix_slide_simple` maps this over dates (the degenerate case: policy "asof",
#' generation == forecast date, no substitutions, but keeping the parquet slide
#' cache). Prod's per-(forecaster, date) `full_data` / nssp-target slices
#' are single calls with a "cheating" policy and/or data substitutions.
#'
#' @param archive          an epi_archive.
#' @param forecast_date    nominal (Wednesday) forecast date; stamped as the
#'   snapshot's `as_of`.
#' @param generation_date  date the forecast is actually generated; the as-of
#'   version to slice at (and, under "cheating", the future-data cutoff).
#' @param as_of_policy     "asof": slice as-of `generation_date` (real-time
#'   data), honoring `substitutions`, `cache_key`, and `before`. "cheating":
#'   slice the finalized data (versions_end) but drop rows with
#'   `time_value >= generation_date`; `substitutions`, `cache_key`, and `before`
#'   don't apply (finalized data needs no real-time corrections and isn't cached).
#' @param substitutions    optional path/df of manual data corrections applied
#'   via `data_substitutions()`. Only under the "asof" policy: substitutions
#'   correct real-time reporting artifacts, so finalized ("cheating") data
#'   ignores them.
#' @param cache_key        if non-NULL, cache the raw slice to a parquet file
#'   keyed on this plus the archive hash (used by the explore slide). Ignored
#'   under "cheating".
#' @param archive_hash     precomputed `rlang::hash(archive)`, so per-date
#'   callers can hash the archive once instead of on every call. Computed here
#'   when NULL; only used when `cache_key` is set.
#' @param before           training-window bound: `min_time_value` of the slice
#'   is `generation_date - before` (Inf keeps all history). Ignored under
#'   "cheating".
#' @return an epi_df with `as_of` stamped to the forecast date and `other_keys`
#'   preserved from the archive.
make_forecast_snapshot <- function(
  archive,
  forecast_date,
  generation_date,
  as_of_policy = "asof",
  substitutions = NULL,
  cache_key = NULL,
  before = Inf,
  archive_hash = NULL
) {
  forecast_date <- as.Date(forecast_date)
  generation_date <- as.Date(generation_date)
  if (forecast_date > generation_date) {
    cli::cli_abort(
      "make_forecast_snapshot(): forecast_date ({forecast_date}) is after generation_date ({generation_date});
       generation can be delayed past the nominal date, never precede it."
    )
  }

  if (as_of_policy == "cheating") {
    # Warn on args the cheating branch can't honor (substitutions is passed
    # unconditionally by design, so warning on it would be per-run noise).
    if (!is.null(cache_key) || is.finite(before)) {
      cli::cli_warn("make_forecast_snapshot(): {.arg cache_key}/{.arg before} are ignored under the \"cheating\" policy.")
    }
    # Finalized data, but drop anything the real-time run couldn't have seen.
    snapshot <- archive %>% epix_as_of(archive$versions_end)
    other_keys <- attributes(snapshot)$metadata$other_keys
    snapshot <- snapshot %>% filter(time_value < generation_date)
  } else {
    version <- min(generation_date, archive$versions_end)
    read_slice <- function() {
      archive %>% epix_as_of(version, min_time_value = generation_date - before)
    }
    if (is.null(cache_key)) {
      snapshot <- read_slice()
    } else {
      # hash the archive so changing the object without renaming it doesn't pull a stale cache
      cache_hash <- archive_hash %||% rlang::hash(archive)
      dir.create("cache/slide_cache", showWarnings = FALSE, recursive = TRUE)
      file_path <- glue::glue("cache/slide_cache/{cache_key}_{cache_hash}_{before}_{generation_date}.parquet")
      if (file.exists(file_path)) {
        snapshot <- qs::qread(file_path)
      } else {
        snapshot <- read_slice()
        qs::qsave(snapshot, file_path)
      }
    }
    other_keys <- attributes(snapshot)$metadata$other_keys
    if (!is.null(substitutions)) {
      snapshot <- snapshot %>% data_substitutions(substitutions, generation_date)
    }
    # Version faithfulness: an as-of snapshot must contain nothing observed
    # after the generation date. epix_as_of bounds versions, but rows published
    # ahead of their observation date (e.g. faux-versioned augmentation rows
    # stamped version < time_value) slip through that bound, so assert on the
    # assembled rows. The cheating branch peeks at finalized values on purpose;
    # its time_value filter above enforces the same bound.
    leaked <- snapshot$time_value > generation_date
    if (any(leaked)) {
      cli::cli_abort(
        "make_forecast_snapshot(): {sum(leaked)} row{?s} with time_value after generation_date ({generation_date}),
         e.g. time_value {max(snapshot$time_value[leaked])}: an as-of snapshot leaked future observations."
      )
    }
  }

  # data_substitutions() drops the epi_df metadata; restore other_keys and stamp
  # the nominal forecast date as the as_of. The min() clamp is a no-op for real
  # forecast dates and reproduces epix_as_of's as_of for the explore case.
  attributes(snapshot)$metadata$other_keys <- other_keys
  attributes(snapshot)$metadata$as_of <- min(forecast_date, archive$versions_end)
  snapshot
}


#' Truncate an archive to what a forecast date could have seen.
#'
#' The archive analog of [make_forecast_snapshot]: rather than collapsing the
#' archive to a single as-of `epi_df`, this hands a revision-aware forecaster the
#' whole archive (main and auxiliary columns alike) with every version after the
#' generation date removed, so it can reconstruct any past vintage up to the
#' forecast date itself. Truncation bounds versions, which is exactly the version
#' faithfulness [make_forecast_snapshot] has to assert after the fact.
#'
#' @param archive         an `epi_archive`.
#' @param forecast_date   nominal (Wednesday) forecast date. Kept for a symmetric
#'   signature with [make_forecast_snapshot]; must not be after `generation_date`.
#' @param generation_date date the forecast is actually generated; the latest
#'   version retained.
#' @return an `epi_archive` with `versions_end <= generation_date`.
#'
#' @importFrom epiprocess epix_truncate_versions_after
make_forecast_archive_snapshot <- function(archive, forecast_date, generation_date) {
  forecast_date <- as.Date(forecast_date)
  generation_date <- as.Date(generation_date)
  if (forecast_date > generation_date) {
    cli::cli_abort(
      "make_forecast_archive_snapshot(): forecast_date ({forecast_date}) is after generation_date ({generation_date})."
    )
  }
  epix_truncate_versions_after(archive, min(generation_date, archive$versions_end))
}


epix_slide_simple <- function(epi_archive, forecaster, ref_time_values, before = Inf, cache_key = NULL) {
  # hash once here rather than per ref_time_value inside make_forecast_snapshot
  archive_hash <- if (!is.null(cache_key)) rlang::hash(epi_archive)
  out <- purrr::map(ref_time_values, function(tv) {
    make_forecast_snapshot(
      epi_archive,
      forecast_date = tv,
      generation_date = tv,
      as_of_policy = "asof",
      cache_key = cache_key,
      before = before,
      archive_hash = archive_hash
    ) %>%
      forecaster()
  }) %>%
    bind_rows()
  gc()
  return(out)
}
