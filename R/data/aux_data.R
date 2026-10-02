EPIDATA_V5_URL <- "https://delphi.cmu.edu/epidata/v5"

build_cast_api_query <- function(
  source = c("nssp", "nhsn"),
  signal = NULL,
  geo_type = c("state", "nation", "hhs"),
  columns = NULL,
  fill_method = NULL,
  limit = NULL,
  offset = NULL,
  report_time_query = NULL,
  geo_value = NULL,
  time_value = NULL
) {
  source <- rlang::arg_match(source)
  if (!is.null(fill_method)) fill_method <- rlang::arg_match(fill_method, c("source", "fill_ave", "fill_zero"))
  geo_type <- rlang::arg_match(geo_type)
  columns <- columns %||% c("geo_value", "time_value", "value", "version")
  columns <- gsub("\\btime_value\\b", "reference_time", columns)
  columns <- gsub("\\bversion\\b", "report_time", columns)
  columns <- paste(columns, collapse = ",")

  httr2::request(EPIDATA_V5_URL) %>%
    httr2::req_url_path_append("archive/") %>%
    httr2::req_url_query(
      source = source,
      signal = signal,
      geo_type = geo_type,
      report_time_query = report_time_query,
      columns = columns,
      limit = limit,
      offset = offset,
      fill_method = fill_method,
      geo_value = geo_value,
      reference_time = time_value,
      format = "csv",
      header = "true",
      .multi = "explode"
    ) %>%
    {
      key <- Sys.getenv("DELPHI_EPIDATA_KEY")
      if (nchar(key) > 0) httr2::req_headers_redacted(., token = key) else .
    }
}

get_cast_api_data <- function(...) {
  req <- build_cast_api_query(...)
  if (Sys.getenv("DEBUG_MODE") == "true") print(req)
  filename <- tempfile(fileext = ".csv")
  req %>% httr2::req_perform(path = filename)
  readr::read_csv(filename, show_col_types = FALSE) %>%
    dplyr::rename(any_of(c(time_value = "reference_time", version = "report_time")))
}

# Fetch a signal for each of `geo_types`, bind, and normalize: lowercase
# character geo_value, Date version, deduplicated on (geo_value, time_value,
# version), plus fill_method when it is requested.
get_cast_api_all_geos <- function(source, signal, columns = c("geo_value", "time_value", "value", "version"),
                                  geo_types = c("state", "nation"), ...) {
  purrr::map(geo_types, \(gt) get_cast_api_data(source = source, signal = signal, geo_type = gt, columns = columns, ...)) %>%
    purrr::map(\(d) mutate(d, geo_value = as.character(geo_value))) %>%
    bind_rows() %>%
    mutate(geo_value = tolower(geo_value), version = as.Date(version)) %>%
    arrange(geo_value, time_value, version) %>%
    distinct(across(any_of(c("geo_value", "time_value", "version", "fill_method"))), .keep_all = TRUE)
}

get_nwss_coarse_data <- function(disease = c("covid", "flu")) {
  disease <- arg_match(disease)
  # TODO: Something is broken about get_bucket_df. There is only key, so just use that directly.
  # aws.s3::get_bucket_df(prefix = glue::glue("2024/aux_data/nwss_{disease}_data"), bucket = "forecasting-team-data") %>%
  #   slice_max(LastModified) %>%
  #   pull(Key) %>%
  #   aws.s3::s3read_using(FUN = readr::read_csv, object = ., bucket = "forecasting-team-data")
  key <- glue::glue("2024/aux_data/nwss_{disease}_data/nwss_20241028.csv")
  aws.s3::s3read_using(
    FUN = readr::read_csv,
    object = key,
    bucket = "forecasting-team-data",
    show_col_types = FALSE
  )
}

#' Get versioned NHSN data from healthdata.gov because covidcast API has
#' incorrect historical data for 2023-2024 season.
get_health_data <- function(as_of, disease = c("covid", "flu")) {
  as_of <- as.Date(as_of)
  disease <- arg_match(disease)
  checkmate::assert_date(as_of, min.len = 1, max.len = 1)

  cache_path <- here::here("cache", "healthdata")
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }

  metadata_path <- here::here(cache_path, "metadata.csv")
  if (!file.exists(metadata_path)) {
    meta_data <- readr::read_csv(
      "https://healthdata.gov/resource/qqte-vkut.csv?$query=SELECT%20update_date%2C%20days_since_update%2C%20user%2C%20rows%2C%20row_change%2C%20columns%2C%20column_change%2C%20metadata_published%2C%20metadata_updates%2C%20column_level_metadata%2C%20column_level_metadata_updates%2C%20archive_link%20ORDER%20BY%20update_date%20DESC%20LIMIT%2010000",
      show_col_types = FALSE
    )
    readr::write_csv(meta_data, metadata_path)
  } else {
    meta_data <- readr::read_csv(metadata_path, show_col_types = FALSE)
  }

  most_recent_row <- meta_data %>%
    # update_date is actually a time, so we need to filter for the day after.
    filter(update_date <= as.Date(as_of) + 1) %>%
    slice_max(update_date)

  if (nrow(most_recent_row) == 0) {
    cli::cli_abort("No data available for the given date.")
  }

  data_filepath <- here::here(cache_path, sprintf("g62h-syeh-%s.csv", as.Date(most_recent_row$update_date)))
  if (!file.exists(data_filepath)) {
    data <- readr::read_csv(most_recent_row$archive_link, show_col_types = FALSE)
    readr::write_csv(data, data_filepath)
  } else {
    data <- readr::read_csv(data_filepath, show_col_types = FALSE)
  }
  if (disease == "covid") {
    data %<>%
      mutate(
        hhs = previous_day_admission_adult_covid_confirmed +
          previous_day_admission_pediatric_covid_confirmed
      )
  } else if (disease == "flu") {
    data %<>% mutate(hhs = previous_day_admission_influenza_confirmed)
  }
  # Minor data adjustments and column renames. The date also needs to be dated
  # back one, since the columns we use report previous day hospitalizations.
  data %>%
    mutate(
      geo_value = tolower(state),
      time_value = date - 1L,
      hhs = hhs,
      .keep = "none"
    ) %>%
    # API seems to complete state level with 0s in some cases rather than NAs.
    # Get something sort of compatible with that by summing to national with
    # na.omit = TRUE. As otherwise we have some NAs from probably territories
    # propagated to US level.
    append_us_aggregate("hhs")
}

#' Get the NHSN data archive from S3
#'
#' If you want to avoid downloading the archive from S3 every time, you can
#' call `get_s3_object_last_modified` to check if the archive has been updated
#' since the last time you downloaded it.
#'
#' @param disease_name The name of the disease ("nhsn_covid" or "nhsn_flu")
#' @return An epi_archive of the NHSN data.
get_nhsn_data_archive <- function(disease = c("covid", "flu", "rsv")) {
  disease <- arg_match(disease)
  get_cast_api_all_geos(
    source = "nhsn",
    signal = glue::glue("confirmed_admissions_{disease}_ew"),
    report_time_query = glue::glue("<={Sys.Date()}")
  ) %>%
    select(geo_value, time_value, version, value) %>%
    as_epi_archive(compactify = TRUE)
}


#' Fetch NHSN inpatient bed count and occupancy archives.
#'
#' Returns an `epi_archive` with two columns: `inpatient_beds_ew` (total
#' inpatient beds, count) and `inpatient_beds_occupied_pct_ew` (occupied
#' inpatient beds, count — misnamed in NHSN; not a percentage), keyed on
#' `geo_value`, `time_value`, and `version`. Rows where occupied exceeds total
#' are set to NA as physically impossible data errors.
#' @export
get_nhsn_beds_archive <- function() {
  fetch_signal <- function(signal) {
    get_cast_api_all_geos(
      source = "nhsn",
      signal = signal,
      report_time_query = glue::glue("<={Sys.Date()}")
    ) %>%
      select(geo_value, time_value, version, value)
  }

  beds <- fetch_signal("inpatient_beds_ew") %>%
    rename(inpatient_beds_ew = value)

  beds_pct <- fetch_signal("inpatient_beds_occupied_pct_ew") %>%
    rename(inpatient_beds_occupied_pct_ew = value)

  beds %>%
    full_join(beds_pct, by = c("geo_value", "time_value", "version")) %>%
    mutate(
      inpatient_beds_occupied_pct_ew = ifelse(
        inpatient_beds_occupied_pct_ew > inpatient_beds_ew,
        NA_real_,
        inpatient_beds_occupied_pct_ew
      )
    ) %>%
    arrange(geo_value, time_value, version) %>%
    as_epi_archive(compactify = TRUE)
}


#' NSSP ED-visit percentage archive from the cast API, Wednesday-labeled.
#'
#' @param geo_types any of "state", "nation", "hhs". HHS regions are served both
#'   zero-filled and average-filled; the average-filled values are kept.
up_to_date_nssp_state_archive <- function(disease = c("covid", "influenza", "rsv"), geo_types = c("state", "nation")) {
  disease <- arg_match(disease)
  signal <- glue::glue("pct_ed_visits_{disease}")
  nssp <- get_cast_api_all_geos(source = "nssp", signal = signal, geo_types = setdiff(geo_types, "hhs"))
  if ("hhs" %in% geo_types) {
    hhs <- get_cast_api_all_geos(
      source = "nssp", signal = signal, geo_types = "hhs",
      columns = c("geo_value", "time_value", "value", "version", "fill_method")
    ) %>%
      filter(fill_method == "ave") %>%
      select(-fill_method)
    nssp <- bind_rows(nssp, hhs)
  }
  nssp %>%
    rename(nssp = value) %>%
    # End-of-week Saturday → midweek Wednesday shift, then snap to Wednesday.
    mutate(time_value = time_value - 3) %>%
    # NSSP publishes explicit NA values for non-reporting geos (wy through
    # version 2026-01-07, backfilled with real values on 2026-01-14). An NA
    # observation is not an observation: keeping the rows makes historical
    # as-of slices serve NAs that crash NA-intolerant forecasters
    # (cdc_baseline via propagate_samples) on replay, while dropping them makes
    # the geo absent for that period -- matching the pre-2026-06-24 behavior of
    # excluding wy outright. Current-date slices are unaffected (later real
    # versions supersede).
    filter(!is.na(nssp)) %>%
    mutate(time_value = floor_date(time_value, "week", week_start = 7) + 3) %>%
    as_epi_archive(compactify = TRUE)
}
