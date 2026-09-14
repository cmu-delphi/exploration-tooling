EPIDATA_V5_URL <- "https://delphi.cmu.edu/epidata/v5"

build_cast_api_query <- function(
  source = c("nssp", "nhsn"),
  signal = NULL,
  geo_type = c("state", "nation"),
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

calculate_burden_adjustment <- function(flusurv_latest) {
  # get burden data
  burden <- readr::read_csv(here::here("aux_data", "flusion_data", "flu_burden.csv"), show_col_types = FALSE) %>%
    separate(Season, into = c("StartYear", "season"), sep = "-") %>%
    select(season, contains("Estimate")) %>%
    mutate(season = as.double(season)) %>%
    mutate(
      season = paste0(
        as.character(season - 1),
        "/",
        substr(season, 3, 4)
      )
    )
  # get population data
  us_population <- readr::read_csv(here::here("aux_data", "flusion_data", "us_pop.csv"), show_col_types = FALSE) %>%
    rename(us_pop = POPTOTUSA647NWDB) %>%
    mutate(season = year(DATE)) %>%
    filter((season >= 2011) & (season <= 2020)) %>%
    select(season, us_pop) %>%
    mutate(season = paste0(as.character(season - 1), "/", substr(season, 3, 4)))
  # renormalize so that the total burden according to hhs matches the total
  # burden according to flusurv
  flusurv_latest %>%
    filter((geo_value == "us") & (start_year >= 2011) & (start_year <= 2020)) %>%
    group_by(season) %>%
    summarise(total_hosp_rate = sum(hosp_rate, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(burden, by = "season") %>%
    left_join(us_population, by = "season") %>%
    mutate(burden_est = total_hosp_rate * us_pop / 100000) %>%
    mutate(adj_factor = `Hospitalizations Estimate` / burden_est) %>%
    select(season, adj_factor)
}

generate_flusurv_adjusted <- function(day_of_week = 1) {
  flusurv_all <- pub_flusurv(
    locations = "CA,CO,CT,GA,MD,MI,MN,NM,NY_albany,NY_rochester,OH,OR,TN,UT,network_all",
    issues = epirange(123401, 345601)
  ) %>%
    select(geo_value = location, time_value = epiweek, hosp_rate = rate_overall, version = issue) %>%
    drop_na() %>%
    mutate(
      agg_level = case_when(
        geo_value == "network_all" ~ "nation",
        TRUE ~ "state"
      )
    ) %>%
    mutate(
      geo_value = if_else(agg_level == "nation", str_replace_all(geo_value, "network_all", "us"), tolower(geo_value))
    ) %>%
    mutate(
      geo_value = if_else(
        geo_value %in% c("ny_rochester", "ny_albany"),
        "ny",
        geo_value
      )
    )
  # sum the two ny regions and reappend to the original dataframe
  flusurv_all <- flusurv_all %>%
    filter(geo_value == "ny") %>%
    group_by(time_value, version) %>%
    summarize(
      geo_value = first(geo_value),
      agg_level = first(agg_level),
      hosp_rate = sum(hosp_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    bind_rows(
      flusurv_all %>% filter(geo_value != "ny")
    ) %>%
    arrange(geo_value, time_value, version)
  flusurv_all <-
    flusurv_all %>%
    mutate(
      epiyear = epiyear(time_value),
      epiweek = MMWRweek(time_value)$MMWRweek
    ) %>%
    left_join(
      (.) %>%
        distinct(epiyear, epiweek) %>%
        mutate(season = convert_epiweek_to_season(epiyear, epiweek)) %>%
        mutate(
          season_week = convert_epiweek_to_season_week(epiyear, epiweek),
          time_value = MMWRweek2Date(epiyear, epiweek, day_of_week)
        )
    ) %>%
    as_epi_archive(compactify = TRUE)
  # create a latest epi_df
  flusurv_all_latest <- flusurv_all %>%
    epix_as_of(version = max(.$DT$version)) %>%
    as_tibble() %>%
    mutate(start_year = as.numeric(substr(season, 1, 4)))
  adj_factor <- calculate_burden_adjustment(flusurv_all_latest)
  # This drop_na() is the *effective* time bound on flusurv, not the live
  # pub_flusurv() fetch above: adj_factor only exists for the seasons covered by
  # the static aux_data/flusion_data/flu_burden.csv + us_pop.csv (2011-2020), so
  # every flusurv row outside those seasons is dropped here (max time_value ends
  # up ~2020-04-22). Downstream (nhsn_prod_archive in flu_hosp_prod.R) folds these
  # rows into the training archive as faux-versioned history and asserts they
  # predate the forecast window. That assertion is really guarding against
  # someone extending flu_burden.csv past 2020 -- new flusurv issues alone can't
  # push data into the window while this cap holds.
  flusurv_lat <- flusurv_all$DT %>%
    left_join(adj_factor, by = "season") %>%
    drop_na() %>%
    mutate(adj_hosp_rate = hosp_rate * adj_factor, source = "flusurv")
  flusurv_lat %>%
    mutate(
      geo_value = if_else(geo_value %in% c("ny_rochester", "ny_albany"), "ny", geo_value)
    ) %>%
    group_by(geo_value, time_value, version, agg_level) %>%
    summarise(
      hosp_rate = mean(hosp_rate, na.rm = TRUE),
      adj_factor = mean(adj_factor, na.rm = TRUE),
      adj_hosp_rate = mean(adj_hosp_rate, na.rm = TRUE),
      epiyear = first(epiyear),
      epiweek = first(epiweek),
      season = first(season),
      season_week = first(season_week),
      .groups = "drop"
    ) %>%
    as_epi_archive(compactify = TRUE)
}


process_who_nrevss <- function(filename1, filename2, filename3) {
  clinical_lab_pos <- readr::read_csv(
    here::here("aux_data", "flusion_data", filename1),
    skip = 1,
    show_col_types = FALSE
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "PERCENT POSITIVE")
  combined_pos <- readr::read_csv(
    here::here("aux_data", "flusion_data", filename2),
    skip = 1,
    show_col_types = FALSE
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "PERCENT POSITIVE")
  pos_state <- bind_rows(clinical_lab_pos, combined_pos)
  ili_state <- readr::read_csv(
    here::here("aux_data", "flusion_data", filename3),
    skip = 1,
    show_col_types = FALSE
  ) %>%
    select("REGION TYPE", "REGION", "YEAR", "WEEK", "% WEIGHTED ILI", "%UNWEIGHTED ILI")
  merge(pos_state, ili_state, by = c("REGION TYPE", "REGION", "YEAR", "WEEK")) %>%
    mutate(across(all_of("PERCENT POSITIVE"), as.numeric)) %>%
    mutate(across(any_of("% UNWEIGHTED ILI"), as.numeric)) %>%
    mutate(across(any_of("%UNWEIGHTED ILI"), as.numeric)) %>%
    mutate(across(any_of("% WEIGHTED ILI"), as.numeric)) %>%
    as_tibble()
}

gen_ili_data <- function(default_day_of_week = 1) {
  ili_plus_nation <- process_who_nrevss(
    "WHO_NREVSS_Clinical_Labs_Nation.csv",
    "WHO_NREVSS_Combined_prior_to_2015_16_Nation.csv",
    "ILINet_Nation.csv"
  )
  ili_plus_HHS <- process_who_nrevss(
    "WHO_NREVSS_Clinical_Labs_HHS.csv",
    "WHO_NREVSS_Combined_prior_to_2015_16_HHS.csv",
    "ILINet_HHS.csv"
  )
  ili_plus_state <- process_who_nrevss(
    "WHO_NREVSS_Clinical_Labs_State.csv",
    "WHO_NREVSS_Combined_prior_to_2015_16_State.csv",
    "ILINet_State.csv"
  ) %>%
    mutate(`% WEIGHTED ILI` = `%UNWEIGHTED ILI`)

  ili_plus <- bind_rows(ili_plus_HHS, ili_plus_nation, ili_plus_state) %>%
    mutate(across(c(`PERCENT POSITIVE`, `% WEIGHTED ILI`), as.numeric)) %>%
    select(-`%UNWEIGHTED ILI`) %>%
    mutate(value = `PERCENT POSITIVE` * `% WEIGHTED ILI` / 100, source = "ILI+") %>%
    rename(agg_level = `REGION TYPE`, geo_value = REGION) %>%
    mutate(agg_level = str_replace_all(agg_level, "HHS Regions", "hhs_region")) %>%
    mutate(agg_level = str_replace_all(agg_level, "National", "nation")) %>%
    mutate(agg_level = str_replace_all(agg_level, "States", "state")) %>%
    mutate(
      geo_value = if_else(agg_level == "hhs_region", str_replace_all(geo_value, "Region (\\d+)", "\\1"), geo_value)
    ) %>%
    mutate(geo_value = if_else(agg_level == "nation", str_replace_all(geo_value, "X", "us"), geo_value)) %>%
    rename(epiyear = YEAR, epiweek = WEEK) %>%
    left_join(
      (.) %>%
        distinct(epiyear, epiweek) %>%
        mutate(season = convert_epiweek_to_season(epiyear, epiweek)) %>%
        mutate(
          season_week = convert_epiweek_to_season_week(epiyear, epiweek),
          time_value = MMWRweek2Date(epiyear, epiweek, default_day_of_week),
          version = time_value
        )
    )
  # map names to lower case
  name_map <- tibble(abb = state.abb, name = state.name) %>%
    bind_rows(
      # fmt: skip
      tribble(
        ~name, ~abb,
        "District of Columbia", "DC",
        "American Samoa", "AS",
        "Guam", "GU",
        "Northern Mariana Islands", "MP",
        "Puerto Rico", "PR",
        "Virgin Islands", "VI",
        "Trust Territories", "TT",
        "us", "US",
        "New York City", "ny"
      )
    ) %>%
    mutate(abb = tolower(abb))
  ili_states <- ili_plus %>%
    filter(agg_level == "state") %>%
    left_join(name_map, by = join_by(geo_value == name)) %>%
    select(
      geo_value = abb,
      time_value,
      version,
      agg_level,
      value,
      season,
      season_week,
      `PERCENT POSITIVE`,
      `% WEIGHTED ILI`,
      source,
      epiyear,
      epiweek
    )

  # aggregate NYC and NY state
  ili_plus <- ili_states %>%
    filter(geo_value == "ny") %>%
    group_by(time_value, version) %>%
    summarize(
      geo_value = first(geo_value),
      agg_level = first(agg_level),
      season = first(season),
      season_week = first(season_week),
      `PERCENT POSITIVE` = mean(`PERCENT POSITIVE`, na.rm = TRUE),
      `% WEIGHTED ILI` = mean(`% WEIGHTED ILI`, na.rm = TRUE),
      source = first(source),
      epiweek = first(epiweek),
      epiyear = first(epiyear),
      .groups = "drop"
    ) %>%
    bind_rows(
      ili_states %>% filter(geo_value != "ny"),
      ili_plus %>% filter(agg_level != "state")
    ) %>%
    rename(hhs = value) %>%
    as_epi_archive(compactify = TRUE)
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
  nhsn_state <- get_cast_api_data(
    source = "nhsn",
    signal = glue::glue("confirmed_admissions_{disease}_ew"),
    geo_type = "state",
    columns = c("geo_value", "time_value", "value", "version"),
    report_time_query = glue::glue("<={Sys.Date()}")
  )
  nhsn_nation <- get_cast_api_data(
    source = "nhsn",
    signal = glue::glue("confirmed_admissions_{disease}_ew"),
    geo_type = "nation",
    columns = c("geo_value", "time_value", "value", "version"),
    report_time_query = glue::glue("<={Sys.Date()}")
  )
  nhsn_data <- nhsn_state %>%
    rbind(nhsn_nation) %>%
    select(geo_value, time_value, version, value) %>%
    mutate(
      geo_value = tolower(geo_value),
      # Need to center the time_value on Wednesday of the week (rather than Saturday).
      version = as.Date(version)
    ) %>%
    # Ensure uniqueness and convert to epi_archive
    arrange(geo_value, time_value, version) %>%
    distinct(geo_value, time_value, version, .keep_all = TRUE) %>%
    as_epi_archive(compactify = TRUE)
  nhsn_data
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
  fetch_nhsn_signal <- function(signal, geo_type) {
    get_cast_api_data(
      source = "nhsn",
      signal = signal,
      geo_type = geo_type,
      columns = c("geo_value", "time_value", "value", "version"),
      report_time_query = glue::glue("<={Sys.Date()}")
    ) %>%
      select(geo_value, time_value, version, value) %>%
      mutate(geo_value = tolower(geo_value), version = as.Date(version)) %>%
      arrange(geo_value, time_value, version) %>%
      distinct(geo_value, time_value, version, .keep_all = TRUE)
  }

  beds <- bind_rows(
    fetch_nhsn_signal("inpatient_beds_ew", "state"),
    fetch_nhsn_signal("inpatient_beds_ew", "nation")
  ) %>%
    rename(inpatient_beds_ew = value)

  beds_pct <- bind_rows(
    fetch_nhsn_signal("inpatient_beds_occupied_pct_ew", "state"),
    fetch_nhsn_signal("inpatient_beds_occupied_pct_ew", "nation")
  ) %>%
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


up_to_date_nssp_state_archive <- function(disease = c("covid", "influenza", "rsv")) {
  disease <- arg_match(disease)
  nssp_national <- get_cast_api_data(
    source = "nssp",
    signal = glue::glue("pct_ed_visits_{disease}"),
    geo_type = "nation",
    columns = c("geo_value", "time_value", "value", "version"),
  )
  nssp_state <- get_cast_api_data(
    source = "nssp",
    signal = glue::glue("pct_ed_visits_{disease}"),
    geo_type = "state",
    columns = c("geo_value", "time_value", "value", "version"),
  )
  nssp_data <- nssp_state %>%
    rbind(nssp_national) %>%
    select(geo_value, time_value, nssp = value, version) %>%
    mutate(
      geo_value = tolower(geo_value),
      # Need to center the time_value on Wednesday of the week (rather than Saturday).
      time_value = time_value - 3,
      version = as.Date(version)
    ) %>%
    # Ensure uniqueness and convert to epi_archive
    arrange(geo_value, time_value, version) %>%
    distinct(geo_value, time_value, version, .keep_all = TRUE) %>%
    # NSSP publishes explicit NA values for non-reporting geos (wy through
    # version 2026-01-07, backfilled with real values on 2026-01-14). An NA
    # observation is not an observation: keeping the rows makes historical
    # as-of slices serve NAs that crash NA-intolerant forecasters
    # (cdc_baseline via propagate_samples) on replay, while dropping them makes
    # the geo absent for that period -- matching the pre-2026-06-24 behavior of
    # excluding wy outright. Current-date slices are unaffected (later real
    # versions supersede).
    filter(!is.na(nssp))

  # Complete the rest of the conversion.
  nssp_data %>%
    # End of week to midweek correction.
    mutate(time_value = floor_date(time_value, "week", week_start = 7) + 3) %>%
    as_epi_archive(compactify = TRUE)
}
