#' Flu surveillance auxiliary sources: FluSurv-NET and ILI+.
#'
#' Both come from the Epidata v5 archive API through `epidatr::epidata_archive`.
#' Only the lab percent positive before the 2016/17 season comes from the static
#' FluView CSVs in `aux_data/flusion_data`, because v5 does not have it.

#' Fetch one weekly signal from an Epidata v5 archive source.
#'
#' The source labels each week by its Saturday. `time_value` is the Sunday that
#' starts the week, and `version` is the report date.
fetch_epidata_v5_weekly <- function(source, signal, geo_type, geo_values = "*") {
  retry_fn(
    max_attempts = 10,
    wait_seconds = 1,
    fn = epidatr::epidata_archive,
    source = source,
    signals = signal,
    geo_type = geo_type,
    geo_values = geo_values
  ) %>%
    transmute(
      geo_value,
      time_value = reference_time - 6L,
      version = as.Date(report_time),
      value
    ) %>%
    spoof_backfill_versions()
}

#' Move bulk-loaded first vintages to `version = time_value`.
#'
#' Epidata v5 started to record some sources years after their first weeks. For
#' those weeks, the first vintage is a bulk load, not a real-time report. At its
#' true report date, a bulk load puts many weeks into one version, and
#' revision-aware training then gets almost no rows from them. This function
#' gives such a first vintage the faux version `version = time_value`, which is
#' the convention for static history. A first vintage is a bulk load when its
#' report date is more than `max_lag_days` after the end of the week.
spoof_backfill_versions <- function(weekly_dt, max_lag_days = 28L) {
  weekly_dt %>%
    group_by(geo_value, time_value) %>%
    mutate(
      version = if_else(
        version == min(version) & as.integer(version - (time_value + 6L)) > max_lag_days,
        time_value,
        version
      )
    ) %>%
    ungroup()
}

#' Keep New York without New York City as `ny`.
#'
#' The v5 lab percent positive has only `ny_minus_nyc`. ILINet also has `ny` and
#' `nyc`, which are removed so that both halves of ILI+ use the same region.
use_ny_minus_nyc <- function(weekly_dt) {
  weekly_dt %>%
    filter(geo_value %nin% c("ny", "nyc")) %>%
    mutate(geo_value = if_else(geo_value == "ny_minus_nyc", "ny", geo_value))
}

#' Read lab percent positive from the FluView CSVs for weeks before `before`.
#'
#' Rows get the faux version `version = time_value`. The "Combined" files cover
#' the seasons before 2015/16, and the clinical lab files cover 2015/16 onward.
read_fluview_positivity_csv <- function(before) {
  state_abb <- setNames(
    tolower(c(state.abb, "DC", "PR", "VI")),
    c(state.name, "District of Columbia", "Puerto Rico", "Virgin Islands")
  )
  filenames <- c(
    paste0("WHO_NREVSS_Combined_prior_to_2015_16_", c("Nation", "HHS", "State"), ".csv"),
    paste0("WHO_NREVSS_Clinical_Labs_", c("Nation", "HHS", "State"), ".csv")
  )
  filenames %>%
    purrr::map(\(filename) {
      readr::read_csv(
        here::here("aux_data", "flusion_data", filename),
        skip = 1,
        col_types = readr::cols(.default = "c")
      ) %>%
        select(region_type = "REGION TYPE", region = "REGION", epiyear = "YEAR", epiweek = "WEEK", value = "PERCENT POSITIVE")
    }) %>%
    bind_rows() %>%
    mutate(
      geo_value = case_when(
        region_type == "National" ~ "us",
        region_type == "HHS Regions" ~ str_remove(region, "^Region "),
        region_type == "States" ~ unname(state_abb[region])
      ),
      time_value = MMWRweek2Date(as.integer(epiyear), as.integer(epiweek), 1L),
      version = time_value,
      value = suppressWarnings(as.numeric(value))
    ) %>%
    filter(!is.na(geo_value), !is.na(value), time_value < before) %>%
    select(geo_value, time_value, version, value)
}

#' Combine weighted ILI and lab percent positive into ILI+.
#'
#' Each input has the columns `geo_value`, `time_value`, `version` and `value`.
#' The merge carries the last vintage of each input forward, so every version
#' of the output uses the data that was available at that version.
combine_ili_plus <- function(wili_dt, positivity_dt) {
  wili <- wili_dt %>%
    rename(wili = value) %>%
    as_epi_archive(compactify = TRUE)
  positivity <- positivity_dt %>%
    rename(pct_positive = value) %>%
    as_epi_archive(compactify = TRUE)
  epix_merge(wili, positivity, sync = "locf")$DT %>%
    as_tibble() %>%
    mutate(hhs = pct_positive * wili / 100) %>%
    filter(!is.na(hhs)) %>%
    select(geo_value, time_value, version, hhs)
}

gen_ili_data <- function() {
  # State ILINet has only the unweighted `ili` signal.
  wili <- bind_rows(
    fetch_epidata_v5_weekly("fluview_ilinet", "wili", "nation"),
    fetch_epidata_v5_weekly("fluview_ilinet", "wili", "hhs"),
    fetch_epidata_v5_weekly("fluview_ilinet", "ili", "state")
  ) %>%
    use_ny_minus_nyc()
  positivity_v5 <- c("nation", "hhs", "state") %>%
    purrr::map(\(geo_type) fetch_epidata_v5_weekly("fluview_resp_lab_clinical", "pct_positive", geo_type)) %>%
    bind_rows() %>%
    use_ny_minus_nyc()
  positivity <- bind_rows(
    read_fluview_positivity_csv(before = min(positivity_v5$time_value)),
    positivity_v5
  )
  combine_ili_plus(wili, positivity) %>%
    mutate(
      agg_level = case_when(
        geo_value == "us" ~ "nation",
        grepl("^[0-9]+$", geo_value) ~ "hhs_region",
        TRUE ~ "state"
      ),
      epiyear = epiyear(time_value),
      epiweek = epiweek(time_value),
      season = convert_epiweek_to_season(epiyear, epiweek),
      season_week = convert_epiweek_to_season_week(epiyear, epiweek),
      source = "ILI+"
    ) %>%
    as_epi_archive(compactify = TRUE)
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
  # New York is the sum of the Albany (10580) and Rochester (40380) MSAs. The
  # two sites revise on different dates, so the merge carries the last vintage
  # of each site forward before the sum.
  ny_sites <- fetch_epidata_v5_weekly("flusurv", "rate_overall", "msa", c("10580", "40380")) %>%
    split(.$geo_value) %>%
    purrr::map(\(site_dt) {
      site_dt %>%
        rename(!!paste0("rate_", site_dt$geo_value[[1]]) := value) %>%
        mutate(geo_value = "ny") %>%
        as_epi_archive(compactify = TRUE)
    })
  ny <- epix_merge(ny_sites[["10580"]], ny_sites[["40380"]], sync = "locf")$DT %>%
    as_tibble() %>%
    transmute(geo_value, time_value, version, value = rate_10580 + rate_40380, agg_level = "state")
  flusurv_all <- bind_rows(
    fetch_epidata_v5_weekly("flusurv", "rate_overall", "nation") %>%
      mutate(agg_level = "nation"),
    fetch_epidata_v5_weekly(
      "flusurv",
      "rate_overall",
      "state",
      c("ca", "co", "ct", "ga", "md", "mi", "mn", "nm", "oh", "or", "tn", "ut")
    ) %>%
      mutate(agg_level = "state"),
    ny
  ) %>%
    rename(hosp_rate = value) %>%
    drop_na() %>%
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
  # This drop_na() is the *effective* time bound on flusurv, not the live v5
  # fetch above: adj_factor only exists for the seasons covered by the static
  # aux_data/flusion_data/flu_burden.csv + us_pop.csv (2011-2020), so every
  # flusurv row outside those seasons is dropped here (max time_value ends up
  # ~2020-04-22).
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
