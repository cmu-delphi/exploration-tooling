print("########################################")
print("Starting at")
print(Sys.time())
print("########################################")
# the crontab that is used to run this is:
# 31 0-23 * * 2,3,5 cd /path/to/root/of/this/project && direnv exec /path/to/root/of/this/project /usr/bin/Rscript scripts/nhsn_download.R >> cache/nhsn_download.log 2>&1
suppressPackageStartupMessages(source(here::here("R", "load_all.R")))
library(readr)
library(epiprocess)
library(qs)
save_folder <- here::here("cache")
dir.create(save_folder)
dir.create(file.path(save_folder, "raw_data"))

# read and immediately save the raw version
epi_data_raw <- readr::read_csv("https://data.cdc.gov/resource/ua7e-t2fy.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
epi_data_raw_prelim <- readr::read_csv("https://data.cdc.gov/resource/mpgq-jmmr.csv?$limit=20000&$select=weekendingdate,jurisdiction,totalconfc19newadm,totalconfflunewadm")
raw_file <- glue::glue("nhsn_data_{Sys.time()}") %>%
  gsub(" ", "_", .) %>%
  gsub(":", "-", .)
raw_path <- raw_file %>%
  file.path(save_folder, "raw_data", .) %>%
  paste0(".parquet")
qs::qsave(epi_data_raw, raw_path)
s3save(epi_data_raw, object = paste0(raw_file, ".rds"), bucket = "forecasting-team-data")
s3save(epi_data_raw_prelim, object = paste0(raw_file, "_prelim", ".rds"), bucket = "forecasting-team-data")

file_path <- file.path(save_folder, "nhsn_data.parquet")
epi_data_to_archive <- function(epi_data_raw) {
  epi_data <- epi_data_raw %>%
    mutate(
      epiweek = epiweek(weekendingdate),
      epiyear = epiyear(weekendingdate)
    ) %>%
    left_join(
      (.) %>%
        distinct(epiweek, epiyear) %>%
        mutate(
          season = convert_epiweek_to_season(epiyear, epiweek),
          season_week = convert_epiweek_to_season_week(epiyear, epiweek)
        ),
      by = c("epiweek", "epiyear")
    ) %>%
    mutate(
      geo_value = tolower(jurisdiction),
      time_value = as.Date(weekendingdate),
      nhsn_covid = totalconfc19newadm,
      nhsn_flu = totalconfflunewadm
    ) %>%
    select(-weekendingdate, -jurisdiction, -starts_with("totalconf")) %>%
    pivot_longer(cols = starts_with("nhsn"), names_to = "disease") %>%
    filter(!is.na(value)) %>%
    mutate(version = Sys.Date()) %>%
    relocate(geo_value, disease, time_value, version)
  epi_data %>% as_epi_archive(other_keys = c("disease"))
}
epi_arch <- epi_data_to_archive(epi_data_raw)
epi_arch_prelim <- epi_data_to_archive(epi_data_raw_prelim)
epi_arch_prelim
dedupe <- function(arch_1, arch_2) {
  arch_1$DT %>%
    bind_rows(arch_2$DT) %>%
    arrange(across(c(geo_value, time_value, all_of("disease"), version))) %>%
    epiprocess:::apply_compactify(c("geo_value", "time_value", "disease", "version")) %>%
    as_epi_archive(other_keys = "disease", compactify = TRUE)
}

# now merge with the already existing versions
earlier_results <- s3readRDS(object = "nhsn_archive.rds", bucket = "forecasting-team-data")
new_results_main <- dedupe(epi_arch, earlier_results)
new_results_prelim <- dedupe(earlier_results, epi_arch_prelim)
if (nrow(new_results_main$DT) > nrow(new_results_prelim$DT)) {
  new_archive <- new_results_main
} else {
  new_archive <- new_results_prelim
}

s3saveRDS(new_archive, object = "nhsn_archive.rds", bucket = "forecasting-team-data")
qs::qsave(new_archive, file_path)
print("########################################")
print("Script Finished at")
print(Sys.time())
print("########################################")
print()
print()
q()
new_results$DT %>%
  filter(version == "2024-11-20") %>%
  filter(time_value < "2024-11-09")
new_results$DT %>%
  filter(time_value %in% "2024-11-02") %>%
  filter(time_value < "2024-11-09")
new_results_prelim$DT %>%
  group_by(geo_value, time_value, disease) %>%
  filter(n() > 2) %>%
  select(disease, geo_value, time_value, version, value) %>%
  arrange(geo_value, disease, time_value, version) %>%
  mutate(diff = max(value) - min(value), rel_diff = (max(value) - min(value)) / first(value)) %>%
  print(n = 57)
new_results_prelim$DT %>%
  group_by(geo_value, time_value, disease) %>%
  filter(version >"2024-11-20")
new_results_prelim$DT %>%
  filter(time_value == max(time_value)) %>%
  as_tibble %>%
  select(disease, geo_value, time_value, version, value) %>%
  group_by(geo_value, time_value, disease) %>%
  mutate(diff = max(value) - min(value), rel_diff = (max(value) - min(value)) / first(value)) %>%
  arrange(rel_diff, geo_value, disease, time_value, version) %>%
  print(n=57)
  print(n=180)
