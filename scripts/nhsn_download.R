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

create_nhsn_data_archive()
