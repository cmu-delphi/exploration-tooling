# R6 refactor comparison script.
#
# This script is used to compare the old and new R6 refactor objects to ensure
# that the refactor did not change the forecast output. This script assumes that
# you:
#
# 1. Ran the covid_hosp_explore pipeline (or downloaded the objects using `make
#    download`)
# 2. Copied the cache objects to a new directory (e.g. `covid_hosp_explore
#    copy`)
# 3. Installed the new epiprocess branch
#    `renv::install("cmu-delphi/epiprocess@ds/r6-clean")`
# 4. Ran the covid_hosp_explore pipeline again (should take about 3.5 hours)
#
# Once that is done, you should be able to run the script below and find no
# differences in the forecasts.

library(dplyr)
library(magrittr)
library(purrr)
library(qs)
library(stringr)

df <- targets::tar_manifest()

# Keep only the forecaster prediction targets.
old_forecasts <- list.files("covid_hosp_explore copy/objects", full.names = TRUE) %>%
  keep(~ basename(.) %in% df$name) %>%
  keep(~ str_detect(., "forecast_")) %>%
  sort()
new_forecasts <- list.files("covid_hosp_explore/objects", full.names = TRUE) %>%
  keep(~ basename(.) %in% df$name) %>%
  keep(~ str_detect(., "forecast_")) %>%
  sort()

# Make sure the lists are the same length and the basenames match.
assertthat::assert_that(
  c(
    length(old_forecasts) == length(new_forecasts),
    basename(old_forecasts) == basename(new_forecasts)
  ) %>% all()
)

tib <- tibble::tibble(
  old_forecasts = old_forecasts,
  new_forecasts = new_forecasts,
  compare = map2_chr(old_forecasts, new_forecasts, function(x, y) {
    x <- qs::qread(x) %>% select(-true_value)
    y <- qs::qread(y) %>% select(-true_value)
    # If objects are not the same, `all.equal` returns a character vector that
    # can have `length` > 1 (for all the mismatches). In that case, we need to
    # collapse the output into a length-1 string so that `purrr::map2_chr`
    # accepts it.
    all.equal(x, y) %>%
      as.character() %>%
      paste(collapse = "; ")
  })
)
miss <- tib %>%
  filter(compare != "TRUE")

miss %>% filter(str_detect(old_forecasts, "forecast_"))

# x <- qread(miss %>% slice(6) %>% pull(old_forecasts))
# y <- qread(miss %>% slice(6) %>% pull(new_forecasts))
