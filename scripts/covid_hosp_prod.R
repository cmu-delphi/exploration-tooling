# The COVID Hospitalization Production Forecasting Pipeline.
#
# Ran into some issues with targets:
#   https://github.com/ropensci/targets/discussions/666#discussioncomment-9050772

source("scripts/targets-common.R")


#' Get exclusions from a JSON file for a given date
#'
#' @param date A date
#' @param exclusions_json A JSON file with exclusions in the format:
#'
#'    {"exclusions": {"2024-03-24": "ak,hi"}}
get_exclusions <- function(date, exclusions_json = here::here("scripts", "geo_exclusions.json")) {
  s <- jsonlite::read_json(exclusions_json)$exclusions[[as.character(date)]]
  if (!is.null(s)) {
    return(s)
  }
  return("")
}

forecast_generation_date <- as.character(seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "1 week"))
geo_exclusions <- Vectorize(get_exclusions)(forecast_generation_date)

rlang::list2(
  tar_target(
    aheads,
    command = {
      c(1:7)
    }
  ),
  tar_map(
    values = tidyr::expand_grid(
      tibble(
        forecast_generation_date = forecast_generation_date,
        geo_exclusions = geo_exclusions
      )
    ),
    names = "forecast_generation_date",
    tar_target(
      hhs_latest_data,
      command = {
        epidatr::pub_covidcast(
          source = "hhs",
          signals = "confirmed_admissions_covid_1d",
          geo_type = "state",
          time_type = "day",
          geo_values = "*",
          time_values = epidatr::epirange(from = "2020-01-01", to = forecast_generation_date),
          as_of = forecast_generation_date,
          fetch_args = epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
        ) %>%
          select(geo_value, time_value, value, issue) %>%
          rename("hhs" := value) %>%
          rename(version = issue)
      }
    ),
    tar_target(
      forecast,
      command = {
        hhs_latest_data %>%
          as_epi_df() %>%
          smoothed_scaled(outcome = "hhs", ahead = aheads)
      },
      pattern = map(aheads)
    ),
    tar_target(
      forecast_with_exclusions,
      command = {
        forecast %>% filter(!geo_value %in% strsplit(geo_exclusions, ",")[[1]])
      }
    ),
    tar_target(
      notebook,
      command = {
        rmarkdown::render(
          "scripts/covid_hosp_prod.Rmd",
          output_file = here::here(
            "reports",
            sprintf("covid_hosp_prod_%s.html", forecast_generation_date)
          ),
          params = list(
            forecast = forecast
          )
        )
      }
    )
  )
)
