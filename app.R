suppressMessages({
  library(pipeR)
  library(plotly)
  library(shiny)
  library(ggplot2)
  library(rlang)
  library(dplyr)
})

POPULATION_DF <-
  covidcast::state_census %>>%
  transmute(geo_value = tolower(ABBR), population = POPESTIMATE2019)

# Dates used for slider.
MIN_RANGE_DATE <- as.Date("2020-01-01")
MAX_RANGE_DATE <- Sys.Date()


# Set application-level caching location. Stores up to 1GB. Removes
# least recently used objects first.
CACHE_LIMIT_MB <- 1000
shinyOptions(cache = cachem::cache_mem(max_size = CACHE_LIMIT_MB * 1024^2, evict = "lru"))
cache <- getShinyOption("cache")

# Load a single score file of `targets` output. Do some scaling and renaming
# of error scores. Calculate `ahead`s.
load_forecast_data_raw <- function(forecaster) {
  inject(tar_read(!!forecaster)) %>%
    left_join(POPULATION_DF, by = "geo_value") %>%
    ## TODO Check what units our forecasts use.
    mutate(across(c(wis, ae), list(
      "count_scale" = function(x) x / 100e3 * population,
      "per_100k" = identity
    ))) %>%
    select(-wis, -ae) %>%
    rename(wis = wis_count_scale, ae = ae_count_scale) %>%
    mutate(
      ahead = as.integer(target_end_date - forecast_date),
      forecaster = forecaster
    ) %>%
    {
      .
    }
}

# Have loading function use the cache.
load_forecast_data <- memoise::memoise(load_forecast_data_raw, cache = cache)

#### Adapted from shiny-eval.R from cmu-delphi/hospitalization-forecaster

shinyApp(
  onStart = function() {
    enableBookmarking(store = "url")
  },
  ui = function(request) {
    fluidPage(
      titlePanel("Eval Summary Dashboard (no national)"),
      bookmarkButton(),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_forecasters",
            "Forecasters:",
            choices = forecaster_options,
            multiple = TRUE
          ),
          radioButtons(
            "selected_metric",
            "Metric:",
            c(
              "Mean WIS" = "wis",
              "Mean WIS per 100k" = "wis_per_100k",
              "Mean AE" = "ae",
              "Mean AE per 100k" = "ae_per_100k",
              "80%PI Coverage" = "ic80"
            )
          ),
          selectInput("x_var",
            "x var:",
            choices = c("forecaster", "ahead", "forecast_date", "target_end_date", "geo_value"),
            multiple = FALSE
          ),
          selectInput("facet_vars",
            "facet vars:",
            choices = c("forecaster", "ahead", "geo_value"),
            multiple = TRUE
          ),
          sliderInput("selected_forecast_date_range",
            "Forecast date range:",
            ## TODO: load the baseline to start and set forecast and target date ranges to
            ## be based on the baseline.
            min = MIN_RANGE_DATE,
            max = MAX_RANGE_DATE,
            value = range(c(MIN_RANGE_DATE, MAX_RANGE_DATE))
          ),
          sliderInput("selected_target_end_date_range",
            "Target end date range:",
            min = MIN_RANGE_DATE,
            max = MAX_RANGE_DATE,
            value = range(c(MIN_RANGE_DATE, MAX_RANGE_DATE))
          ),
          selectInput("excluded_geo_values",
            "Exclude geo values:",
            choices = setdiff(POPULATION_DF$geo_value, "us"),
            multiple = TRUE,
            selected = c("as", "gu", "mp", "vi")
          ),
        ),
        mainPanel(
          plotlyOutput("main_plot", height = "90em")
        )
      )
    )
  },
  server = function(input, output, session) {
    filtered_scorecards_reactive <- reactive({
      ## TODO this is just an experiment with `reactive`; it may or may not be a
      ## good idea. Might speed up computations with the same set of data to
      ## summarize when the summary is changed, but might also eat a bunch of
      ## extra memory.
      if (length(input$selected_forecasters) == 0) { return(data.frame()) }

      processed_evaluations_internal <- lapply(input$selected_forecasters, function(forecaster) {
          load_forecast_data(forecaster) %>>%
          filter(
            .data$forecast_date %>>% between(.env$input$selected_forecast_date_range[[1L]], .env$input$selected_forecast_date_range[[2L]]),
            .data$target_end_date %>>% between(.env$input$selected_target_end_date_range[[1L]], .env$input$selected_target_end_date_range[[2L]]),
            !.data$geo_value %in% c(.env$input$excluded_geo_values, "us")
          )
        }) %>%
        bind_rows()
    })
    output$main_plot <- renderPlotly({
      input_df <- filtered_scorecards_reactive()
      if (nrow(input_df) == 0) { return() }
      
      input_df %>>%
        # Aggregate scores over all geos
        group_by(across(all_of(input$x_var)), across(all_of(input$facet_vars)), forecaster) %>>%
        ## TODO Could make the metric a faceting option with free_y
        summarize(across(input$selected_metric, list(mean = mean)), n = n(), .groups = "drop") %>>%
        (~plot.df) %>>%
        # Select x and y vars to display
        # Use https://stackoverflow.com/a/53168593/14401472 to refer to x, y,
        # group by var/string
        ggplot(aes(!!sym(input$x_var), !!sym(paste0(input$selected_metric, "_mean")), colour = !!sym("forecaster"))) %>>%
        `+`(expand_limits(y = if (grepl("cov_", paste0(input$selected_metric, "_mean"))) c(0, 1) else 0)) %>>%
        # Add a horizontal reference line if plotting coverage
        `+`(geom_hline(
          linetype = "dashed",
          yintercept = switch(input$selected_metric,
            ic80 = 0.80,
            # Avoid https://github.com/plotly/plotly.R/issues/1947 by using NA
            # default and na.rm=TRUE rather than numeric(0L) default
            NA_real_
          ),
          na.rm = TRUE
        )) %>>%
        # Use scatterplot or lines depending on the x var
        {
          if (input$x_var %in% c(input$facet_vars, "geo_value", "forecaster", "ahead") || range(plot.df[["n"]]) %>>% {
            .[[2L]] > 1.2 * .[[1L]]
          }) {
            . + geom_point(aes(size = n)) + expand_limits(size = 0)
          } else {
            . + geom_line()
          }
        } %>>%
        # Create subplots if requested
        `+`(if (length(input$facet_vars) == 0L) {
          theme()
        } else if (length(input$facet_vars) == 1L) {
          facet_wrap(input$facet_vars)
        } else {
          facet_grid(as.formula(paste0(input$facet_vars[[1L]], " ~ ", paste(collapse = " + ", input$facet_vars[-1L]))))
        }) %>>%
        ggplotly() %>>%
        layout(hovermode = "x unified")
    })
  }
)
