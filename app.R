suppressMessages({
  library(pipeR)
  library(plotly)
  library(shiny)
  library(ggplot2)
  library(rlang)
})

POPULATION_DF <-
  covidcast::state_census %>>%
  transmute(geo_value = tolower(ABBR), population = POPESTIMATE2019)

MIN_RANGE_DATE <- as.Date("2020-01-01")
MAX_RANGE_DATE <- Sys.Date()


# Set application-level caching location. Stores up to 1GB. Removes
# least recently used objects first.
shinyOptions(cache = cachem::cache_mem(max_size = 1000 * 1024^2, evict = "lru"))
cache <- getShinyOption("cache")

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
        group_by(across(all_of(input$x_var)), across(all_of(input$facet_vars)), forecaster) %>>%
        ## TODO Could make the metric a faceting option with free_y
        summarize(across(input$selected_metric, list(mean = mean)), n = n(), .groups = "drop") %>>%
        (~plot.df) %>>%
        ggplot(aes_string(input$x_var, paste0(input$selected_metric, "_mean"), colour = "forecaster")) %>>%
        `+`(expand_limits(y = if (grepl("cov_", paste0(input$selected_metric, "_mean"))) c(0, 1) else 0)) %>>%
        `+`(geom_hline(
          linetype = "dashed",
          ## It's natural here to have the default
          yintercept = switch(input$selected_metric,
            ic80 = 0.80,
            ## (Avoid
            ## https://github.com/plotly/plotly.R/issues/1947
            ## by using NA default and na.rm=TRUE
            ## rather than numeric(0L) default)
            NA_real_
          ),
          na.rm = TRUE
        )) %>>%
        {
          if (input$x_var %in% c(input$facet_vars, "geo_value", "forecaster") || range(plot.df[["n"]]) %>>% {
            .[[2L]] > 1.2 * .[[1L]]
          }) {
            . + geom_point(aes(size = n)) + expand_limits(size = 0)
          } else {
            . + geom_line()
          }
        } %>>%
        `+`(if (length(input$facet_vars) == 0L) {
          theme()
        } else if (length(input$facet_vars) == 1L) {
          facet_wrap(input$facet_vars)
        } else {
          facet_grid(as.formula(paste0(input$facet_vars[[1L]], " ~ ", paste(collapse = " + ", input$facet_vars[-1L]))))
        }) %>>%
        ggplotly() %>%
        layout(hovermode = "x unified")
    })
  }
)
