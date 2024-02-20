suppressPackageStartupMessages({
  library(pipeR)
  library(plotly)
  library(shiny)
  library(ggplot2)
  library(rlang)
  library(dplyr)
  library(purrr)
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
    ## TODO Only display raw error scores for now. We'd want to make sure we
    ## have scores available both raw and normalized by population, but that
    ## depends on the units our forecasts use, and if all models are
    ## population-normalized or just some.
    # left_join(POPULATION_DF, by = "geo_value") %>%
    # mutate(across(c(wis, ae), list(
    #   "count_scale" = function(x) x / 100e3 * population,
    #   "per_100k" = identity
    # ))) %>%
    # select(-wis, -ae) %>%
    # rename(wis = wis_count_scale, ae = ae_count_scale) %>%
    mutate(
      ahead = as.integer(target_end_date - forecast_date),
      forecaster = gsub(
        EXTERNAL_PREFIX,
        "",
        names(forecaster_options[forecaster_options == .env$forecaster]),
        fixed = TRUE
      )
    )
}

# Have loading function use the cache.
load_forecast_data <- memoise::memoise(load_forecast_data_raw, cache = cache)

#' create a data table for the shiny plot of the forecasters present
#' @description
prepare_forecaster_table <- function(selected_forecasters) {
  forecasters <- tar_read(forecaster_params_grid) %>%
    select(-id) %>%
    mutate(across(where(is.list), map, `%||%`, c(0, 7, 14))) %>%
    mutate(lags = paste(lags, sep = ",")) %>%
    group_by(parent_id) %>%
    mutate(ahead = toString(unique(ahead))) %>%
    ungroup() %>%
    distinct(parent_id, .keep_all = TRUE) %>%
    rename(name = parent_id) %>%
    select(name, everything())
  forecasters$present <- map_vec(paste0("score_", forecasters$name), \(x) x %in% selected_forecasters)
  return(forecasters)
}

#' create a data table for the shiny plot of the ensembles present
#' @description
prepare_ensemble_table <- function(selected_forecasters) {
  forecasters <- tar_read(ensemble_forecasters) %>%
    select(-id) %>%
    group_by(parent_id) %>%
    mutate(ahead = toString(unique(ahead))) %>%
    ungroup() %>%
    distinct(parent_id, .keep_all = TRUE) %>%
    rename(name = parent_id) %>%
    mutate(ensemble_params = paste(ensemble_params, sep = ",")) %>%
    mutate(forecaster_ids = paste(forecaster_ids, sep = ",")) %>%
    select(name, everything()) %>%
    select(-forecasters)
  forecasters$present <- map_vec(paste0("ensemble_score_", forecasters$name), \(x) x %in% selected_forecasters)
  return(forecasters)
}
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
          width = 3,
          selectInput("selected_forecasters",
            "Forecasters:",
            choices = forecaster_options,
            multiple = TRUE
          ),
          selectInput("baseline",
            "Baseline forecaster:",
            choices = forecaster_options,
            multiple = FALSE
          ),
          checkboxInput(
            "scale_by_baseline",
            "Scale by baseline forecaster",
            value = FALSE,
          ),
          radioButtons(
            "selected_metric",
            "Error metric:",
            c(
              "Mean WIS" = "wis",
              # "Mean WIS per 100k" = "wis_per_100k",
              "Mean AE" = "ae",
              # "Mean AE per 100k" = "ae_per_100k",
              "80%PI Coverage" = "cov_80"
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
          checkboxInput(
            "facets_share_scale",
            "Share y scale between subplots",
            value = TRUE,
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
          selectInput("excluded_aheads",
            "Exclude aheads:",
            choices = 1:28,
            multiple = TRUE
          )
        ),
        mainPanel(
          verticalLayout(
            plotlyOutput("main_plot", height = "90em"),
            h2("forecaster name -> parameters"),
            # textOutput("forecaster_param_title"),
            dataTableOutput("forecaster_table"),
            h2("ensemble name -> parameters"),
            dataTableOutput("ensemble_table")
          ),
          width = 8
        )
      )
    )
  },
  server = function(input, output, session) {
    filtered_scorecards_reactive <- reactive({
      agg_forecasters <- unique(c(input$selected_forecasters, input$baseline))
      if (length(agg_forecasters) == 0 ||
        all(agg_forecasters == "" | is.null(agg_forecasters) | is.na(agg_forecasters))
      ) {
        return(data.frame())
      }

      processed_evaluations_internal <- lapply(agg_forecasters, function(forecaster) {
        load_forecast_data(forecaster) %>>%
          filter(
            .data$forecast_date %>>% between(.env$input$selected_forecast_date_range[[1L]], .env$input$selected_forecast_date_range[[2L]]),
            .data$target_end_date %>>% between(.env$input$selected_target_end_date_range[[1L]], .env$input$selected_target_end_date_range[[2L]]),
            !.data$geo_value %in% c(.env$input$excluded_geo_values, "us"),
            !.data$ahead %in% .env$input$excluded_aheads
          )
      }) %>%
        bind_rows()
    })
    output$main_plot <- renderPlotly({
      input_df <- filtered_scorecards_reactive()
      if (nrow(input_df) == 0) {
        return()
      }

      # Normalize by baseline scores. This is not relevant for coverage, which is compared
      # to the nominal confidence level.
      if (input$scale_by_baseline && input$selected_metric != "cov_80") {
        # These merge keys are overkill; this should be fully specified by
        # c("forecast_date", "target_end_date", "geo_value")
        merge_keys <- c("forecast_date", "target_end_date", "ahead", "issue", "geo_value")
        # Load selected baseline
        baseline_scores <- load_forecast_data(input$baseline)[, c(merge_keys, input$selected_metric)]

        baseline_scores$score_baseline <- baseline_scores[[input$selected_metric]]
        baseline_scores[[input$selected_metric]] <- NULL

        # Add on reference scores from baseline forecaster.
        # Note that this drops any scores where there isn't a corresponding
        # baseline value. If a forecaster and a baseline cover
        # non-overlapping dates or use different aheads, the forecaster will
        # not be shown.
        input_df <- inner_join(
          input_df, baseline_scores,
          by = merge_keys, suffix = c("", "")
        )
        # Scale score by baseline forecaster
        input_df[[input$selected_metric]] <- input_df[[input$selected_metric]] / input_df$score_baseline
      }


      x_tick_angle <- list(tickangle = -30)
      facet_x_tick_angles <- setNames(rep(list(x_tick_angle), 10), paste0("xaxis", 1:10))
      scale_type <- ifelse(input$facets_share_scale, "fixed", "free_y")

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
            cov_80 = 0.80,
            # Avoid https://github.com/plotly/plotly.R/issues/1947 by using NA
            # default and na.rm=TRUE rather than numeric(0L) default
            NA_real_
          ),
          na.rm = TRUE
        )) %>>%
        # Use scatterplot or lines depending on the x var.
        {
          if (input$x_var %in% c(input$facet_vars, "geo_value", "forecaster", "ahead")) {
            scale_factor_fcast <- length(input$selected_forecasters) * 2
            scale_factor_facet <- length(input$facet_vars) * 5
            scale_factor_geo <- ("geo_value" %in% input$facet_vars) * (60 - length(input$excluded_geo_values))
            scale_factor_geo_x <- ("geo_value" %in% input$x_var) * 10
            scale_factor_facet_fcast <- ifelse("forecaster" %in% input$facet_vars, length(input$selected_forecasters), 0) * 5
            scale_factor <- scale_factor_geo + scale_factor_geo_x + scale_factor_fcast + scale_factor_facet + scale_factor_facet_fcast

            max_size <- 5
            dynamic_size <- ((0.97 ^ scale_factor) + 0.2) * max_size

            . + geom_point(aes(size = n)) +
              scale_size_area(max_size = dynamic_size) +
              expand_limits(size = 0) + geom_line()
          } else {
            . + geom_line()
          }
        } %>>%
        # Create subplots if requested
        `+`(if (length(input$facet_vars) == 0L) {
          theme()
        } else if (length(input$facet_vars) == 1L) {
          facet_wrap(input$facet_vars, scales = scale_type)
        } else {
          facet_grid(as.formula(paste0(input$facet_vars[[1L]], " ~ ", paste(collapse = " + ", input$facet_vars[-1L]))), scales = scale_type)
        }) %>>%
        # Make subplots close together
        `+`(
          theme(panel.spacing.x = unit(1, "mm"), panel.spacing.y = unit(0.5, "mm"))
        ) %>>%
        ggplotly() %>>% {
          inject(layout(., hovermode = "x unified", legend = list(orientation = "h", title = list(text = "forecaster")), xaxis = x_tick_angle, !!!facet_x_tick_angles))
        }
    })
    output$forecaster_table <- renderDataTable(
      prepare_forecaster_table(input$selected_forecasters)
    )
    output$ensemble_table <- renderDataTable(
      prepare_ensemble_table(input$selected_forecasters)
    )
  }
)
