#' Configuration parameters for flu hospitalization forecasting
#'
#' This file contains configuration parameters used by the flu hospitalization
#' forecasting pipeline.

#' Get flu forecaster parameters
#'
#' @param dummy_mode Boolean indicating whether to use dummy forecasters
#' @return A list of forecaster parameters
#' @export
get_flu_forecaster_params <- function() {
  out <- rlang::list2(
    # just the data, possibly population scaled; likely to run into troubles
    # because of the scales of the different sources
    scaled_pop_main = bind_rows(
      tidyr::expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        lags = list2(
          c(0, 7),
          c(0, 7, 14, 21),
        ),
        pop_scaling = FALSE,
        filter_source = "nhsn",
        filter_agg_level = "state",
        scale_method = "none",
        center_method = "median",
        nonlin_method = c("quart_root", "none"),
        n_training = Inf,
        drop_non_seasons = TRUE,
        keys_to_ignore = g_very_latent_locations
      ),
      tidyr::expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        lags = list2(
          c(0, 7),
          c(0, 7, 14, 21),
        ),
        pop_scaling = FALSE,
        filter_source = "nhsn",
        filter_agg_level = "state",
        scale_method = "quantile",
        center_method = "median",
        nonlin_method = "quart_root",
        n_training = Inf,
        drop_non_seasons = TRUE,
        keys_to_ignore = g_very_latent_locations
      )
    ),
    scaled_pop_data_augmented = tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      lags = list(
        c(0, 7, 14, 21),
        c(0, 7)
      ),
      pop_scaling = FALSE,
      scale_method = "quantile",
      center_method = "median",
      nonlin_method = c("quart_root", "none"),
      filter_source = "",
      filter_agg_level = "",
      n_training = Inf,
      drop_non_seasons = TRUE,
      keys_to_ignore = g_very_latent_locations
    ),
    ## # The covid forecaster, ported over to flu. Also likely to struggle with the
    ## # extra data
    # the thing to beat (a simplistic baseline forecast)
    flatline = tidyr::expand_grid(
      forecaster = "flatline_fc",
      filter_source = "nhsn",
      filter_agg_level = "state"
    ),
    # using exogenous variables
    scaled_pop_exogenous = bind_rows(
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        # since it's a list, this gets expanded out to a single one in each row
        extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7) # exogenous feature
          )
        ),
        pop_scaling = FALSE,
        scale_method = "quantile",
        center_method = "median",
        nonlin_method = "quart_root",
        filter_source = "nhsn",
        filter_agg_level = "state",
        n_training = Inf,
        drop_non_seasons = TRUE,
        keys_to_ignore = g_very_latent_locations,
      ),
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        extra_sources = list2(
          c("nssp", "google_symptoms"),
          c("nssp", "nwss"),
          c("nssp", "nwss_region"),
          c("google_symptoms", "nwss"),
          c("google_symptoms", "nwss_region"),
          c("nwss", "nwss_region")
        ),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # first feature
            c(0, 7) # second feature
          )
        ),
        pop_scaling = FALSE,
        scale_method = "quantile",
        center_method = "median",
        nonlin_method = "quart_root",
        filter_source = "nhsn",
        filter_agg_level = "state",
        n_training = Inf,
        drop_non_seasons = TRUE,
        keys_to_ignore = g_very_latent_locations,
      ),
      expand_grid(
        forecaster = "scaled_pop",
        trainer = "quantreg",
        extra_sources = list2(
          c("nssp", "google_symptoms", "nwss", "nwss_region"),
        ),
        lags = list2(
          list2(
            c(0, 7, 14, 21), # hhs
            c(0, 7), # nssp
            c(0, 7), # google symptoms
            c(0, 7), # nwss
            c(0, 7), # nwss_region
          )
        ),
        pop_scaling = FALSE,
        scale_method = "quantile",
        center_method = "median",
        nonlin_method = "quart_root",
        filter_source = "nhsn",
        filter_agg_level = "state",
        n_training = Inf,
        drop_non_seasons = TRUE,
        keys_to_ignore = g_very_latent_locations,
      )
    ),
    ## # another kind of baseline forecaster
    ## no_recent_quant = tidyr::expand_grid(
    ##   forecaster = "no_recent_outcome",
    ##   trainer = "quantreg",
    ##   scale_method = "quantile",
    ##   nonlin_method = "quart_root",
    ##   filter_source = "",
    ##   use_population = c(TRUE, FALSE),
    ##   use_density = c(TRUE, FALSE),
    ##   week_method = "sine",
    ##   keys_to_ignore = g_very_latent_locations
    ## ),
    no_recent_but_exogenous = bind_rows(
      expand_grid(
        forecaster = "no_recent_outcome",
        trainer = "quantreg",
        # since it's a list, this gets expanded out to a single one in each row
        extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region"),
        lags = list2(
          list2(
            # no hhs
            c(0, 7) # exogenous feature
          )
        ),
        scale_method = "quantile",
        nonlin_method = "quart_root",
        filter_source = c("", "nhsn"),
        use_population = TRUE,
        use_density = FALSE,
        week_method = "sine",
        n_training = Inf,
        keys_to_ignore = g_very_latent_locations
      ),
      # expand_grid(
      #   forecaster = "no_recent_outcome",
      #   trainer = "quantreg",
      #   extra_sources = list2(
      #     c("nssp", "google_symptoms"),
      #     c("nssp", "nwss"),
      #     c("nssp", "nwss_region"),
      #     c("google_symptoms", "nwss"),
      #     c("google_symptoms", "nwss_region"),
      #     c("nwss", "nwss_region")
      #   ),
      #   lags = list2(
      #     list2(
      #       # no hhs
      #       c(0, 7), # first feature
      #       c(0, 7) # second feature
      #     )
      #   ),
      #   scale_method = "quantile",
      #   nonlin_method = "quart_root",
      #   filter_source = c("", "nhsn"),
      #   use_population = TRUE,
      #   use_density = FALSE,
      #   week_method = "sine",
      #   n_training = Inf,
      #   keys_to_ignore = g_very_latent_locations
      # ),
      expand_grid(
        forecaster = "no_recent_outcome",
        trainer = "quantreg",
        extra_sources = list2(
          c("nssp", "google_symptoms", "nwss", "nwss_region"),
        ),
        lags = list2(
          list2(
            # no hhs
            c(0, 7), # nssp
            c(0, 7), # google symptoms
            c(0, 7), # nwss
            c(0, 7) # nwss_region
          )
        ),
        scale_method = "quantile",
        nonlin_method = "quart_root",
        filter_source = c("", "nhsn"),
        use_population = TRUE,
        use_density = FALSE,
        week_method = "sine",
        n_training = Inf,
        keys_to_ignore = g_very_latent_locations
      )
    ),
    scaled_pop_season = bind_rows(
      tidyr::expand_grid(
        forecaster = "scaled_pop_seasonal",
        trainer = "quantreg",
        lags = list(
          c(0, 7, 14, 21),
          c(0, 7)
        ),
        seasonal_method = list("flu", "indicator", "climatological"),
        pop_scaling = FALSE,
        train_residual = c(TRUE, FALSE),
        filter_source = "nhsn",
        filter_agg_level = "state",
        drop_non_seasons = c(TRUE, FALSE),
        n_training = Inf,
        seasonal_backward_window = 5,
        keys_to_ignore = g_very_latent_locations
      ),
      # Window-based seasonal method shouldn't drop non-seasons
      tidyr::expand_grid(
        forecaster = "scaled_pop_seasonal",
        trainer = "quantreg",
        lags = list(
          c(0, 7)
        ),
        seasonal_method = list("window", c("window", "flu"), c("window", "climatological")),
        pop_scaling = FALSE,
        train_residual = c(FALSE, TRUE),
        filter_source = c("", "nhsn"),
        filter_agg_level = "state",
        drop_non_seasons = FALSE,
        n_training = Inf,
        seasonal_backward_window = 5,
        keys_to_ignore = g_very_latent_locations
      ),
      tidyr::expand_grid(
        forecaster = "scaled_pop_seasonal",
        trainer = "quantreg",
        lags = list(
          c(0, 7, 14, 21)
        ),
        seasonal_method = list("window", c("window", "flu"), c("window", "climatological")),
        pop_scaling = FALSE,
        train_residual = c(FALSE, TRUE),
        filter_source = c("", "nhsn"),
        filter_agg_level = "state",
        drop_non_seasons = FALSE,
        n_training = Inf,
        seasonal_backward_window = 8,
        keys_to_ignore = g_very_latent_locations
      )
      # trying various window sizes
    ),
    scaled_pop_season_exogenous = bind_rows(
      expand_grid(
        forecaster = "scaled_pop_seasonal",
        trainer = "quantreg",
        # since it's a list, this gets expanded out to a single one in each row
        extra_sources = list2("nssp", "nwss", "nwss_region"), # removing google_symptoms for lack of data for now
        lags = list2(
          list2(
            c(0, 7), # hhs
            c(0, 7) # exogenous feature
          )
        ),
        seasonal_method = list("window"),
        pop_scaling = FALSE,
        filter_source = "",
        filter_agg_level = "state",
        n_training = Inf,
        drop_non_seasons = FALSE,
        keys_to_ignore = g_very_latent_locations
      )
    ),
    season_window_sizes = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list(
        c(0, 7)
      ),
      seasonal_method = list("window"),
      pop_scaling = FALSE,
      train_residual = FALSE,
      filter_source = "",
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      seasonal_backward_window = c(3, 5, 7, 9, 52),
      seasonal_forward_window = c(3, 5, 7),
      keys_to_ignore = g_very_latent_locations
    ),
    climate_linear = bind_rows(
      expand_grid(
        forecaster = "climate_linear_ensembled",
        scale_method = "quantile",
        center_method = "median",
        nonlin_method = c("quart_root", "none"),
        model_used = c("climate_linear", "climate", "climatological_forecaster"),
        filter_source = c("", "nhsn"),
        filter_agg_level = "state",
        drop_non_seasons = c(TRUE, FALSE),
        aheads = list(g_aheads),
        residual_tail = 0.67,
        residual_center = 0.097
      ),
      expand_grid(
        forecaster = "climate_linear_ensembled",
        scale_method = "none",
        center_method = "none",
        nonlin_method = c("quart_root", "none"),
        model_used = c("climate_linear", "climate", "climatological_forecaster"),
        filter_source = c("", "nhsn"),
        filter_agg_level = "state",
        drop_non_seasons = c(TRUE, FALSE),
        aheads = list(g_aheads),
        residual_tail = 0.99,
        residual_center = 0.35
      ),
    )
  ) %>%
    map(function(x) {
      if (g_dummy_mode) {
        x$forecaster <- "dummy_forecaster"
      }
      x <- add_id(x)
      if ("trainer" %in% names(x) && is.list(x$trainer)) {
        x$trainer <- x$trainer[[1]]
      }
      if ("seasonal_method" %in% names(x) && is.list(x$seasonal_method)) {
        x$seasonal_method <- x$seasonal_method[[1]]
      }
      # Add the outcome to each forecaster.
      x$outcome <- "hhs"
      x
    })

  # Make sure all ids are unique.
  stopifnot(
    length(out$id %>% unique()) == length(out$id)
  )
  out
}
