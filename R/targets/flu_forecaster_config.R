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
    scaled_pop_main = tidyr::expand_grid(
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
    ),
    scaled_pop_data_augmented = tidyr::expand_grid(
      forecaster = "scaled_pop",
      trainer = "quantreg",
      lags = list2(
        c(0, 7),
        c(0, 7, 14, 21),
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
        extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region", "va_flu_per_100k"),
        lags = list2(
          list2(
            c(0, 7), # hhs
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
          c("nssp", "va_flu_per_100k"),
          c("google_symptoms", "nwss"),
          c("google_symptoms", "nwss_region"),
          c("google_symptoms", "va_flu_per_100k"),
          c("nwss", "nwss_region"),
          c("nwss", "va_flu_per_100k"),
        ),
        lags = list2(
          list2(
            c(0, 7), # hhs
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
          c("nssp", "google_symptoms", "nwss", "nwss_region", "va_flu_per_100k"),
        ),
        lags = list2(
          list2(
            c(0, 7), # hhs
            c(0, 7), # nssp
            c(0, 7), # google symptoms
            c(0, 7), # nwss
            c(0, 7), # nwss_region
            c(0, 7), # va_flu_per_100k
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
    # This is broken (scale issue?)
    # no_recent_but_exogenous = bind_rows(
    #   expand_grid(
    #     forecaster = "no_recent_outcome",
    #     trainer = "quantreg",
    #     # since it's a list, this gets expanded out to a single one in each row
    #     extra_sources = list2("nssp", "google_symptoms", "nwss", "nwss_region", "va_flu_per_100k"),
    #     lags = list2(
    #       list2(
    #         # no hhs
    #         c(0, 7) # exogenous feature
    #       )
    #     ),
    #     scale_method = "quantile",
    #     nonlin_method = "quart_root",
    #     filter_source = "",
    #     use_population = TRUE,
    #     use_density = FALSE,
    #     week_method = "sine",
    #     n_training = Inf,
    #     keys_to_ignore = g_very_latent_locations
    #   ),
    #   expand_grid(
    #     forecaster = "no_recent_outcome",
    #     trainer = "quantreg",
    #     extra_sources = list2(
    #       c("nssp", "google_symptoms"),
    #       c("nssp", "nwss"),
    #       c("nssp", "nwss_region"),
    #       c("google_symptoms", "nwss"),
    #       c("google_symptoms", "nwss_region"),
    #       c("nwss", "nwss_region")
    #     ),
    #     lags = list2(
    #       list2(
    #         # no hhs
    #         c(0, 7), # first feature
    #         c(0, 7) # second feature
    #       )
    #     ),
    #     scale_method = "quantile",
    #     nonlin_method = "quart_root",
    #     filter_source = "",
    #     use_population = TRUE,
    #     use_density = FALSE,
    #     week_method = "sine",
    #     n_training = Inf,
    #     keys_to_ignore = g_very_latent_locations
    #   ),
    #   expand_grid(
    #     forecaster = "no_recent_outcome",
    #     trainer = "quantreg",
    #     extra_sources = list2(
    #       c("nssp", "google_symptoms", "nwss", "nwss_region", "va_flu_per_100k"),
    #     ),
    #     lags = list2(
    #       list2(
    #         # no hhs
    #         c(0, 7), # nssp
    #         c(0, 7), # google symptoms
    #         c(0, 7), # nwss
    #         c(0, 7), # nwss_region
    #         c(0, 7), # va_flu_per_100k
    #       )
    #     ),
    #     scale_method = "quantile",
    #     nonlin_method = "quart_root",
    #     filter_source = "",
    #     use_population = TRUE,
    #     use_density = FALSE,
    #     week_method = "sine",
    #     n_training = Inf,
    #     keys_to_ignore = g_very_latent_locations
    #   )
    # ),
    scaled_pop_season = bind_rows(
      tidyr::expand_grid(
        forecaster = "scaled_pop_seasonal",
        trainer = "quantreg",
        lags = list2(c(0, 7)),
        seasonal_method = list2(
          list2("window"),
          list2("window", "flu"),
          list2("window", "climatological")
        ),
        pop_scaling = FALSE,
        train_residual = c(FALSE, TRUE),
        filter_source = c("", "nhsn"),
        filter_agg_level = "state",
        n_training = Inf,
        keys_to_ignore = g_very_latent_locations
      )
    ),
    scaled_pop_season_exogenous = bind_rows(
      expand_grid(
        forecaster = "scaled_pop_seasonal",
        trainer = "quantreg",
        # since it's a list, this gets expanded out to a single one in each row
        extra_sources = list2("nssp", "nwss", "nwss_region", "va_flu_per_100k"), # removing google_symptoms for lack of data for now
        lags = list2(
          list2(
            c(0, 7), # hhs
            c(0, 7) # exogenous feature
          )
        ),
        seasonal_method = list2("window"),
        pop_scaling = FALSE,
        filter_source = c("", "nhsn"),
        filter_agg_level = "state",
        n_training = Inf,
        drop_non_seasons = FALSE,
        keys_to_ignore = g_very_latent_locations
      )
    ),
    season_window_sizes = tidyr::expand_grid(
      forecaster = "scaled_pop_seasonal",
      trainer = "quantreg",
      lags = list2(c(0, 7)),
      seasonal_method = list2("window"),
      pop_scaling = FALSE,
      train_residual = FALSE,
      filter_source = c("", "nhsn"),
      filter_agg_level = "state",
      drop_non_seasons = FALSE,
      n_training = Inf,
      seasonal_backward_window = c(3 * 7, 5 * 7, 7 * 7),
      seasonal_forward_window = c(7, 3 * 7, 5 * 7),
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
        quantiles_by_geo = c(TRUE, FALSE),
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
        filter_source = "nhsn",
        filter_agg_level = "state",
        drop_non_seasons = c(TRUE, FALSE),
        quantiles_by_geo = c(TRUE, FALSE),
        aheads = list(g_aheads),
        residual_tail = 0.99,
        residual_center = 0.35
      ),
      # only linear, a bunch of the parameters don't matter for it
      expand_grid(
        forecaster = "climate_linear_ensembled",
        scale_method = "none",
        center_method = "none",
        nonlin_method = "none",
        model_used = "linear",
        filter_source = "nhsn",
        filter_agg_level = "state",
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
