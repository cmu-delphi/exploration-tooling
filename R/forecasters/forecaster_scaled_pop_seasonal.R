#' Scaled pop seasonal
#'
#' This is identical to the `scaled_pop` forecaster, but with seasonal features
#' added in. The seasonal features are pre-cached from a PCA analysis of past
#' data.
#'
#' @param epi_data the actual data used
#' @param outcome the name of the target variable
#' @param extra_sources the name of any extra columns to use. This list could be
#'   empty
#' @param ahead (this is relative to the `as_of` field of the `epi_df`, which is
#'   likely *not* the same as the `ahead` used by epipredict, which is relative
#'   to the max time value of the `epi_df`. how to handle this is a modelling
#'   question left up to each forecaster; see latency_adjusting.R for the
#'   existing examples)
#' @param pop_scaling an example extra parameter unique to this forecaster
#' @param trainer an example extra parameter that is fairly common
#' @param filter_source if multiple sources are mixed together, if this is non-null it filters to just ones that match this value
#' @param filter_agg_level if multiple geographic levels are mixed together, if this is non-null it filters to just ones that match this value (e.g. you probably want "state")
#' @param ... it can also have any number of other parameters. In this case, the
#'   `...` args are all inputs to [`epipredict::default_args_list`].  Consult the
#'   repository for existing parameter names so that your function will follow a
#'   similar schema (e.g. `trainer`, while not strictly required, is a common
#'   parameter, as are any of the `default_args_list()` parameters) these parameters
#'   should be ones that will store well in a data.table; if you need more
#'   complicated parameters, it is better to store them in separate files, and
#'   use the filename as the parameter.
#' @param quantile_levels The quantile levels to predict. Defaults to those required by
#'   covidhub.
#' @seealso some utilities for making forecasters: [format_storage],
#'   [sanitize_args_predictors_trainer]
#'
#' @importFrom epipredict epi_recipe step_population_scaling frosting default_args_list layer_population_scaling
#' @importFrom tibble tibble
#' @importFrom zeallot %<-%
#' @importFrom recipes all_numeric
#' @export
scaled_pop_seasonal <- function(epi_data,
                                outcome,
                                extra_sources = "",
                                ahead = 1,
                                pop_scaling = TRUE,
                                drop_non_seasons = FALSE,
                                scale_method = c("none", "quantile", "std"),
                                center_method = c("median", "mean"),
                                nonlin_method = c("quart_root", "none"),
                                seasonal_pca = c("none", "flu", "covid", "indicator"),
                                trainer = parsnip::linear_reg(),
                                quantile_levels = covidhub_probs(),
                                filter_source = "",
                                filter_agg_level = "",
                                ...) {
  scale_method <- arg_match(scale_method)
  center_method <- arg_match(center_method)
  nonlin_method <- arg_match(nonlin_method)
  seasonal_pca <- arg_match(seasonal_pca)
  # perform any preprocessing not supported by epipredict
  #
  # this is for the case where there are multiple sources in the same column
  epi_data %<>% filter_extraneous(filter_source, filter_agg_level)
  # this next part is basically unavoidable boilerplate you'll want to copy
  args_input <- list(...)
  # edge case where there is no data or less data than the lags; eventually epipredict will handle this
  if (!confirm_sufficient_data(epi_data, ahead, args_input, outcome, extra_sources)) {
    null_result <- tibble(
      geo_value = character(),
      forecast_date = lubridate::Date(),
      target_end_date = lubridate::Date(),
      quantile = numeric(),
      value = numeric()
    )
    return(null_result)
  }
  # this is to deal with grouping by source in tests that don't include it
  adding_source <- FALSE
  if (!("source" %in% names(epi_data))) {
    adding_source <- TRUE
    epi_data$source <- c("none")
    attributes(epi_data)$metadata$other_keys <- "source"
  }
  args_input[["ahead"]] <- ahead
  args_input[["quantile_levels"]] <- quantile_levels
  args_list <- inject(default_args_list(!!!args_input))
  # if you want to hardcode particular predictors in a particular forecaster
  predictors <- c(outcome, extra_sources)
  c(args_list, predictors, trainer) %<-% sanitize_args_predictors_trainer(epi_data, outcome, predictors, trainer, args_list)

  if (!("season_week" %in% names(epi_data))) {
    epi_data %<>%
      mutate(
        epiweek = epiweek(time_value),
        epiyear = epiyear(time_value)
      ) %>%
      left_join(
        (.) %>%
          distinct(epiweek, epiyear) %>%
          mutate(
            season = convert_epiweek_to_season(epiyear, epiweek),
            season_week = convert_epiweek_to_season_week(epiyear, epiweek)
          ),
        by = c("epiweek", "epiyear")
      )
  }

  # end of the copypasta
  # finally, any other pre-processing (e.g. smoothing) that isn't performed by
  # epipredict
  if (drop_non_seasons) {
    season_data <- epi_data %>% drop_non_seasons()
  } else {
    season_data <- epi_data
  }

  # whiten to get the sources on the same scale
  learned_params <- calculate_whitening_params(season_data, predictors, scale_method, center_method, nonlin_method)
  epi_data %<>% data_whitening(predictors, learned_params, nonlin_method)

  # get the seasonal features
  if (seasonal_pca %in% c("flu", "covid")) {
    if (seasonal_pca == "flu") {
      if (!file.exists("aux_data/pca_data/flu_seasonal_pcs")) {
        # Read the flusion data
        stopifnot(file.exists("aux_data/flusion_data/flusion_merged"))
        flusion_data_archive <-
          qs::qread(here::here("aux_data/flusion_data/flusion_merged")) %>%
          filter(
            !geo_value %in% c("as", "pr", "vi", "gu", "mp"),
            !is.na(value),
            time_value <= "2024-04-24"
          ) %>%
          rename(hhs = value) %>%
          relocate(source, geo_value, time_value, version, hhs, agg_level, season, season_week, year, population, density) %>%
          as_epi_archive(other_keys = "source", compactify = TRUE)

        # Whiten the data and get the PCA
        learned_params <- flusion_data_archive %>%
          epix_as_of(as.Date("2023-09-01")) %>%
          drop_non_seasons() %>%
          calculate_whitening_params("hhs")
        pca <- flusion_data_archive %>%
          epix_as_of(as.Date("2023-09-01")) %>%
          data_whitening("hhs", learned_params) %>%
          select(geo_value, season, source, season_week, hhs) %>%
          filter(!is.na(season_week), !is.na(hhs)) %>%
          pivot_wider(names_from = c(geo_value, season, source), values_from = hhs, values_fn = mean) |>
          fill(-season_week, .direction = "downup") %>%
          select(-season_week) %>%
          as.matrix() %>%
          prcomp()
        # Using the top 3 PCs, since they all look reasonable
        seasonal_features <- as_tibble(predict(pca)[, 1:3])
        seasonal_features$season_week <- 1:nrow(seasonal_features)
        qs::qsave(seasonal_features, "aux_data/seasonal_features/flu")
      } else {
        seasonal_features <- qs::qread("aux_data/seasonal_features/flu")
      }
      args_list$lags <- c(args_list$lags, 0, 0, 0)
    }
    if (seasonal_pca == "covid") {
      if (!file.exists("aux_data/pca_data/covid_pcs")) {
        seasonal_data <- pub_covidcast(
          "hhs", "confirmed_admissions_covid_1d_prop_7dav",
          geo_type = "state",
          geo_values = "*",
          time_type = "day",
          time_values = epirange(20210801, 20240501)
        ) %>%
          select(geo_value, time_value, hosp = value) %>%
          filter(geo_value %nin% c("as", "pr", "vi", "gu", "mp"))
        pca <- seasonal_data %>%
          mutate(
            epiweek = epiweek(time_value),
            epiyear = epiyear(time_value)
          ) %>%
          left_join(
            (.) %>%
              distinct(epiyear, epiweek) %>%
              mutate(
                season = convert_epiweek_to_season(epiyear, epiweek),
                season_week = convert_epiweek_to_season_week(epiyear, epiweek)
              ),
            by = c("epiyear", "epiweek")
          ) %>%
          group_by(geo_value, season, season_week) %>%
          summarise(hosp = sum(hosp), .groups = "drop") %>%
          mutate(hr = case_when(hosp == 0 ~ NA, TRUE ~ hosp)) %>%
          select(geo_value, season, season_week, hosp) %>%
          mutate(hosp = hosp^(1 / 4)) |>
          pivot_wider(names_from = c(geo_value, season), values_from = hosp) |>
          fill(-season_week, .direction = "downup") %>%
          select(-season_week) %>%
          as.matrix() %>%
          prcomp()

        # Only using the first two, because the third looks like noise
        seasonal_features <- as_tibble(predict(pca)[, 1:2])
        seasonal_features$season_week <- 1:nrow(seasonal_features)
        qs::qsave(seasonal_features, "aux_data/seasonal_features/covid")
      } else {
        seasonal_features <- qs::qread("aux_data/seasonal_features/covid")
      }
      args_list$lags <- c(args_list$lags, 0, 0)
    }

    epi_data <- epi_data %>% left_join(seasonal_features, by = "season_week")
  }

  # TODO Really jank way of accounting for ahead.
  object <- list(
    shift_grid = tibble(PC1 = c(), PC2 = c(), PC3 = c())
  )
  epi_data %>% mutate(
    add_shifted_columns(!!pppp, ahead = ahead, role = "predictor")
  )

  # preprocessing supported by epipredict
  preproc <- epi_recipe(epi_data)
  if (pop_scaling) {
    preproc %<>% step_population_scaling(
      all_of(predictors),
      df = epidatasets::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  if (seasonal_pca == "indicator") {
    stopifnot("season_week" %in% names(epi_data))
    preproc %<>%
      # TODO Really jank way of accounting for ahead.
      step_mutate(before_peak = (season_week - ahead < 16), role = "predictor") %>%
      step_mutate(after_peak = (season_week - ahead > 20), role = "predictor")
  } else if (seasonal_pca != "none") {
    preproc %<>%
      add_role(PC1, PC2, PC3, role = "predictor")
  }
  preproc %<>% arx_preprocess(outcome, predictors, args_list)

  # postprocessing supported by epipredict
  postproc <- frosting()
  postproc %<>% arx_postprocess(trainer, args_list)
  if (pop_scaling) {
    postproc %<>% layer_population_scaling(
      .pred, .pred_distn,
      df = epidatasets::state_census,
      df_pop_col = "pop",
      create_new = FALSE,
      rate_rescaling = 1e5,
      by = c("geo_value" = "abbr")
    )
  }
  # browser()
  # with all the setup done, we execute and format
  pred <- run_workflow_and_format(preproc, postproc, trainer, season_data, epi_data)
  # now pred has the columns
  # (geo_value, forecast_date, target_end_date, quantile, value)
  # finally, any postprocessing not supported by epipredict e.g. calibration
  # reintroduce color into the value
  pred_final <- pred %>%
    rename({{ outcome }} := value) %>%
    data_coloring(outcome, learned_params, join_cols = key_colnames(epi_data, exclude = "time_value"), nonlin_method = nonlin_method) %>%
    rename(value = {{ outcome }}) %>%
    mutate(value = pmax(0, value))
  if (adding_source) {
    pred_final %<>% select(-source)
  }
  gc()

  return(pred_final)
}
