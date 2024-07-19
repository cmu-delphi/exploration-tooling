#' Helper functions

make_newx <- function(df_features) {
  # From Alden; actually is doing "latest" as well.
  # I add a suppressWarnings() here beacuse we know that max(time_value[!is.na(.x)])
  # will have an empty argument for Google Symptoms and I want to separate these
  # out from unexpected warnings.
  suppressWarnings(newx_info <- df_features %>%
    group_by(geo_value) %>%
    summarise(across(tidyselect::matches("^value(\\+0|-)"), ~ max(time_value[!is.na(.x)])), .groups = "drop_last") %>%
    tidyr::pivot_longer(-geo_value, names_to = "feature_name", values_to = "time_value"))
  # Test features
  df_features_test <- left_join(newx_info,
    df_features %>%
      filter(time_value %in% unique(newx_info$time_value)) %>%
      tidyr::pivot_longer(-c(geo_value, time_value), names_to = "feature_name", values_to = "value"),
    by = c("feature_name", "geo_value", "time_value")
  ) %>%
    select(-time_value) %>%
    tidyr::pivot_wider(names_from = "feature_name", values_from = "value")
  test_geo_value <- df_features_test %>% pull(geo_value)
  newx <- df_features_test %>%
    select(-geo_value) %>%
    as.matrix()
  # Note: this will be deferred to post hoc analysis.
  # newx_max_latency = (lubridate::ymd(forecast_date)
  #                - min(lubridate::ymd(newx_info$time_value), na.rm=TRUE))
  return(list(
    newx = newx,
    test_geo_value = test_geo_value,
    newx_info = newx_info
  ))
}

make_newx_latest_data <- function(df_features) {
  df_feat <- tibble(expand.grid(
    geo_value = unique(df_features$geo_value),
    time_value = unique(df_features$time_value)
  ))
  df_feat$geo_value <- as.character(df_feat$geo_value)
  df_feat <- left_join(df_feat, df_features, by = c("geo_value", "time_value"))
  df_feat <- df_feat %>%
    arrange(time_value) %>%
    group_by(geo_value) %>%
    group_modify(~ zoo::na.locf(.x, na.rm = FALSE)) %>%
    ungroup()
  test_geo_value <- df_feat %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value) %>%
    pull()
  newx <- df_feat %>%
    filter(time_value == max(time_value)) %>%
    select(-c(geo_value, time_value)) %>%
    as.matrix()
  return(list(newx = newx, test_geo_value = test_geo_value))
}

resample_matx <- function(matx, resample, newx = FALSE) {
  resample_cols <- grep(resample$substring, colnames(matx))
  if (newx) {
    resampled_mat <- matx[, resample_cols]
  } else {
    resampled_mat <- matx[sample(1:nrow(matx), nrow(matx), replace = TRUE), resample_cols]
  }
  colnames(resampled_mat) <- paste0(colnames(matx)[resample_cols], "_resampled")
  if (resample$overwrite) {
    matx <- matx[, -resample_cols]
  }
  matx <- cbind(matx, resampled_mat)
  return(matx)
}

zero_impute_matx <- function(matx, zero_impute) {
  impute_cols <- grep(zero_impute, colnames(matx))
  impute_mat <- matx[, impute_cols]
  impute_mat[is.na(impute_mat)] <- 0
  matx <- matx[, -impute_cols]
  matx <- cbind(matx, impute_mat)
  return(matx)
}


#' Simple quantile autoregressive forecaster based on `quantgen`
#'
#' Performs predictions by setting newx to the latest data at which all
#' signals available; but also saves predictions where we use the latest data
#' per feature.
quantgen_forecaster <- function(
    df_list,
    forecast_date,
    incidence_period,
    ahead,
    geo_type,
    n = 4 * ifelse(incidence_period == "day", 7, 1),
    lags = 0,
    tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    transform = NULL,
    inv_trans = NULL,
    featurize = NULL,
    resample = NULL,
    zero_impute = NULL,
    verbose = FALSE,
    n_core = 1,
    debug = NULL,
    ...) {
  # Check that Gurobi is installed
  if (!"gurobi" %in% rownames(installed.packages())) {
    stop("Gurobi is not installed. Please install Gurobi and try again.")
  }

  if (n_core > 1) {
    n_core <- min(n_core, parallel::detectCores())
  } else {
    n_core <- 1
  }

  # Check lags vector or list
  if (any(unlist(lags) < 0)) stop("All lags must be nonnegative.")
  if (!is.list(lags)) {
    lags <- rep(list(lags), length(df_list))
  } else if (length(lags) != length(df_list)) {
    stop(paste(
      "If `lags` is a list, it must have length equal to the number",
      "of signals."
    ))
  }

  # Check transform function or list, and apply them if we need to
  if (!is.null(transform)) {
    if (!is.list(transform)) {
      transform <- rep(list(transform), length(df_list))
    } else if (length(transform) != length(df_list)) {
      stop(paste(
        "If `transform` is a list, it must have length equal to the",
        "number of signals."
      ))
    }
    if (is.null(inv_trans)) {
      stop("If `transform` is specified, then `inv_trans` must be as well.")
    }
    for (i in 1:length(df_list)) {
      df_list[[i]] <- df_list[[i]] %>% mutate(value = transform[[i]](value))
    }
  }

  # Define dt by flipping the sign of lags, include dt = +ahead as a response
  # shift, for each ahead value, for convenience later
  dt <- lapply(lags, "-")
  dt[[1]] <- c(dt[[1]], ahead)

  # Append shifts, and aggregate into wide format
  df_wide <- covidcast::aggregate_signals(df_list, dt = dt, format = "wide")

  # Separate out into feature data frame, featurize if we need to
  df_features <- df_wide %>%
    select(geo_value, time_value, tidyselect::matches("^value(\\+0|-)"))
  feature_end_date <- df_features %>%
    summarize(max(time_value)) %>%
    pull()
  if (!is.null(featurize)) df_features <- featurize(df_features)

  # Identify params for quantgen training and prediction functions
  params <- list(...)
  params$tau <- tau
  if (!"lambda" %in% names(params)) params$lambda <- 0

  # CV functionality removed.
  # Noncrossing functionality removed.
  train_fun <- quantgen::quantile_lasso
  predict_fun <- quantgen:::predict.quantile_genlasso

  train_names <- names(as.list(args(train_fun)))
  predict_names <- names(as.list(args(predict_fun)))
  train_params <- params[names(params) %in% train_names]
  predict_params <- params[names(params) %in% predict_names]

  # This controls how many threads Gurobi uses. By default (0) Gurobi uses all
  # virtual cores (up to 32), but we want to limit this, so we can parallelize across
  # aheads.
  train_params$params$Threads <- (n_core > 1) * max(floor(n_core / length(ahead)), 1)

  # Form both, "latest" will be saved to disk
  newx_list <- make_newx(df_features)

  if (!is.null(resample)) {
    newx_list$newx <- resample_matx(newx_list$newx, resample, newx = TRUE)
  }
  if (!is.null(zero_impute)) {
    newx_list$newx <- zero_impute_matx(newx_list$newx, zero_impute)
  }

  if (verbose) message(sprintf("Quantgen forecaster running with %d cores", n_core))
  # Loop over ahead values, fit model, make predictions
  if (n_core > 1) {
    lapply_fun <- function(X, FUN) {
      bettermc::mclapply(X, FUN, mc.cores = min(n_core, length(ahead)), mc.allow.recursive = FALSE)
    }
  } else {
    lapply_fun <- lapply
  }

  results_list <- lapply_fun(1:length(ahead), function(i) {
    print(paste(i, "out of", length(ahead), "aheads"))
    a <- ahead[i]
    if (verbose) cat(sprintf("%s%i", ifelse(i == 1, "\nahead = ", ", "), a))
    # Training end date
    response_end_date <- df_wide %>%
      select(time_value, tidyselect::starts_with(sprintf("value+%i:", a))) %>%
      tidyr::drop_na() %>%
      summarize(max(time_value)) %>%
      pull()
    train_end_date <- min(feature_end_date, response_end_date)

    # Training x and y
    x <- df_features %>%
      filter(between(time_value, train_end_date - n + 1, train_end_date)) %>%
      select(-c(geo_value, time_value)) %>%
      as.matrix()
    y <- df_wide %>%
      filter(between(time_value, train_end_date - n + 1, train_end_date)) %>%
      select(tidyselect::starts_with(sprintf("value+%i:", a))) %>%
      pull()
    if (!is.null(resample)) {
      x <- resample_matx(x, resample)
    }
    if (!is.null(zero_impute)) {
      x <- zero_impute_matx(x, zero_impute)
    }

    # Add training x and y to training params list, fit model
    train_params$x <- x
    train_params$y <- y
    train_obj <- do.call(train_fun, train_params)

    # Add training object and newx to test params list, make predictions
    predict_params$object <- train_obj
    predict_params$object$inv_trans <- inv_trans # Let quantgen handle this
    predict_params$newx <- newx_list$newx
    predict_mat <- do.call(predict_fun, predict_params)

    # Do some wrangling to get it into evalcast "long" format
    colnames(predict_mat) <- tau
    predict_df <- bind_cols(geo_value = newx_list$test_geo_value, predict_mat) %>%
      pivot_longer(cols = -geo_value, names_to = "quantile", values_to = "value") %>%
      mutate(quantile = as.numeric(quantile), value = as.numeric(value), ahead = a)
    return(list(predict_df, predict_params))
  })

  if (verbose) cat("\n")

  result <- lapply(results_list, function(x) x[[1]])

  if (!is.null(debug)) {
    saveRDS(
      list(
        ahead = ahead,
        forecast_date = forecast_date,
        predict_params = lapply(results_list, function(x) x[[2]]),
        nexw_list = newx_list,
        predictions = bind_rows(result)
      ),
      sprintf("%s_%s_%s.RDS", debug, geo_type, forecast_date)
    )
  }

  # Collapse predictions into one big data frame, and return
  return(do.call(rbind, result))
}

quantreg_forecaster <- function() {
  stop("quantreg_forecaster not implemented")
}


#### BEGIN copied content from cmu-delphi/hospitalization-forecaster
#### baseline-forecaster/forecaster-utils.R as of 2022-10-17, slightly
#### reformatted

# Make a forecaster that indexes aheads relative to the forecast_date from one
# that indexes them relative to the time_value of the latest response value
# available. Assumes that the first df in the df_list is the response. Assumes
# indexing is relative to the latest response and does not change based on
# covariate availability, and that there are no NA entries that might be
# omitted before such indexing. Only precisely works if this indexing is
# performed across all geos simultaneously, or if the latest response time
# value is the same across all geos. Requires `ahead` to be specified by name.
make_forecaster_account_for_response_latency <- function(forecaster) {
  assert_that(is_function(forecaster) && any(c("ahead", "...") %in% fn_fmls_names(forecaster)))

  return(
    function(df_list, forecast_date, ..., incidence_period, ahead) {
      assert_that(identical(incidence_period, "day"))

      max_response_time_value <- max(df_list[[1L]][["time_value"]])
      response_latency <- as.integer(forecast_date - max_response_time_value)
      ahead_of_latest_response <- ahead + response_latency
      forecast_relative_to_latest_response <- forecaster(
        df_list = df_list,
        forecast_date = forecast_date,
        ...,
        ahead = ahead_of_latest_response
      )
      assert_that(
        !"target_end_date" %in% names(forecast_relative_to_latest_response)
      )
      forecast <- forecast_relative_to_latest_response %>%
        mutate(ahead = .data$ahead - .env$response_latency)
      return(forecast)
    }
  )
}

match_scalar_fn_arg <- function(arg) {
  fmls <- fn_fmls(caller_fn())
  arg.as.character <- as.character(ensym(arg))
  assert_that(arg.as.character %in% names(fmls), msg = sprintf("arg name (%s) was not in formal arg names (%s)", arg.as.character, toString(names(fmls))))
  choices <- eval_bare(fmls[[arg.as.character]], caller_env())
  if (identical(arg, choices)) {
    ## (arg was probably missing in call to caller_fn() since it is identical to
    ## the full list of choices; assume that this indeed was the case and
    ## replace it with the first choice:)
    arg <- choices[[1L]]
  }
  assert_that(if (is.list(choices)) list(arg) %in% choices else arg %in% choices,
    msg = paste(collapse = "\n", capture.output({
      cat("arg was not in choices;\n")
      cat("arg:\n")
      print(arg)
      cat("choices:\n")
      print(choices)
    }))
  )
  return(arg)
}

make_caching_forecaster <- function(
    forecaster,
    forecaster_name,
    cache_parent_dirpath,
    replace_results_with_trivial_for_mem = FALSE,
    disable_saving = list(FALSE, "TRUE.because.internal.saving", TRUE)) {
  assert_that(is_function(forecaster))
  assert_that(is_scalar_character(forecaster_name))
  assert_that(is_bool(replace_results_with_trivial_for_mem))
  assert_that(is_scalar_character(cache_parent_dirpath))
  disable_saving <- match_scalar_fn_arg(disable_saving)
  cache_parent_dirpath <- here::here(cache_parent_dirpath)

  return(
    (function(df_list, forecast_date, ...) {
      if (!dir.exists(cache_parent_dirpath)) {
        dir.create(cache_parent_dirpath)
      }
      cache.dirpath <- file.path(cache_parent_dirpath, forecaster_name)
      cache.filepath <- file.path(cache.dirpath, paste0(forecast_date, ".RDS"))
      trivial.results <- tibble(ahead = 1L, geo_value = "-1", quantile = 0.5, value = NA_real_)
      if (file.exists(cache.filepath)) {
        if (replace_results_with_trivial_for_mem) {
          cat("Cache file exists, but skipping loading & returning trivial results\n")
          return(trivial.results)
        } else {
          cat("Loading forecast from cache.\n")
          return(readRDS(cache.filepath))
        }
      } else {
        cat(sprintf(
          "No cache file found; generating result and %s at %s.\n",
          switch(disable_saving,
            "FALSE" = "storing",
            "TRUE.because.internal.saving" = "not storing at this level (saving is internal)",
            "TRUE" = "NOT storing (saving disabled)"
          ),
          cache.filepath
        ))
        ## We save time and have the same effect by skipping generating the
        ## forecast if disable_saving is TRUE (and there is no internal saving)
        ## and we are replacing the results with the trivial results.
        if (!(identical(disable_saving, TRUE) && replace_results_with_trivial_for_mem)) {
          forecast <- forecaster(df_list, forecast_date, ...)
        }
        ## We save only in the disable_saving FALSE case; we don't save in either
        ## the TRUE or "TRUE.because.internal.saving" cases.
        if (identical(disable_saving, FALSE)) {
          if (!dir.exists(cache.dirpath)) {
            dir.create(cache.dirpath)
          }
          saveRDS(forecast, cache.filepath)
        }
        if (replace_results_with_trivial_for_mem) {
          return(trivial.results)
        } else {
          return(forecast)
        }
      }
    }) %>% `class<-`(c("caching_forecaster", "function"))
  )
}

cached_forecast_is_available <- function(forecaster, forecast_date) {
  UseMethod("cached_forecast_is_available", forecaster)
}
cached_forecast_is_available.default <- function(forecaster, forecast_date) {
  return(FALSE)
}
cached_forecast_is_available.caching_forecaster <- function(forecaster, forecast_date) {
  assert_that(is_scalar_atomic(forecast_date) && inherits(forecast_date, "Date"))
  ##
  caching_env <- environment(forecaster)
  cache.dirpath <- file.path(caching_env$cache_parent_dirpath, caching_env$forecaster_name)
  cache.filepath <- file.path(cache.dirpath, paste0(forecast_date, ".RDS"))
  return(file.exists(cache.filepath))
}

fetch_cached_forecast <- function(forecaster, forecast_date) {
  UseMethod("fetch_cached_forecast", forecaster)
}
fetch_cached_forecast.caching_forecaster <- function(forecaster, forecast_date) {
  assert_that(is_scalar_atomic(forecast_date) && inherits(forecast_date, "Date"))
  assert_that(cached_forecast_is_available(forecaster, forecast_date))
  ##
  ## TODO avoid code duplication and worries about race conditions
  caching_env <- environment(forecaster)
  cache.dirpath <- file.path(caching_env$cache_parent_dirpath, caching_env$forecaster_name)
  cache.filepath <- file.path(cache.dirpath, paste0(forecast_date, ".RDS"))
  trivial.results <- tibble(ahead = 1L, geo_value = "-1", quantile = 0.5, value = NA_real_)
  if (caching_env$replace_results_with_trivial_for_mem) {
    return(trivial.results)
  } else {
    return(readRDS(cache.filepath))
  }
}

#### END copied content

#### BEGIN another chunk from forecaster-utils.R

make_forecaster_with_prespecified_args <- function(forecaster, ...) {
  function(df_list, forecast_date) {
    forecaster(df_list, forecast_date, ...)
  }
}

#### END

make_named_forecaster <- function(forecaster, forecaster_name) {
  (function(df_list, forecast_date, ...) {
    forecaster(df_list, forecast_date, ...)
  }) %>% `class<-`("named_forecaster")
}

forecaster_name <- function(forecaster) {
  UseMethod("forecaster_name", forecaster)
}

forecaster_name.caching_forecaster <- function(forecaster) {
  environment(forecaster)[["forecaster_name"]]
}

forecaster_name.named_forecaster <- function(forecaster) {
  environment(forecaster)[["forecaster_name"]]
}
