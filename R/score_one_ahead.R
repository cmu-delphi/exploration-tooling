calc_wis_one_ahead <- function(ahead, model, model_name, lags, training_window = 120, training_window_pad = 20, extra_sources = c("chng"), refit = FALSE) {
  #
  # reformatting the results
  #
  get_name <- function(extension) quo(paste0(col_name, extension)) # unfortunately, glue doesn't work with strings on the input side, only the output side, so we still need this rather jank method
  named_geo <- quo(paste0(col_name, "_geo_value"))
  named_target <- quo(paste0(col_name, "_target_date"))
  named_pred <- quo(paste0(col_name, "_target_date"))
  predictions <- res %>%
    rename(geo_value := (!!(get_name("_geo_value"))), target_date = (!!(get_name("_target_date")))) %>% # keys are shared across models
    select(-c(!!(get_name("_.pred")), !!(get_name("_forecast_date")))) %>% # drop unused fields
    group_by(time_value, geo_value, target_date) %>%
    rename(.pred_distn := (!!(get_name("_.pred_distn")))) %>% # temp rename b/c of substitution difficulties
    reframe(quantile = quantiles, value = quantile(.pred_distn, quantiles)[[1]]) %>% # actually list results by the given quantile levels
    left_join(ground_truth, by = join_by(geo_value, target_date == time_value)) %>%
    ungroup() %>%
    rename(actual_value = hhs) # add in the actual values for comparison

  #
  # actually evaluating the performance and saving the results
  #
  evaluated_WIS <- weighted_interval_score(predictions) %>% rename("{col_name}_WIS" := weighted_interval_score)
  # use a unique name
  predictions <- predictions %>% rename("{col_name}_value" := value)
  # make sure we can actually save it there
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  write_parquet(evaluated_WIS, save_file, compression = "gzip")
  if (upload) {
    # ignore the local part of the save directory
    remote_save_dir <- paste(reduce(
      strsplit(c(save_dir, here::here()), "/"),
      setdiff
    ), collapse = "/")
    remote_save_file <- paste(reduce(
      strsplit(c(save_file, here::here()), "/"),
      setdiff
    ), collapse = "/")

    if (!aws.s3::object_exists(remote_save_dir) || refit) {
      aws.s3::put_object(save_file, remote_save_file, s3bucket)
    }
  }
  return(list(evaluated_WIS, duration, predictions))
}
