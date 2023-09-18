generate_forecast <- function() {
  #
  # loading old results and file system variables
  #
  lag_string <- paste(unlist(c("lags", lags)), collapse = "_")
  extras_string <- paste(extra_sources, collapse = "_")
  training_string <- glue::glue("window={n_training}")
  if (extras_string != "") {
    save_dir <- file.path(here::here(), results_directory, model_name, extras_string, training_string, lag_string)
  } else {
    save_dir <- file.path(here::here(), results_directory, model_name, training_string, lag_string)
  }
  save_file <- file.path(save_dir, glue::glue("ahead_{ahead}.gz.parquet"))
  if ((is.null(refit) || !refit) && file.exists(save_file)) {
    print(glue::glue("ahead={ahead}, model={model_name}, {lag_string}, n_training = {n_training}, extra sources={extras_string} was already fit! returning the cached model"))
    return(read_parquet(save_file))
  }
  #
  # it hasn't already been fit and/or we're refitting
  #
  start_date <- min(archive$DT$time_value) + n_training + n_training_pad
  end_date <- max(archive$DT$time_value) - ahead
  valid_predict_dates <- seq.Date(from = start_date, to = end_date, by = 1)
  print(glue::glue("evaluating name = {name}, ahead={ahead}, model={model_name}, {lag_string}, n_training = {n_training}, extra sources={extras_string}"))
  col_name <- glue::glue("{model_name}")
  duration <- system.time(res <- epix_slide(
    archive,
    ~ testing_forecast(ahead, .x, model, lags, extra_sources, quantiles, "hhs", n_training),
    before = n_training + n_training_pad - 1,
    ref_time_values = valid_predict_dates,
    new_col_name = col_name,
  ))
  print(glue::glue("for ahead {ahead} duration was"))
  print(duration)
}
