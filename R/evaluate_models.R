evaluate_models <- function(models = NULL, parallel = FALSE, ncore = 12) {
  if (parallel) {
    clusterExport(cl, "testing_forecast")
    clusterExport(cl, "weighted_interval_score")
    clusterExport(cl, "calc_wis_one_ahead")
  }
  if (parallel) {
    clusterExport(cl, "net_models")
    # actually fit
    overall_time <- system.time(overall_result <- parallel::parApply(cl, to_be_fit, 1, \(x) calc_wis_one_ahead(x$ahead, net_models[[x$model]], x$model, x$lags, x$n_training, x$n_training_pad, x$extra_sources, x$refit)))
  } else {
    overall_time <- system.time(overall_result <- apply(to_be_fit, MARGIN = 1, FUN = \(x) calc_wis_one_ahead(x$ahead, net_models[[x$model]], x$model, x$lags, x$n_training, x$n_training_pad, x$extra_sources, x$refit)))
  }
}
