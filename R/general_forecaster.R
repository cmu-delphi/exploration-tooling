new_forecaster <- function(pre_preprocessing, preprocessing, postprocessing, post_postprocessing, params) {
  structure(pre_preprocessing, preprocessing, postprocessing, post_postprocessing, params,)
}
