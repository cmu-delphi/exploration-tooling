epix_slide_simple <- function(epi_archive, forecaster, ref_time_values, before = Inf, cache_key = NULL) {
  # this is so that changing the object without changing the name doesn't result in pulling the wrong cache
  cache_hash <- rlang::hash(epi_archive)
  dir.create(".exploration_cache/slide_cache", showWarnings = FALSE, recursive = TRUE)
  out <- purrr::map(ref_time_values, function(tv) {
    if (is.null(cache_key)) {
      epi_df <- epi_archive %>%
        epix_as_of(tv, min_time_value = tv - before)
    } else {
      file_path <- glue::glue(".exploration_cache/slide_cache/{cache_key}_{cache_hash}_{before}_{tv}.parquet")
      if (file.exists(file_path)) {
        epi_df <- qs::qread(file_path)
      } else {
        epi_df <- epi_archive %>%
          epix_as_of(tv, min_time_value = tv - before)
        qs::qsave(epi_df, file_path)
      }
    }
    epi_df %>% forecaster()
  }) %>% bind_rows()
  gc()
  return(out)
}
