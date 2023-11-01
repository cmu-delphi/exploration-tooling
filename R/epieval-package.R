#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr select rename inner_join join_by mutate relocate any_of
#'    group_by reframe summarize left_join across filter rowwise everything ungroup
#' @importFrom purrr transpose map map2_vec
#' @keywords internal
"_PACKAGE"
.onLoad <- function(libname, pkgname) {
  epidatr::set_cache(
    cache_dir = ".exploration_cache",
    days = 14,
    max_size = 4,
    confirm = FALSE
  )
}
