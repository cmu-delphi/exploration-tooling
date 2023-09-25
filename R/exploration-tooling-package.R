#' @keywords internal
"_PACKAGE"
library(epidatr)
.onLoad <- function(libname, pkgname) {
  epidatr::set_cache(cache_dir = ".exploration_cache", days = 14, max_size = 4)
  set_registry()
}
