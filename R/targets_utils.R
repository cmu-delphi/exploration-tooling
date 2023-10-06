#' convert a list of forecasters
#' @description
#' the required format for targets is a little jank; this takes a human legible tibble and makes it targets legible.
#' Currently only `forecaster` and `trainer` can be symbols.
#' @param param_grid the tibble of parameters. Must have forecaster and trainer, everything else is optional
#' @export
#' @importFrom rlang syms
#' @importFrom purrr map
make_target_param_grid <- function(param_grid) {
  param_grid %<>% mutate(forecaster = syms(forecaster))
  param_grid %<>% mutate(trainer = syms(trainer))
  list_of_params <- lists_of_real_values(param_grid)
  list_names <- map(list_of_params, names)
  tibble(
    forecaster = rlang::syms(param_grid$forecaster),
    id = param_grid$id,
    params = list_of_params,
    param_names = list_names
  )
}

# helper function for `make_target_param_grid`
lists_of_real_values <- function(param_grid) {
  full_lists <- transpose(param_grid %>% select(-forecaster, -id))
  filter_nonvalues <- function(x) {
    Filter(Negate(function(a) is.null(a) && is.na(a)), x)
  }
  purrr::map(full_lists, filter_nonvalues)
}
