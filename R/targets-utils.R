#' Look up forecasters by name
#'
#' Given a (partial) forecaster name, look up all forecasters in the given
#' project which contain part of that name.
#'
#' @param forecaster_name a part of the adj.adj.1 name used to identify the forecaster.
#'
#' @importFrom targets tar_read tar_config_get
#' @export
forecaster_lookup <- function(forecaster_name, param_grid) {
  param_grid %>% filter(grepl(strip_underscored(forecaster_name), id))
}

strip_underscored <- function(x) {
  g <- gregexpr("_", x, fixed = TRUE)
  last_underscore <- g[[1]][[length(g[[1]])]]
  substr(x[[1]], start = last_underscore + 1, stop = nchar(x))
}

#' Add a unique id based on the column contents
#'
#' Create a string of `n_adj` that is a hash of the parameters.
#'
#' @param tib the tibble to add a column to. everything should be convertable to a string
#' @param exclude a vector of column names to exclude from the hash
#'
#' @importFrom cli hash_animal
#' @export
add_id <- function(tib, exclude = c()) {
  tib %>%
    select(all_of(order(colnames(.))) & !all_of(exclude)) %>%
    rowwise() %>%
    mutate(id = get_single_id(across(everything()))) %>%
    ungroup() %>%
    relocate(id, .before = everything())
}

#' Generate a two-word id from a simple list of parameters.
#'
#' Assumes the list keys are sorted.
#'
#' @param param_list the list of parameters. must include `ahead` if `ahead = NULL`
#'
#' @export
get_single_id <- function(param_list) {
  param_list %>%
    paste(sep = "", collapse = "") %>%
    hash_animal(n_adj = 1) %>%
    pluck("words", 1) %>%
    paste(sep = ".", collapse = ".")
}

#' Make a forecaster grid.
#'
#' Convert a tibble of forecasters and their parameters to a specific format
#' that we can iterate over in targets. Currently only `forecaster` and
#' `trainer` can be symbols.
#'
#' @param tib the tibble of parameters. Must have the forecaster and trainer
#' columns, everything else is optional.
#'
#' @export
#' @importFrom rlang syms
make_forecaster_grid <- function(tib) {
  if ("trainer" %in% colnames(tib)) {
    tib$trainer <- rlang::syms(tib$trainer)
  }

  params_list <- tib %>%
    select(-forecaster, -id) %>%
    transpose()

  if (length(params_list) == 0) {
    out <- tibble(
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = list(list()),
      param_names = list(list())
    )
  } else {
    out <- tibble(
      id = tib$id,
      forecaster = rlang::syms(tib$forecaster),
      params = params_list,
      param_names = map(params_list, names)
    )
  }

  out
}

#' Make an ensemble grid.
#'
#' Same as `make_forecaster_grid`, but for ensembles.
#'
#' @param tib the tibble of parameters. Must have the forecaster and trainer
#' columns, everything else is optional.
#'
#' @export
#' @importFrom rlang syms
make_ensemble_grid <- function(tib) {
  sym_subset <- function(param_list) {
    imap(param_list, \(x, y) if (y %in% list("average_type")) sym(x) else x)
  }

  tibble(
    id = tib$id,
    children_ids = tib$children_ids %>%
      map(function(x) paste0("forecast_", x)) %>%
      map(syms),
    ensemble = syms(tib$ensemble),
    ensemble_args = map(tib$ensemble_args, sym_subset),
    ensemble_args_names = map(tib$ensemble_args, ~ names(.x))
  )
}

#' Get exclusions from a JSON file for a given date
#'
#' @param date A date
#' @param exclusions_json A JSON file with exclusions in the format:
#'
#'    {"exclusions": {"2024-03-24": "ak,hi"}}
#'
#' @export
get_exclusions <- function(
    date,
    exclusions_json = here::here("scripts", "geo_exclusions.json")) {
  if (!file.exists(exclusions_json)) {
    return("")
  }

  s <- jsonlite::read_json(exclusions_json)$exclusions[[as.character(date)]]
  if (!is.null(s)) {
    return(strsplit(s, ",")[[1]])
  }
  return("")
}
