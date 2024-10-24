#' Look up forecasters by name
#'
#' Given a (partial) forecaster name, look up all forecasters in the given
#' project which contain part of that name.
#'
#' @param forecaster_grid the forecaster grid to search.
#' @param pattern string to search in the forecaster name.
#'
#' @export
forecaster_lookup <- function(pattern, forecaster_grid = NULL, printing = TRUE) {
  if (is.null(forecaster_grid)) {
    forecaster_grid <- tar_read_raw("forecaster_parameter_combinations") %>%
      map(make_forecaster_grid) %>%
      bind_rows()
  }
  fc_row <- forecaster_grid %>% filter(grepl(pattern, id))
  if (printing) {
    params <- fc_row$params[[1]]
    if (!is.null(params$trainer)) {
      params$trainer <- as_string(params$trainer)
    }
    print(glue::glue("name: {fc_row %>% pull(id)}"))
    print(glue::glue("forecaster: {fc_row$forecaster[[1]]}"))
    print(glue::glue("params:"))
    print(params %>% data.table::as.data.table())
  }
  return(fc_row)
}

#' Add a unique id based on the column contents
#'
#' Create a string of `n_adj` that is a hash of the parameters.
#'
#' @param tib the tibble to add a column to. everything should be convertable to a string
#' @param exclude a vector of column names to exclude from the hash
#'
#' @export
add_id <- function(tib, exclude = c()) {
  ids <- tib %>%
    select(-all_of(exclude)) %>%
    purrr::transpose() %>%
    map_chr(get_single_id)
  tib %>%
    mutate(id = ids) %>%
    relocate(id, .before = everything())
}

#' Generate a two-word id from a simple list of parameters.
#'
#' @param param_list a list of parameters.
#'
#' @export
get_single_id <- function(param_list) {
  param_list[sort(names(param_list))] %>%
    paste(sep = "", collapse = "") %>%
    gsub("[[:blank:]]", "", .) %>%
    cli::hash_animal(n_adj = 1) %>%
    purrr::pluck("words", 1) %>%
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
make_forecaster_grid <- function(tib) {
  if ("trainer" %in% colnames(tib)) {
    tib$trainer <- rlang::syms(tib$trainer)
  }
  # turns a tibble into a list of named lists
  params_list <-
    tib %>%
    select(-forecaster, -id) %>%
    split(seq_len(nrow(.))) %>%
    unname() %>%
    lapply(as.list)
  # for whatever reason, trainer ends up being a list of lists, which we do not want
  params_list %<>% lapply(function(x) {
    x$trainer <- x$trainer[[1]]
    x$lags <- x$lags[[1]]
    x
  })

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

  return(out)
}

#' Make an ensemble grid.
#'
#' Same as `make_forecaster_grid`, but for ensembles.
#'
#' @param tib the tibble of parameters.
#'
#' @export
make_ensemble_grid <- function(tib) {
  sym_subset <- function(param_list) {
    imap(param_list, \(x, y) if (y %in% list("average_type")) rlang::sym(x) else x)
  }

  tibble(
    id = tib$id,
    children_ids = tib$children_ids %>%
      map(function(x) paste0("forecast_", x)) %>%
      map(rlang::syms),
    ensemble = rlang::syms(tib$ensemble),
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
