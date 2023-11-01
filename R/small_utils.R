#' the quantile levels used by the covidhub repository
#' @param type either standard or inc_case, with inc_case being a small subset of the standard
#' @export
covidhub_probs <- function(type = c("standard", "inc_case")) {
  type <- match.arg(type)
  switch(type,
    standard = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
    inc_case = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  )
}


#' add a unique id based on the column contents
#' @description
#' create a string of `n_adj` that is a hash of the parameters
#' and append the `ahead` at the end.
#' @param df the df to add a column to. everything should be convertable to a string
#' @param n_adj the number of adjectives to use; default of 2.
#' @importFrom cli hash_animal
#' @export
add_id <- function(df, n_adj = 2) {
  no_ahead <- df %>%
    select(-ahead)
  stringified <- no_ahead %>%
    select(order(colnames(no_ahead))) %>%
    rowwise() %>%
    mutate(id = paste(across(everything()), sep = "", collapse = ""), .keep = "none") %>%
    mutate(id = hash_animal(id, n_adj = n_adj)$words) %>%
    mutate(id = paste(id[1:n_adj], sep = "", collapse = "."))
  df %<>%
    ungroup() %>%
    mutate(parent_id = stringified$id) %>%
    rowwise() %>%
    mutate(id = paste(parent_id, ahead, sep = ".", collapse = " ")) %>%
    ungroup()
  return(df)
}


#' generate an id from a simple list of parameters
#' @param param_list the list of parameters. must include `ahead` if `ahead = NULL`
#' @param ahead the ahead to use.
#' @inheritParams add_id
#' @export
single_id <- function(param_list, ahead = NULL, n_adj = 2) {
  full_hash <- param_list[names(param_list) != "ahead"] %>%
    .[order(names(.))] %>% # put in alphabetical order
    paste(collapse = "") %>%
    hash_animal(n_adj = n_adj)
  single_string <- full_hash$words[[1]][1:n_adj] %>% paste(sep = ".", collapse = ".")
  if (is.null(ahead)) {
    full_name <- paste(single_string, param_list$ahead, sep = ".")
  } else {
    full_name <- paste(single_string, ahead, sep = ".")
  }
  return(full_name)
}


#' given target name(s), lookup the corresponding parameters
#' @export
lookup_ids <- function() {
}


#' add aheads, forecaster_ids, and ids to a list of ensemble models
#' @description
#' minor utility
#' @param ensemble_grid the list of ensembles,
#' @param aheads the aheads to add
#' @inheritParams add_id
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
#' @export
id_ahead_ensemble_grid <- function(ensemble_grid, aheads, n_adj = 2) {
  ensemble_grid <- expand_grid(
    ensemble_grid,
    tibble(ahead = aheads)
  )

  ensemble_grid %<>%
    add_id(., n_adj = 2) %>%
    rowwise() %>%
    mutate(forecaster_ids = list(map2_vec(forecasters, ahead, single_id, n_adj = 2)))
  return(ensemble_grid)
}
