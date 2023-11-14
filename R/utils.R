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

#' look up forecasters by name
#' @description
#' given a (partial) forecaster name, look up all forecasters in the given project which contain part of that name.
#' @param forecaster_name a part of the adj.adj.1 name used to identify the forecaster.
#' @param param_grid the tibble containing the mapping between
#' @param project the project to be used; by default, the environmental variable is used
#' @importFrom targets tar_read tar_config_get
#' @export
forecaster_lookup <- function(forecaster_name, param_grid = NULL, project = NULL) {
  forecaster_name <- strip_underscored(forecaster_name)
  if (is.null(project)) {
    project <- tar_config_get("store")
  }
  if (is.null(param_grid)) {
    param_grid <- tar_read(forecaster_params_grid, store = project)
  }
  param_grid %>% filter(grepl(forecaster_name, id))
}

strip_underscored <- function(x) {
  g <- gregexpr("_", x, fixed = TRUE)
  last_underscore <- g[[1]][[length(g[[1]])]]
  substr(x[[1]], start = last_underscore + 1, stop = nchar(x))
}

#' list forecasters used in the given ensemble table not found in the given forecaster grid
#' @description
#' list forecasters used in the given ensemble table not found in the given forecaster grid
#'
#' @param ensemble_grid the grid of ensembles used
#' @param param_grid the grid of forecasters used that we're checking for presence
#' @param project the project to be used; by default, the environmental variable is used
#' @export
ensemble_missing_forecasters <- function(ensemble_grid = NULL, param_grid = NULL, project = NULL) {
  if (is.null(project)) {
    project <- tar_config_get("store")
  }
  if (is.null(ensemble_grid)) {
    ensemble_grid <- tar_read(ensemble_forecasters, store = project)
  }
  used_forecasters <- unlist(ensemble_grid$forecaster_ids) %>% unique()
  is_present <- map_vec(used_forecasters, \(given_forecaster) nrow(forecaster_lookup(given_forecaster, param_grid, project)) > 0)
  absent_forecasters <- used_forecasters[!is_present]
  return(absent_forecasters)
}

#' given an ensemble and a list of forecasters used in some of those ensembles, return the ones that use them
#' @inheritParams ensemble_missing_forecasters
#' @export
ensemble_missing_forecasters_details <- function(ensemble_grid = NULL, param_grid = NULL, project = NULL) {
  absent_forecasters <- ensemble_missing_forecasters(ensemble_grid, param_grid, project)
  grid_with_missing <- ensemble_grid %>%
    rowwise() %>%
    mutate(
      missing_forecasters = list(map(
        absent_forecasters,
        # extract a list of the subforecasters with associated id, with only the missing ones having non-empty lists
        function(absent_fc) {
          is_missing <- grepl(absent_fc, forecaster_ids)
          params_only <- forecasters[is_missing]
          mapply(c, params_only, id = forecaster_ids[is_missing])
        }
      ))
    )
  flat_missing <- unlist(grid_with_missing$missing_forecasters, recursive = FALSE)
  unique_missing <- flat_missing[map_vec(flat_missing, \(x) length(x) > 0)] %>% unique()
  return(unique_missing)
}


#' generate an id from a simple list of parameters
#' @param param_list the list of parameters. must include `ahead` if `ahead = NULL`
#' @param ahead the ahead to use.
#' @inheritParams add_id
#' @export
single_id <- function(param_list, ahead = NULL, n_adj = 2) {
  full_hash <- param_list[names(param_list) != "ahead"] %>%
    .[order(names(.))] %>% # put in alphabetical order
    lapply(function(x) if (length(x) > 1) list(x) else x) %>% # the tibble version needs vectors to actually be lists, so this is a conversion to make sure the strings are identical
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

#' add aheads, forecaster_ids, and ids to a list of ensemble models
#' @description
#' First, do an expand grid to do a full combination of ensemble_grid x aheads.
#'   Then add a column containing lists of ids of the dependent forecasters
#'   based on their parameters.
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
  if (length(ensemble_grid$id %>% unique()) < length(ensemble_grid$id)) {
    abort("ensemble grid has non-unique forecasters")
  }
  return(ensemble_grid)
}


#' temporary patch that pulls `NA`'s out of an epi_df
#' @description
#' just delete rows that have NA's in them. eventually epipredict should directly handle this so we don't have to
#' @param epi_data the epi_df to be fixed
#' @importFrom tidyr drop_na
#' @importFrom epiprocess as_epi_df
#' @export
clear_lastminute_nas <- function(epi_data) {
  meta_data <- attr(epi_data, "metadata")
  epi_data %<>%
    drop_na() %>%
    as_epi_df()
  attr(epi_data, "metadata") <- meta_data
  return(epi_data)
}
