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
#' @import openssl dplyr
#' @importFrom cli hash_animal
#' @export
add_id <- function(df, n_adj = 2) {
  stringified <- df %>%
    select(-ahead) %>%
    rowwise() %>%
    mutate(id = paste(across(everything()), collapse = ""), .keep="none") %>%
    mutate(id = hash_animal(id, n_adj = n_adj)$words) %>%
    mutate(id = paste(id[1:n_adj], sep="", collapse = " "))
  df %<>%
    mutate(id = stringified) %>%
    rowwise() %>%
    mutate(id = paste(id, ahead, collapse = " ")) %>%
    ungroup()
  return(df)
}
