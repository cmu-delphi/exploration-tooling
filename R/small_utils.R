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
#' feed a character represenation of the column contents through md5 and reencoding in base64 to get short unique hashes
#' make sure that there are no columns with `NA`'s
#' @param df the df to add a column to. everything should be convertable to a string
#' @param name_length the number of base64 characters to keep from the md5 encoding
#' @import openssl
#' @export
add_id <- function(df, name_length = 5) {
  df %<>%
    rowwise() %>%
    mutate(id = md5(paste(across(everything()), collapse = ""))) %>% # make a full md5 hash
    mutate(id = openssl::base64_encode((id))) %>%
    mutate(id = substr(id, 1, name_length)) # only keep first  `name_length` characters
  return(df)
}
