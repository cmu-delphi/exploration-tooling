#' the quantile levels used by the covidhub repository
covidhub_probs <- function(type = c("standard", "inc_case")) {
  type <- match.arg(type)
  switch(type,
    standard = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
    inc_case = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  )
}
