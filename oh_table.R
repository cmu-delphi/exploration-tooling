suppressPackageStartupMessages(source(here::here("R/load_all.R")))

forecasts <- hub_read_forecasts()
truth <- hub_read_truth()

rounds <- hub_label_seasons(unique(forecasts$reference_date))
round_date <- rounds$round_date
levels <- sort(unique(forecasts$level))
m <- length(levels)
update_from <- !(rounds$season %in% "2023-2024")

h <- 2L
series <- forecasts %>% filter(location == "39", horizon == h)
Yhat <- matrix(NA_real_, nrow = m, ncol = length(round_date))
Yhat[cbind(match(series$level, levels), match(series$reference_date, round_date))] <- series$value

ted <- round_date + 7L * h
tl <- setNames(truth$truth, paste(truth$location, truth$target_end_date))
Y <- unname(tl[paste("39", ted)])
learnable <- !is.na(Y) & !apply(is.na(Yhat), 2, any)
delay <- qt_delay_from_dates(round_date, ted, 14L)
delay <- lapply(delay, function(idx) idx[learnable[idx]])

res <- qt_track(
  Y = Y, Yhat = Yhat, levels = levels, delay = delay,
  lr = "adaptive+", lr_window = 20, lr_args = list(mult = 0.03),
  projection = "isotonic", nonneg = TRUE,
  update_from = update_from, hidden_scale = rep(1, length(round_date))
)

# Subsample 6 levels across the fan
show_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
li <- match(show_levels, levels)

sel <- which(round_date >= as.Date("2025-12-01") & round_date <= as.Date("2026-05-31"))
fmt <- function(v) paste(formatC(v, format = "f", digits = 0, width = 5), collapse = " ")

cat(sprintf("OH, horizon %d. Vectors subsampled at levels: %s\n", h,
  paste(show_levels, collapse = " ")))
cat(sprintf("%-10s %6s %8s | %-29s | %-29s | %-29s | %-29s\n",
  "date", "eta", "truth",
  "pre-PAVA offset (hidden)", "post-PAVA offset (played-base)",
  "base forecast", "calibrated forecast"))
for (t in sel) {
  cat(sprintf("%-10s %6.1f %8s | %s | %s | %s | %s\n",
    format(round_date[t]),
    res$lr[t],
    ifelse(is.na(Y[t]), "NA", Y[t]),
    fmt(res$hidden[li, t]),
    fmt(res$offset[li, t]),
    fmt(Yhat[li, t]),
    fmt(res$played[li, t])
  ))
}
