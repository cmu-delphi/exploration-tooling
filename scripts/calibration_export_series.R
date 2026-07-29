# Export real hub series + our qt_track() output, for cross-checking the R port
# against the authors' Python.
#
# Pairs with ~/repos/delphi/multiQT/check_r_port_real_series.py. Run this first,
# then that. Exports only series that are complete across every round, because
# the reference Python has no notion of an inactive round and could not be given
# the identical problem otherwise.
#
# Usage:
#   Rscript scripts/calibration_export_series.R [outdir] [n_series]

suppressPackageStartupMessages(source(here::here("R/load_all.R")))

args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else here::here("cache/calibration/crosscheck")
n_series <- if (length(args) >= 2) as.integer(args[[2]]) else 4L

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

forecasts <- hub_read_forecasts()
truth <- hub_read_truth()
rounds <- hub_label_seasons(unique(forecasts$reference_date))
round_date <- rounds$round_date

complete <- forecasts %>%
  group_by(location, horizon) %>%
  summarize(n_rounds = n_distinct(reference_date), .groups = "drop") %>%
  filter(n_rounds == length(round_date))

if (nrow(complete) == 0L) {
  cli::cli_abort("No series is complete across all {length(round_date)} rounds.")
}
# Spread the picks across the table rather than taking the first few, so the
# check touches several locations and horizons.
picks <- complete[unique(round(seq(1, nrow(complete), length.out = n_series))), ]
cli::cli_alert_info(
  "Exporting {nrow(picks)} complete series to {.path {outdir}}
   (of {nrow(complete)} complete, {nrow(distinct(forecasts, location, horizon))} total)"
)

for (i in seq_len(nrow(picks))) {
  loc <- picks$location[i]
  h <- picks$horizon[i]
  series <- forecasts %>% filter(location == loc, horizon == h)
  Yhat <- hub_series_matrix(series, round_date, length(HUB_QUANTILE_LEVELS))

  target_end_date <- round_date + 7L * h
  Y <- truth$truth[match(
    paste(loc, target_end_date), paste(truth$location, truth$target_end_date)
  )]

  learnable <- !is.na(Y) & !apply(is.na(Yhat), 2, any)
  delay <- qt_delay_from_dates(round_date, target_end_date, settle_days = 14L)
  delay <- lapply(delay, function(idx) idx[learnable[idx]])

  # Settings must match what check_r_port_real_series.py passes to projectedQT.
  res <- qt_track(
    Y = Y, Yhat = Yhat, levels = HUB_QUANTILE_LEVELS, delay = delay,
    lr = "adaptive+", lr_window = 50,
    projection = "isotonic", eval_grad_at = "played"
  )

  tag <- sprintf("%s_h%d", loc, h)
  # Python has no NA; unrevealed outcomes are never read, so 0 is a safe filler.
  # Full precision, or the comparison measures CSV rounding rather than the port.
  wr <- function(x, suffix) {
    utils::write.csv(x, file.path(outdir, paste0(tag, "_", suffix, ".csv")),
      row.names = FALSE
    )
  }
  old <- options(digits = 17)
  wr(data.frame(Y = ifelse(is.na(Y), 0, Y)), "Y")
  wr(Yhat, "Yhat")
  wr(res$played, "played_R")
  revealed <- purrr::imap(delay, function(idx, t) {
    if (length(idx) == 0L) NULL else data.frame(round = t, revealed = idx)
  }) %>% bind_rows()
  wr(revealed, "delay")
  options(old)

  cli::cli_alert_success(
    "{tag}: T={length(Y)}, revealed={nrow(revealed)}, truth missing={sum(is.na(Y))}"
  )
}

utils::write.csv(
  data.frame(level = HUB_QUANTILE_LEVELS),
  file.path(outdir, "levels.csv"),
  row.names = FALSE
)
cli::cli_alert_info(
  "Now run:
   cd ~/repos/delphi/multiQT && uv run --with numpy --with scikit-learn
   --with matplotlib python check_r_port_real_series.py {outdir}"
)
