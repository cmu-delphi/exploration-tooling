#!/usr/bin/env Rscript
# Sweep our hub submissions for forecasts far from the data, to catch pipeline
# mistakes. See notes/spoiled-submissions.md for what it has found.
#
# For every round, location and horizon it joins the submitted 2.5/50/97.5%
# quantiles with the NHSN value as published when the round was forecast
# (`asof_value`; `latest_value` is the latest week then reported) and the
# finalized value. It writes the joined table to
# cache/submission_sweep_{disease}.csv and prints two flags at h−1 and h0:
#
# - suspect: the median is off by more than 2x from BOTH the value known at
#   the time and the finalized value, which a model rarely does on its own.
# - missed: the finalized value is outside the 95% interval and the median is
#   off from it by more than 2x. This also catches data problems (bad first
#   reports, data gaps), so read it alongside `asof_value`.
#
# Usage: Rscript scripts/one_offs/sweep_submission_deviations.R [flu|covid ...]

suppressPackageStartupMessages(source(here::here("R/load_all.R")))

diseases <- commandArgs(trailingOnly = TRUE)
if (length(diseases) == 0) diseases <- c("flu", "covid")
hub_dirs <- c(flu = "../FluSight-forecast-hub", covid = "../covid19-forecast-hub")
targets <- c(flu = HUB_FLU_TARGET, covid = HUB_COVID_TARGET)

sweep_one <- function(disease) {
  archive <- get_nhsn_data_archive(disease)
  vintages <- nhsn_read_vintages(disease, archive)
  final <- nhsn_read_truth(disease, archive) %>% rename(final = truth)
  fc <- suppressMessages(hub_read_forecasts(here::here(hub_dirs[[disease]]), target = targets[[disease]], drop_spoiled = FALSE)) %>%
    filter(level %in% c(0.025, 0.5, 0.975), reference_date >= min(vintages$version) + 3) %>%
    mutate(q = c(`0.025` = "lo", `0.5` = "med", `0.975` = "hi")[as.character(level)]) %>%
    select(reference_date, horizon, target_end_date, location, q, value) %>%
    tidyr::pivot_wider(names_from = q, values_from = value)
  asof <- purrr::map(sort(unique(fc$reference_date)), function(rd) {
    hub_vintage_snapshot(vintages, hub_round_asof(rd)) %>%
      filter(target_end_date >= rd - 28) %>%
      group_by(location) %>%
      mutate(latest_week = max(target_end_date), latest_value = truth[target_end_date == max(target_end_date)]) %>%
      ungroup() %>%
      mutate(reference_date = rd)
  }) %>% bind_rows()
  fc %>%
    left_join(select(asof, reference_date, location, target_end_date, asof_value = truth),
      by = c("reference_date", "location", "target_end_date")) %>%
    left_join(distinct(asof, reference_date, location, latest_week, latest_value), by = c("reference_date", "location")) %>%
    left_join(final, by = c("location", "target_end_date")) %>%
    mutate(
      disease = disease,
      known = coalesce(asof_value, latest_value),
      lr_known = log(pmax(med, 0.5) / known),
      lr_final = log(pmax(med, 0.5) / final)
    )
}

show_flag <- function(df, title) {
  cat("\n", title, "\n", sep = "")
  df %>%
    arrange(desc(abs(lr_final))) %>%
    group_by(disease, reference_date, horizon) %>%
    summarize(
      n = n(),
      `worst (median / known then / final)` = paste(head(sprintf("%s %.0f/%.0f/%.0f", location, med, known, final), 4), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(disease, reference_date, horizon) %>%
    print(n = Inf, width = Inf)
}

for (disease in diseases) {
  res <- sweep_one(disease)
  readr::write_csv(res, here::here("cache", paste0("submission_sweep_", disease, ".csv")))
  near <- res %>% filter(horizon %in% c(-1, 0), final >= 20, known >= 20)
  show_flag(near %>% filter(abs(lr_known) > log(2), abs(lr_final) > log(2)), paste0("== ", disease, ": suspect"))
  show_flag(near %>% filter(final < lo | final > hi, abs(lr_final) > log(2)), paste0("== ", disease, ": missed"))
}
