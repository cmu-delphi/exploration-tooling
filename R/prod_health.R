# Weekly prod health check: which ensemble components and submitted
# (geo, horizon) pairs are missing, plus the run's warnings and errors.
# See notes/prod-health-check.md for what is checked and how alerts are sent.

#' Coverage of the submitted ensemble and of its components for one signal.
#'
#' @param forecasts component forecasts (`forecast_filtered[[signal]]`).
#' @param clim_lin the climate_linear ensemble for the signal (a component too).
#' @param submitted the submitted ensemble for the signal (`ensemble_mixture[[signal]]`).
#' @param weights prod weights for the signal (`geo_weights[[signal]]`).
#' @param spec the `ensemble_mix` ensemble spec; `clim_id` the climate_linear id.
#' @param signal "nhsn" or "nssp".
#' @param aheads horizons in weeks.
#' @return a list of two tibbles:
#'   - `components`: one row per configured (component, geo, ahead) with a
#'     positive weight, its configured share of the ensemble, and whether it
#'     produced a forecast;
#'   - `submission`: one row per (geo, ahead) that should be submitted, with
#'     whether it was.
#'   Expected geos are those any forecaster produced for the signal, minus geos
#'   with no positive weight.
prod_ensemble_coverage <- function(forecasts, clim_lin, submitted, weights, spec, clim_id, signal, aheads) {
  components <- c(spec$components[[signal]], clim_id)
  dropped <- if (isTRUE(spec$drop_negative_aheads[[signal]])) {
    setdiff(spec$components[[signal]], spec$drop_negative_aheads_exempt[[signal]] %||% character(0))
  } else {
    character(0)
  }
  expected_geos <- unique(forecasts$geo_value)
  configured <- weights %>%
    filter(forecaster %in% components, geo_value %in% expected_geos) %>%
    expand_weights_by_ahead(tidyr::expand_grid(forecaster = components, ahead = as.integer(aheads))) %>%
    # Non-exempt components are dropped at negative aheads by design, not missing.
    filter(!(forecaster %in% dropped & ahead < 0), ahead %in% aheads, weight > 0) %>%
    group_by(geo_value, ahead) %>%
    mutate(share = weight / sum(weight)) %>%
    ungroup() %>%
    select(forecaster, geo_value, ahead, share)
  present <- bind_rows(forecasts %>% filter(forecaster %in% components), clim_lin %>% mutate(forecaster = clim_id)) %>%
    add_week_ahead() %>%
    distinct(forecaster, geo_value, ahead) %>%
    mutate(present = TRUE)
  component_cov <- configured %>%
    left_join(present, by = c("forecaster", "geo_value", "ahead")) %>%
    mutate(present = coalesce(present, FALSE), signal = signal)
  submitted_pairs <- submitted %>%
    add_week_ahead() %>%
    distinct(geo_value, ahead) %>%
    mutate(submitted = TRUE)
  submission_cov <- configured %>%
    distinct(geo_value, ahead) %>%
    left_join(submitted_pairs, by = c("geo_value", "ahead")) %>%
    mutate(submitted = coalesce(submitted, FALSE), signal = signal)
  list(components = component_cov, submission = submission_cov)
}

#' Summarize coverage into a status.
#'
#' "fail" when any (signal, horizon) is missing from more than `fail_share` of
#' its expected geos; "attention" when anything is missing or substituted;
#' otherwise "ok".
#' @param coverage a list of [prod_ensemble_coverage] results, one per signal.
prod_health_status <- function(coverage, fail_share = 0.25) {
  submission <- bind_rows(purrr::map(coverage, "submission"))
  components <- bind_rows(purrr::map(coverage, "components"))
  by_horizon <- submission %>%
    group_by(signal, ahead) %>%
    summarize(expected = n(), missing = sum(!submitted), .groups = "drop")
  failing <- by_horizon %>% filter(missing > fail_share * expected)
  reasons <- c(
    sprintf("%s h%+d: %d of %d locations not submitted", failing$signal, failing$ahead, failing$missing, failing$expected),
    if (any(!components$present)) {
      sprintf("%d (component, location, horizon) forecasts missing; their weight went to other components", sum(!components$present))
    },
    if (any(!submission$submitted) && nrow(failing) == 0) {
      sprintf("%d (location, horizon) pairs not submitted", sum(!submission$submitted))
    }
  )
  status <- if (nrow(failing) > 0) "fail" else if (length(reasons) > 0) "attention" else "ok"
  list(status = status, reasons = reasons, by_horizon = by_horizon)
}

#' Warnings and errors recorded by targets for one forecast date.
#'
#' Keeps targets whose name carries the date and targets with no date in their
#' name (shared data targets). The metadata holds each target's latest build, so
#' a shared target's warning can predate this run; `time` says when.
#' @param store a targets store path.
#' @param forecast_date the round's forecast date.
pipeline_messages <- function(store, forecast_date) {
  date_tag <- gsub("-", ".", format(as.Date(forecast_date)))
  meta <- targets::tar_meta(store = store, fields = c("name", "warnings", "error", "time"), complete_only = FALSE)
  meta %>%
    filter(grepl(date_tag, name, fixed = TRUE) | !grepl("[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}", name)) %>%
    tidyr::pivot_longer(c(warnings, error), names_to = "kind", values_to = "message") %>%
    filter(!is.na(message), nzchar(message)) %>%
    mutate(kind = if_else(kind == "error", "error", "warning")) %>%
    arrange(desc(kind == "error"), name) %>%
    select(kind, target = name, message, time)
}

#' Render the health notebook for the latest forecast date in a prod store.
#'
#' Runs after `tar_make()`, so it can report on a pipeline that errored. Writes
#' `rendered_reports/{forecast_date}_{disease}_health_on_{today}.html`.
#' @param project a targets project name from `_targets.yaml` (e.g. "flu_hosp_prod").
#' @param disease "flu" or "covid".
#' @return a list with `status` ("ok", "attention", "fail", or "error" when the
#'   pipeline failed), `reasons`, and the report `file`.
render_prod_health <- function(project, disease) {
  store <- targets::tar_config_get("store", project = project)
  names <- targets::tar_meta(store = store, fields = "name")$name
  # The round is the latest forecast_filtered date, so a run that errored before
  # health_coverage still reports on the right round.
  dated <- sub("^forecast_filtered_", "", grep("^forecast_filtered_[0-9]", names, value = TRUE))
  latest <- if (length(dated) > 0) max(dated) else gsub("-", ".", format(Sys.Date()))
  forecast_date <- as.Date(gsub("\\.", "-", latest))
  coverage <- tryCatch(
    targets::tar_read_raw(paste0("health_coverage_", latest), store = store),
    error = function(e) NULL
  )
  messages <- pipeline_messages(store, forecast_date)
  health <- if (is.null(coverage)) {
    list(status = "error", reasons = "health_coverage was not built (see errors below)", by_horizon = tibble())
  } else {
    prod_health_status(coverage)
  }
  if (any(messages$kind == "error") && health$status != "fail") {
    health$status <- "error"
    health$reasons <- c(sprintf("%d target(s) errored", sum(messages$kind == "error")), health$reasons)
  }
  dir.create(here::here("rendered_reports"), showWarnings = FALSE)
  file <- here::here(
    "rendered_reports",
    sprintf("%s_%s_health_on_%s.html", forecast_date, disease, Sys.Date())
  )
  rmarkdown::render(
    here::here("pipelines", "templates", "health_report.Rmd"),
    output_file = file,
    params = list(
      disease = disease, forecast_date = forecast_date, health = health,
      coverage = coverage, messages = messages
    ),
    quiet = TRUE
  )
  c(health, list(file = file))
}

#' Post a message to Slack through the incoming webhook in `SLACK_WEBHOOK_URL`.
#'
#' @return TRUE if sent, FALSE if no webhook is configured or the post failed.
notify_slack <- function(text) {
  url <- Sys.getenv("SLACK_WEBHOOK_URL")
  if (!nzchar(url)) {
    return(FALSE)
  }
  tryCatch(
    {
      httr2::request(url) %>%
        httr2::req_body_json(list(text = text)) %>%
        httr2::req_perform()
      TRUE
    },
    error = function(e) FALSE
  )
}
