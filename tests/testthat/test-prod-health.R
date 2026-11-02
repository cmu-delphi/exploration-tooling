suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("prod_ensemble_coverage separates by-design drops from missing components", {
  fd <- as.Date("2026-04-01")
  mk <- function(f, geo, ahead_wk) {
    tibble(forecaster = f, geo_value = geo, forecast_date = fd, target_end_date = fd + 7 * ahead_wk + 3, quantile = 0.5, value = 1)
  }
  # windowed_seasonal is dropped at h−1 by design; revision_aware carries h−1 but
  # is missing for tx; tx's h−1 is still submitted (climate_linear carried it).
  forecasts <- bind_rows(mk("windowed_seasonal", c("ca", "tx"), -1), mk("windowed_seasonal", c("ca", "tx"), 0), mk("revision_aware", "ca", -1))
  clim_lin <- bind_rows(mk("climate_linear", c("ca", "tx"), -1), mk("climate_linear", c("ca", "tx"), 0))
  submitted <- bind_rows(mk("ensemble_mix", c("ca", "tx"), -1), mk("ensemble_mix", "ca", 0))
  weights <- tibble(
    forecast_date = fd, forecaster = rep(c("windowed_seasonal", "climate_linear", "revision_aware"), 2),
    geo_value = rep(c("ca", "tx"), each = 3), ahead = rep(c(NA, NA, -1), 2), weight = rep(c(1, 0.001, 1), 2)
  )
  spec <- list(
    components = list(nhsn = c("windowed_seasonal", "revision_aware")),
    drop_negative_aheads = list(nhsn = TRUE), drop_negative_aheads_exempt = list(nhsn = "revision_aware")
  )
  cov <- prod_ensemble_coverage(forecasts, clim_lin, submitted, weights, spec, "climate_linear", "nhsn", -1:0)
  missing <- cov$components %>% filter(!present)
  expect_equal(missing$forecaster, "revision_aware")
  expect_equal(missing$geo_value, "tx")
  expect_false(any(cov$components$forecaster == "windowed_seasonal" & cov$components$ahead == -1))
  expect_equal(cov$submission %>% filter(!submitted) %>% select(geo_value, ahead), tibble(geo_value = "tx", ahead = 0L))
  status <- prod_health_status(list(nhsn = cov))
  # tx h0 is half of h0's locations, over the 25% fail threshold.
  expect_equal(status$status, "fail")
})

test_that("notify_slack starts a daily thread once and replies in it", {
  withr::local_envvar(SLACK_BOT_TOKEN = "xoxb-test", SLACK_CHANNEL = "C0TEST")
  thread_file <- withr::local_tempfile()
  bodies <- list()
  httr2::local_mocked_responses(function(req) {
    bodies[[length(bodies) + 1]] <<- req$body$data
    httr2::response_json(body = list(ok = TRUE, ts = sprintf("100.%d", length(bodies))))
  })
  expect_true(notify_slack("first", thread_file = thread_file))
  expect_true(notify_slack("second", broadcast = TRUE, thread_file = thread_file))
  expect_length(bodies, 3)
  expect_null(bodies[[1]]$thread_ts)
  expect_match(bodies[[1]]$text, "prod forecasts for")
  expect_equal(readLines(thread_file), "100.1")
  expect_equal(bodies[[2]]$thread_ts, "100.1")
  expect_false(bodies[[2]]$reply_broadcast)
  expect_equal(bodies[[3]]$thread_ts, "100.1")
  expect_true(bodies[[3]]$reply_broadcast)
})

test_that("notify_slack returns FALSE when Slack responds with ok = FALSE", {
  withr::local_envvar(SLACK_BOT_TOKEN = "xoxb-test", SLACK_CHANNEL = "C0TEST")
  thread_file <- withr::local_tempfile()
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(ok = FALSE, error = "not_in_channel")))
  expect_message(expect_false(notify_slack("hi", thread_file = thread_file)), "not_in_channel")
  expect_false(file.exists(thread_file))
})

test_that("notify_slack returns FALSE without a token or channel", {
  withr::local_envvar(SLACK_BOT_TOKEN = "", SLACK_CHANNEL = "C0TEST")
  expect_false(notify_slack("hi", thread_file = withr::local_tempfile()))
})
