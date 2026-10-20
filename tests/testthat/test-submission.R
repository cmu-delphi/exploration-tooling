suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

pred <- tibble::tibble(location = "01", value = 1)

test_that("write_submission_file creates the model directory on a first submission", {
  hub <- withr::local_tempdir()
  dir.create(file.path(hub, "model-output"))
  model_dir <- file.path(hub, "model-output", "CMU-TimeSeries")

  write_submission_file(pred, as.Date("2026-10-17"), model_dir)

  expect_true(file.exists(file.path(model_dir, "2026-10-17-CMU-TimeSeries.csv")))
})

test_that("write_submission_file errors when the hub has no model-output directory", {
  hub <- withr::local_tempdir()
  expect_error(
    write_submission_file(pred, as.Date("2026-10-17"), file.path(hub, "model-output", "CMU-TimeSeries")),
    "does not exist"
  )
})
