suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

pred <- tibble::tibble(location = "01", value = 1)

# validate_submission() returns the checks grouped by where they ran.
as_collection <- function(...) {
  structure(
    list(`hub-config` = structure(list(...)[1], class = c("hub_validations", "list")),
         `model/file.csv` = structure(list(...)[-1], class = c("hub_validations", "list"))),
    class = c("hub_validations_collection", "list")
  )
}

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

test_that("validate_submission_or_abort lists every failing check in its error", {
  checks <- as_collection(
    valid_config = structure(list(message = "Config valid."), class = c("check_success", "hub_check")),
    file_exists = structure(list(message = "File exists."), class = c("check_success", "hub_check")),
    valid_vals = structure(list(message = "Values `-1` are not valid."), class = c("check_failure", "hub_check")),
    rows_unique = structure(list(message = "Duplicate rows found."), class = c("check_error", "hub_check"))
  )
  local_mocked_bindings(validate_submission = function(...) checks, .package = "hubValidations")
  err <- expect_error(validate_submission_or_abort("hub", "model/file.csv"), class = "rlang_error")
  expect_match(conditionMessage(err), "valid_vals: Values `-1` are not valid.", fixed = TRUE)
  expect_match(conditionMessage(err), "rows_unique: Duplicate rows found.", fixed = TRUE)
  expect_no_match(conditionMessage(err), "file_exists")
})

test_that("validate_submission_or_abort returns the validation when every check passes", {
  checks <- as_collection(
    valid_config = structure(list(message = "Config valid."), class = c("check_success", "hub_check")),
    file_exists = structure(list(message = "File exists."), class = c("check_success", "hub_check"))
  )
  local_mocked_bindings(validate_submission = function(...) checks, .package = "hubValidations")
  expect_identical(validate_submission_or_abort("hub", "model/file.csv"), checks)
})

test_that("validate_submission_or_abort cuts long check messages", {
  checks <- as_collection(
    valid_config = structure(list(message = "Config valid."), class = c("check_success", "hub_check")),
    valid_vals = structure(list(message = strrep("a", 500)), class = c("check_failure", "hub_check"))
  )
  local_mocked_bindings(validate_submission = function(...) checks, .package = "hubValidations")
  err <- expect_error(validate_submission_or_abort("hub", "model/file.csv", max_chars = 50))
  expect_match(conditionMessage(err), paste0("valid_vals: ", strrep("a", 47), "..."), fixed = TRUE)
})

test_that("validate_submission_or_abort summarizes the rows a custom check flagged", {
  flagged <- tibble::tibble(
    location = c("US", "US", "06"), target = "wk inc flu prop ed visits", horizon = c(3L, 3L, 3L),
    output_type_id = c("0.99", "0.9", "0.99"), value = c(1.26, 0.35, 0.28)
  )
  checks <- as_collection(
    valid_config = structure(list(message = "Config valid."), class = c("check_success", "hub_check")),
    max_prop_ed_visits = structure(
      list(message = "Values too high.", error_object = flagged),
      class = c("check_failure", "hub_check")
    )
  )
  local_mocked_bindings(validate_submission = function(...) checks, .package = "hubValidations")
  err <- expect_error(validate_submission_or_abort("hub", "model/file.csv"))
  expect_match(conditionMessage(err), "location US, wk inc flu prop ed visits, horizon 3: 2 value(s), max 1.26, from quantile 0.9 up", fixed = TRUE)
  expect_match(conditionMessage(err), "location 06, wk inc flu prop ed visits, horizon 3: 1 value(s), max 0.28, from quantile 0.99 up", fixed = TRUE)
})
