# crew (1.3.3 and its main branch as of 2026-10-28) never sets `status` for a
# timed-out task, so tar_make() fails without naming the target: "object
# 'status' not found", or "invalid subscript type 'closure'" when an attached
# package (renv) exports a status() function. This adds the missing fallback to
# crew's internal as_monad(). It checks crew's behavior first, so it does
# nothing once crew is fixed. Upstream: https://github.com/wlandau/crew/issues/254.
# See notes/quantreg-solver.md.
patch_crew_timeout_status <- function() {
  ns <- asNamespace("crew")
  # 5L is mirai's "timed out" code.
  probe <- tryCatch(
    ns$as_monad(list(data = 5L), name = "probe", controller = "probe"),
    error = function(e) NULL
  )
  status <- if (is.list(probe)) .subset2(probe, "status") else NULL
  if (is.character(status) && length(status) == 1L) {
    return(invisible(FALSE))
  }
  patched <- function(task, name, controller) {
    out <- .subset2(task, "data")
    if (is.list(out)) {
      return(out)
    }
    if (!is.integer(out) || length(out) != 1L || anyNA(out)) {
      status <- "error"
      error <- as.character(out)
      code <- -1L
    } else {
      code <- as.integer(out)
      error <- nanonext::nng_error(code)
      status <- if (identical(code, code_crash)) {
        "crash"
      } else if (identical(code, code_cancel)) {
        "cancel"
      } else {
        "error"
      }
    }
    monad_init(name = name, status = status, error = error, code = code, controller = controller)
  }
  environment(patched) <- ns
  utils::assignInNamespace("as_monad", patched, "crew")
  invisible(TRUE)
}

# Branches and targets that tar_progress() still shows as dispatched: in flight,
# or in flight when the run stopped.
dispatched_targets <- function(project) {
  store <- targets::tar_config_get("store", project = project)
  progress <- tryCatch(
    suppressMessages(targets::tar_progress(store = store, fields = c(type, progress))),
    error = function(cond) NULL
  )
  if (is.null(progress)) {
    return(character(0))
  }
  progress$name[progress$progress == "dispatched" & progress$type != "pattern"]
}
