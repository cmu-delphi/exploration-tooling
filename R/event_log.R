# One append-only log per targets project that the runner and its crew workers
# all write to, so a single `tail -f` (or `make status`) shows what is running.
# Every line starts with who wrote it: `[runner]` or `[worker <pid> <target>]`.
# See notes/run-logs.md.

event_log_path <- function(project = Sys.getenv("TAR_RUN_PROJECT", "unknown")) {
  here::here("cache", "logs", sprintf("%s_events.log", project))
}

# Name of the target the calling crew worker is building, or "-" outside one.
current_target_name <- function() {
  tryCatch(targets::tar_name(), error = function(e) "-")
}

# Appends `[<who>] <iso time> <message>` to the project's event log. `who`
# defaults to this worker's pid and target; the runner passes "runner".
log_event <- function(..., who = NULL, project = Sys.getenv("TAR_RUN_PROJECT", "unknown")) {
  path <- event_log_path(project)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (is.null(who)) {
    who <- sprintf("worker %d %s", Sys.getpid(), current_target_name())
  }
  line <- sprintf("[%s] %s %s", who, format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), paste0(..., collapse = " "))
  line <- gsub("\n", " ", line, fixed = TRUE)
  cat(line, "\n", sep = "", file = path, append = TRUE)
  invisible(line)
}

# Runs `expr`, logging every warning, message and error it raises. Conditions
# are not muffled, so targets still records them and errors still stop the run.
with_event_log <- function(expr, label) {
  withCallingHandlers(
    expr,
    warning = function(w) log_event(sprintf("warning %s: %s", label, conditionMessage(w))),
    message = function(m) log_event(sprintf("message %s: %s", label, trimws(conditionMessage(m)))),
    error = function(e) log_event(sprintf("error %s: %s", label, conditionMessage(e)))
  )
}

# Workers whose latest start/done event is a `start`: pid, target, what they
# started, since when, and whether the process still exists. Empty data frame
# if none.
events_in_flight <- function(lines, now = Sys.time(), pid_alive = \(pid) file.exists(file.path("/proc", pid))) {
  m <- regmatches(lines, regexec("^\\[worker (\\d+) (\\S+)\\] (\\S+) (start|done) (.*)$", lines))
  m <- m[lengths(m) == 6L]
  empty <- data.frame(pid = integer(), target = character(), what = character(), since = character(), elapsed = character(), alive = logical())
  if (length(m) == 0L) {
    return(empty)
  }
  events <- data.frame(
    pid = as.integer(vapply(m, `[`, "", 2L)),
    target = vapply(m, `[`, "", 3L),
    time = as.POSIXct(vapply(m, `[`, "", 4L), format = "%Y-%m-%dT%H:%M:%S%z"),
    kind = vapply(m, `[`, "", 5L),
    what = vapply(m, `[`, "", 6L)
  )
  last <- events[!duplicated(events$pid, fromLast = TRUE), ]
  running <- last[last$kind == "start", ]
  if (nrow(running) == 0L) {
    return(empty)
  }
  data.frame(
    pid = running$pid,
    target = running$target,
    what = running$what,
    since = format(running$time, "%H:%M:%S"),
    elapsed = format(round(difftime(now, running$time, units = "mins"), 1)),
    alive = vapply(running$pid, pid_alive, logical(1))
  )
}

# Worker lines that report a warning or error, newest last.
events_problems <- function(lines) {
  grep("^\\[worker [^]]*\\] \\S+ (warning|error) ", lines, value = TRUE)
}
