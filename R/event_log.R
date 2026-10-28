# One append-only log per targets project that the runner and its crew workers
# all write to, so a single `tail -f` (or `make status`) shows what is running.
# See notes/run-logs.md.

event_log_path <- function(project = Sys.getenv("TAR_RUN_PROJECT", "unknown")) {
  here::here("cache", "logs", sprintf("%s_events.log", project))
}

# Writes `<iso time> pid=<pid> <message>` to the project's event log and echoes
# the line to stdout (which crew keeps in local_logs/).
log_event <- function(..., project = Sys.getenv("TAR_RUN_PROJECT", "unknown")) {
  path <- event_log_path(project)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  line <- sprintf("%s pid=%d %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), Sys.getpid(), paste0(...))
  cat(line, "\n", sep = "", file = path, append = TRUE)
  message(line)
  invisible(line)
}

# Workers whose latest event is a `start`: pid, what they started, since when,
# and whether the process still exists. Empty data frame if none.
events_in_flight <- function(lines, now = Sys.time(), pid_alive = \(pid) file.exists(file.path("/proc", pid))) {
  m <- regmatches(lines, regexec("^(\\S+) pid=(\\d+) (start|done) (.*)$", lines))
  m <- m[lengths(m) == 5L]
  empty <- data.frame(pid = integer(), what = character(), since = character(), elapsed = character(), alive = logical())
  if (length(m) == 0L) {
    return(empty)
  }
  events <- data.frame(
    time = as.POSIXct(vapply(m, `[`, "", 2L), format = "%Y-%m-%dT%H:%M:%S%z"),
    pid = as.integer(vapply(m, `[`, "", 3L)),
    kind = vapply(m, `[`, "", 4L),
    what = vapply(m, `[`, "", 5L)
  )
  last <- events[!duplicated(events$pid, fromLast = TRUE), ]
  running <- last[last$kind == "start", ]
  if (nrow(running) == 0L) {
    return(empty)
  }
  data.frame(
    pid = running$pid,
    what = running$what,
    since = format(running$time, "%H:%M:%S"),
    elapsed = format(round(difftime(now, running$time, units = "mins"), 1)),
    alive = vapply(running$pid, pid_alive, logical(1))
  )
}
