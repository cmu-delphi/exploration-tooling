# On Ubuntu, install packages as Linux binaries from Posit Package Manager
# instead of compiling them (rocker images set the same thing in Rprofile.site).
# The lockfile's repository is the source-only URL, so renv needs the override.
local({
  os <- tryCatch(readLines("/etc/os-release"), error = function(e) character())
  field <- function(key) sub(sprintf('^%s="?([^"]*)"?$', key), "\\1", grep(sprintf("^%s=", key), os, value = TRUE))
  codename <- field("VERSION_CODENAME")
  if (identical(field("ID"), "ubuntu") && length(codename) == 1 && nzchar(codename)) {
    url <- sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename)
    options(
      repos = c(CRAN = url),
      renv.config.repos.override = c(CRAN = url),
      HTTPUserAgent = sprintf(
        "R/%s R (%s)", getRversion(),
        paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
      )
    )
  }
})

source("renv/activate.R")

# Crew workers (set_targets_config() sets this) drop package attach and
# masking messages, which otherwise fill every worker log.
if (identical(Sys.getenv("QUIET_PACKAGE_STARTUP"), "true")) {
  globalCallingHandlers(packageStartupMessage = function(m) invokeRestart("muffleMessage"))
}

# R reads only one .Renviron: this project's file replaces ~/.Renviron. Read
# ~/.Renviron for user secrets (DELPHI_EPIDATA_KEY), then read the project file
# again so that its values have priority.
if (file.exists("~/.Renviron")) {
  readRenviron("~/.Renviron")
  readRenviron(".Renviron")
}

# Check if user .Rprofile exists
if (file.exists("~/.Rprofile")) {
  # Source user .Rprofile
  source("~/.Rprofile")
}
