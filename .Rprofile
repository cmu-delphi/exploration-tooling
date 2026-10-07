source("renv/activate.R")

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
