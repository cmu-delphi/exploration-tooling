for (file in list.files(here::here("R"), recursive = TRUE, full.names = TRUE)) {
  if (basename(file) != "load_all.R") source(file)
}
