source(here::here("R", "imports.R"))
for (file in list.files(here::here("R"), recursive = TRUE, full.names = TRUE)) {
  if (!(basename(file) %in% c("imports.R", "load_all.R"))) source(file)
}
