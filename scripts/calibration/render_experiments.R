# Render calibration experiment notebooks into reports/calibration_experiments/
# and rebuild its index.html (an overview plus links to every rendered notebook).
#
# Usage: Rscript scripts/calibration/render_experiments.R [learning_rate ...]
# With no arguments, renders every notebook; `--index` renders only the index.

src_dir <- here::here("reports/writeups/calibration_experiments")
out_dir <- here::here("reports/calibration_experiments")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

args <- commandArgs(trailingOnly = TRUE)
notebooks <- if (identical(args, "--index")) {
  character(0)
} else if (length(args) > 0) {
  file.path(src_dir, paste0(sub("[.]Rmd$", "", args), ".Rmd"))
} else {
  setdiff(list.files(src_dir, pattern = "^[^_].*[.]Rmd$", full.names = TRUE), file.path(src_dir, "index.Rmd"))
}
for (nb in notebooks) {
  rmarkdown::render(nb, output_dir = out_dir, envir = new.env(), quiet = TRUE)
}
rmarkdown::render(file.path(src_dir, "index.Rmd"), output_dir = out_dir, envir = new.env(), quiet = TRUE)
