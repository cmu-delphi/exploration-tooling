tar_project <- Sys.getenv("TAR_PROJECT", "covid_hosp_explore")
external_scores_path <- Sys.getenv("EXTERNAL_SCORES_PATH", "")
debug_mode <- as.logical(Sys.getenv("DEBUG_MODE", TRUE))
use_shiny <- as.logical(Sys.getenv("USE_SHINY", FALSE))
aws_s3_prefix <- Sys.getenv("AWS_S3_PREFIX", "2024")
aws_s3_prefix <- paste0(aws_s3_prefix, "/", tar_project)
suppressPackageStartupMessages({
  library(targets)
  library(shiny)
})
# Prevent functions defined in /R dir from being loaded unnecessarily
options(shiny.autoload.r = FALSE)

forecaster_options <- unique(tar_read(forecaster_params_grid)[["parent_id"]])
# Map forecaster names to score files
forecaster_options <- setNames(
  # File names
  paste0("score_", gsub(" ", ".", forecaster_options)),
  # Display names
  forecaster_options
)

# Add ensembles
ensemble_options <- unique(tar_read(ensemble_forecasters)[["parent_id"]])
ensemble_options <- setNames(
  # File names
  paste0("ensemble_score_", ensemble_options),
  # Display names
  paste0("ensemble.", ensemble_options)
)

external_options <- unique(tar_read(external_names))
EXTERNAL_PREFIX <- "[external] "
if (!is.null(external_options) && length(external_options) > 0 && external_scores_path != "") {
  external_options <- setNames(
    # File names
    # Get names of all branches of `external_scores` target by index. The way these
    # were specified, `external_names` provides the order of the branches.
    tar_branch_names(external_scores, seq_along(external_options)),
    # Display names
    paste0(
      EXTERNAL_PREFIX,
      gsub(" forecaster", "", gsub("_", " ", external_options, fixed = TRUE), fixed = TRUE)
    )
  )
} else {
  external_options <- character(0)
}

forecaster_options <- c(ensemble_options, forecaster_options, external_options)

runApp(here::here("scripts", "app.R"), port = 3838)
