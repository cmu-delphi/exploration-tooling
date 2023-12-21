library(targets)
library(shiny)
use_aws_s3 <- as.logical(Sys.getenv("USE_AWS_S3", FALSE))
if (use_aws_s3) {
  epieval::manage_S3_forecast_cache()
} else {
  cli::cli_inform("Syncing turned off")
}
