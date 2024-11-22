# FluSight 2024-2025 Forecaster Testing

## Flu Forecasting

To render the notebook, you will need `renv::restore()` to install the necessary packages.
Then, you can run the following commands to render the notebooks:

```r
library(aws.s3)
library(dplyr)

s3load("flu_2023_forecaster_parameter_combinations.rds", bucket = "forecasting-team-data")
forecaster_families <- setdiff(forecaster_parameter_combinations_ %>% names, c("flusion_grf"))
reports_dir <- "reports"

# Indivudal notebooks
for (name in forecaster_families) {
  output_file <- file.path(reports_dir, paste0("flu-notebook-", name, ".html"))
  rmarkdown::render("flu-comparison-notebook.Rmd", params = list(forecaster_set = name), output_file = output_file)
}

# Overall notebook
rmarkdown::render("flu-overall-comparison-notebook.Rmd", output_file = file.path(reports_dir, "flu-overall-notebook.html"))
```

## Covid Forecasting

```r
for (i in 1:3) {
  output_file <- file.path(reports_dir, paste0("covid-notebook-", i, ".html"))
  rmarkdown::render("covid-comparison-notebook.Rmd", params = list(forecaster_set = i), output_file = output_file)
}
```
