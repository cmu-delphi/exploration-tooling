% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ensemble_missing_forecasters}
\alias{ensemble_missing_forecasters}
\title{list forecasters used in the given ensemble table not found in the given forecaster grid}
\usage{
ensemble_missing_forecasters(
  ensemble_grid = NULL,
  param_grid = NULL,
  project = NULL
)
}
\arguments{
\item{ensemble_grid}{the grid of ensembles used}

\item{param_grid}{the grid of forecasters used that we're checking for presence}

\item{project}{the project to be used; by default, the environmental variable is used}
}
\description{
list forecasters used in the given ensemble table not found in the given forecaster grid
}
