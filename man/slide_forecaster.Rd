% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/looping.R
\name{slide_forecaster}
\alias{slide_forecaster}
\title{generate forecaster predictions on a particular dataset}
\usage{
slide_forecaster(
  data,
  outcome,
  extra_sources = "",
  forecaster = scaled_pop,
  slide_training = 0,
  n_training_pad = 5,
  forecaster_args = list(),
  forecaster_args_names = list(),
  date_range_step_size = 1L
)
}
\arguments{
\item{data}{the epi_df object}

\item{outcome}{the name of the target column}

\item{extra_sources}{any extra columns used for prediction that aren't
the target}

\item{forecaster}{a function that does the actual forecasting for a given
day. See \code{exampleSpec.R} for an example function and its documentation
for the general parameter requirements.}

\item{slide_training}{a required parameter that governs how much data to
exclude before starting the evaluation.}

\item{n_training_pad}{a required parameter that determines how many extra
samples for epix_slide to hand to the forecaster to guarantee that at
least \code{ntraining} examples are available to the forecaster.}

\item{forecaster_args}{the list of arguments to the forecaster; it must
contain \code{ahead}}

\item{forecaster_args_names}{a bit of a hack around targets, it contains
the names of the \code{forecaster_args}.}

\item{date_range_step_size}{the step size (in days) to use when generating
the forecast dates.}
}
\description{
a wrapper that turns a forecaster, parameters, data
combination into an actual experiment that outputs a
prediction.
as far as batchtools is concerned, the scoring function is a particular
parameter of the forecaster (or Algorithm, as they call it).
}
