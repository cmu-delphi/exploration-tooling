% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecaster_flatline.R
\name{flatline_fc}
\alias{flatline_fc}
\title{flatline forecaster (aka baseline)}
\usage{
flatline_fc(
  epi_data,
  outcome,
  extra_sources = "",
  ahead = 1,
  trainer = parsnip::linear_reg(),
  quantile_levels = covidhub_probs(),
  ...
)
}
\arguments{
\item{epi_data}{the actual data used}

\item{outcome}{the name of the target variable}

\item{extra_sources}{the name of any extra columns to use. This list could be
empty}

\item{ahead}{(this is relative to the \code{as_of} field of the \code{epi_df}, which is
likely \emph{not} the same as the \code{ahead} used by epipredict, which is relative
to the max time value of the \code{epi_df}. how to handle this is a modelling
question left up to each forecaster; see latency_adjusting.R for the
existing examples)}

\item{trainer}{an example extra parameter that is fairly common}

\item{quantile_levels}{The quantile levels to predict. Defaults to those required by
covidhub.}

\item{...}{it can also have any number of other parameters. In this case, the
\code{...} args are all inputs to \code{\link[epipredict:arx_args_list]{epipredict::arx_args_list}}.  Consult the
repository for existing parameter names so that your function will follow a
similar schema (e.g. \code{trainer}, while not strictly required, is a common
parameter, as are any of the \code{arx_args_list()} parameters) these parameters
should be ones that will store well in a data.table; if you need more
complicated parameters, it is better to store them in separate files, and
use the filename as the parameter.}
}
\description{
a minimal forecaster whose median is just the last value
does not support \code{lags} as a parameter, but otherwise has the same parameters as \code{arx_forecaster}
}
