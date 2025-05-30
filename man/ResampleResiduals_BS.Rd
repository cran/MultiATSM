% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bootstrap.R
\name{ResampleResiduals_BS}
\alias{ResampleResiduals_BS}
\title{Compute the residuals from the original model}
\usage{
ResampleResiduals_BS(
  residPdynOriginal,
  residYieOriginal,
  InputsForOutputs,
  ModelType,
  nlag = 1
)
}
\arguments{
\item{residPdynOriginal}{Time-series of the residuals from the P-dynamics equation (T x F)}

\item{residYieOriginal}{Time-series of the residuals from the observational equation (T x J or T x CJ)}

\item{InputsForOutputs}{List containing the desired inputs for the construction of the numerical outputs.}

\item{ModelType}{A character vector indicating the model type to be estimated}

\item{nlag}{Number of lags in the P-dynamics. Default is set to 1.}
}
\description{
Compute the residuals from the original model
}
\keyword{internal}
