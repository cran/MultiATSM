% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bootstrap.R
\name{BuildYields_BS}
\alias{BuildYields_BS}
\title{Build the time-series of bond yields for each bootstrap draw}
\usage{
BuildYields_BS(
  ModelParaPE,
  ModelType,
  RiskFactors_BS,
  BFull,
  BS_Set,
  Economies
)
}
\arguments{
\item{ModelParaPE}{List of point estimates of the model parameter}

\item{ModelType}{String-vector containing the label of the model to be estimated}

\item{RiskFactors_BS}{Time-series of the risk factors (F x T)}

\item{BFull}{B matrix of loadings}

\item{BS_Set}{Set of bootstrap inputs}

\item{Economies}{String-vector containing the names of the economies which are part of the economic system}
}
\description{
Build the time-series of bond yields for each bootstrap draw
}
\keyword{internal}
