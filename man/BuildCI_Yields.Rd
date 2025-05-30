% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConfidenceBoundsBootstrap.R
\name{BuildCI_Yields}
\alias{BuildCI_Yields}
\title{Build Confidence intervals for yield-related outputs}
\usage{
BuildCI_Yields(
  NumOutBounds,
  NumOutPE,
  Lab_Int,
  ModelType,
  Economies,
  IdxResp,
  IdxShock,
  Ortho = FALSE
)
}
\arguments{
\item{NumOutBounds}{numerical output set from the bootstrap analysis}

\item{NumOutPE}{numerical output set from the point estimate analysis}

\item{Lab_Int}{Label of interest. available options are "IRF" and "FEVD"}

\item{ModelType}{desired model type}

\item{Economies}{name of the economies forming the economic system}

\item{IdxResp}{index associated with the response variable}

\item{IdxShock}{index associated with the shock variable}

\item{Ortho}{Option for orthogonal outputs, for JLL models. Default is FALSE.}
}
\description{
Build Confidence intervals for yield-related outputs
}
\keyword{internal}
