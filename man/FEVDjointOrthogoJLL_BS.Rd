% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NumOutputsBootstrap.R
\name{FEVDjointOrthogoJLL_BS}
\alias{FEVDjointOrthogoJLL_BS}
\title{FEVDs after bootstrap for JLL-based models}
\usage{
FEVDjointOrthogoJLL_BS(
  ModelType,
  ModelParaBoot,
  FEVDhoriz,
  FactorLabels,
  Economies
)
}
\arguments{
\item{ModelType}{string-vector containing the label of the model to be estimated}

\item{ModelParaBoot}{list of model parameter estimates (see the "Optimization" function) after a bootstrap draw}

\item{FEVDhoriz}{single numerical vector conataining the desired horizon of analysis for the FEVDs}

\item{FactorLabels}{string-list based which contains all the labels of all the variables present in the model}

\item{Economies}{string-vector containing the names of the economies which are part of the economic system}
}
\description{
FEVDs after bootstrap for JLL-based models
}
\keyword{internal}
