% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GraphicalOutputs.R
\name{GraphicalOutputs}
\alias{GraphicalOutputs}
\title{Generate the graphical outputs for the selected models (Point estimate)}
\usage{
GraphicalOutputs(
  ModelType,
  ModelPara,
  NumOut,
  InputsForOutputs,
  Economies,
  FactorLabels
)
}
\arguments{
\item{ModelType}{A character vector indicating the model type to be estimated.}

\item{ModelPara}{List of model parameter estimates (See the "Optimization" function)}

\item{NumOut}{list of computed outputs containing the model fit, IRFs, FEVDs, GIRFs, GFEVDs and Term Premia}

\item{InputsForOutputs}{list containing the desired inputs for the construction of the desired output}

\item{Economies}{A character vector containing the names of the economies included in the system.}

\item{FactorLabels}{A list of character vectors with labels for all variables in the model.}
}
\description{
Generate the graphical outputs for the selected models (Point estimate)
}
\keyword{internal}
