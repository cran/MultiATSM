% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GraphicalOutputs.R
\name{TPDecompGraph}
\alias{TPDecompGraph}
\title{Term Premia decomposition graphs for all models}
\usage{
TPDecompGraph(
  ModelType,
  NumOut,
  ModelPara,
  WishRPgraphs,
  UnitYields,
  Economies,
  PathsGraphs
)
}
\arguments{
\item{ModelType}{a string-vector containing the label of the model to be estimated}

\item{NumOut}{list of computed outputs containing the model fit, IRFs, FEVDs, GIRFs, GFEVDs and Risk premia}

\item{ModelPara}{list of model parameter estimates (See the "Optimization" function)}

\item{WishRPgraphs}{binary variable: set 1, if the user wishes graphs to be generated; or set 0, otherwise}

\item{UnitYields}{(i) "Month": if maturity of yields are expressed in months or
(ii) "Year": if maturity of yields are expressed in years}

\item{Economies}{a string-vector containing the names of the economies which are part of the economic system}

\item{PathsGraphs}{Path of the folder in which the graphs will be saved}
}
\description{
Term Premia decomposition graphs for all models
}
\keyword{internal}
