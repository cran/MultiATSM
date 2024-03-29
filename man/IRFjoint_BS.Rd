% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NumOutputsBootstrap.R
\name{IRFjoint_BS}
\alias{IRFjoint_BS}
\title{IRFs after bootstrap for "joint Q" models}
\usage{
IRFjoint_BS(ModelType, ModelParaBoot, IRFhoriz, FactorLabels, Economies)
}
\arguments{
\item{ModelType}{string-vector containing the label of the model to be estimated}

\item{ModelParaBoot}{list of model parameter estimates (see the "Optimization" function) after a bootstrap draw}

\item{IRFhoriz}{single numerical vector conataining the desired horizon of analysis for the IRFs}

\item{FactorLabels}{a string-list based which contains all the labels of all the variables present in the model}

\item{Economies}{a string-vector containing the names of the economies which are part of the economic system}
}
\description{
IRFs after bootstrap for "joint Q" models
}
\keyword{internal}
