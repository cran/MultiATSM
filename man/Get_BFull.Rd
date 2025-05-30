% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bootstrap.R
\name{Get_BFull}
\alias{Get_BFull}
\title{Compute the B matrix of loadings}
\usage{
Get_BFull(ModelParaPE, FactorLabels, mat, Economies, ModelType)
}
\arguments{
\item{ModelParaPE}{List of point estimates of the model parameter}

\item{FactorLabels}{String-list based which contains the labels of all the variables present in the model}

\item{mat}{Vector of bond yield maturities}

\item{Economies}{String-vector containing the names of the economies which are part of the economic system}

\item{ModelType}{A character vector indicating the model type to be estimated}
}
\description{
Compute the B matrix of loadings
}
\keyword{internal}
