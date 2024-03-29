% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InputsForMLEdensity.R
\name{InputsForMLEdensity_BS}
\alias{InputsForMLEdensity_BS}
\title{Generates several  inputs that are necessary to build the likelihood function - Bootstrap version}
\usage{
InputsForMLEdensity_BS(
  ModelType,
  Y_artificial,
  Z_artificial,
  FactorLabels,
  mat,
  Economies,
  DataFrequency,
  JLLinputs = NULL,
  GVARinputs = NULL,
  BRWinputs = NULL
)
}
\arguments{
\item{ModelType}{string-vector containing the label of the model to be estimated}

\item{Y_artificial}{time series of yields (CJ x T or JxT)}

\item{Z_artificial}{time series of the risk factors (F x T)}

\item{FactorLabels}{string-list based which contains the labels of all the variables present in the model}

\item{mat}{vector of maturities (in years) used in the estimation}

\item{Economies}{string-vector containing the names of the economies of the system. \cr
If the ModelType selected is "JPS", "JPS jointP", "GVAR sepQ", then only one economy can be selected. \cr
 For the other models, more than one economy must be selected.}

\item{DataFrequency}{character-based-vector. Avaialable options are: "Daily All Days", "Daily Business Days", "Weekly", "Monthly", "Quarterly", "Annually"}

\item{JLLinputs}{list of necessary inputs for the estimation of JLL-based models (see "JLL" function)}

\item{GVARinputs}{list of necessary inputs for the estimation of GVAR-based models (see "GVAR" function)}

\item{BRWinputs}{list of necessary inputs for performing the bias-corrected estimation (see "Bias_Correc_VAR" function)}
}
\description{
Generates several  inputs that are necessary to build the likelihood function - Bootstrap version
}
\keyword{internal}
