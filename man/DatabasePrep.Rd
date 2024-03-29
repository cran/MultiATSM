% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatabasePrep.R
\name{DatabasePrep}
\alias{DatabasePrep}
\title{Prepare the GVARFactors database}
\usage{
DatabasePrep(
  t_First,
  t_Last,
  Economies,
  N,
  FactorLabels,
  ModelType,
  Wgvar = NULL,
  DataPathMacro = NULL,
  DataPathYields = NULL
)
}
\arguments{
\item{t_First}{sample starting date (yyyy-mm-dd)}

\item{t_Last}{sample last date (yyyy-mm-dd)}

\item{Economies}{string-vector containing the names of the economies which are part of the economic system}

\item{N}{number of country-specific spanned factor (scalar)}

\item{FactorLabels}{list containing the factor labels}

\item{ModelType}{string-vector containing the label of the model to be estimated}

\item{Wgvar}{GVAR transition matrix (CxC), if GVAR type model is chosen; default is set to NULL.}

\item{DataPathMacro}{path of the Excel file containing the macroeconomic data (if any). The default is linked to the Excel file available in the package.}

\item{DataPathYields}{path of the Excel file containing the yields data (if any). The default is linked to the Excel file available in the package.}
}
\value{
List of the risk factor set used in the estimation of the GVAR model

List containing the risk factor set used in the estimation of the GVAR-based models
}
\description{
Prepare the GVARFactors database
}
\examples{
DomVar <- c("Eco_Act", "Inflation")
GlobalVar <- c("GBC", "CPI_OECD")
t0 <- "2006-09-01"
tF <-  "2019-01-01"
Economies <- c("China", "Brazil", "Mexico", "Uruguay", "Russia")
N <- 3
ModelType <- "JPS jointQ"
FactorLabels <-  LabFac(N, DomVar, GlobalVar, Economies, ModelType)

GVARFactors <- DatabasePrep(t0, tF, Economies, N, FactorLabels, ModelType)


}
