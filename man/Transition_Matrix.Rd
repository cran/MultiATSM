% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transition_Matrix.R
\name{Transition_Matrix}
\alias{Transition_Matrix}
\title{Compute the transition matrix required in the estimation of the GVAR model}
\usage{
Transition_Matrix(
  t_First,
  t_Last,
  Economies,
  type,
  DataPath = NULL,
  Data = NULL
)
}
\arguments{
\item{t_First}{Sample starting date (year)}

\item{t_Last}{Sample last date (year)}

\item{Economies}{Vector containing the names of all the economies of the system.}

\item{type}{Three possibilities:
\itemize{
     \item "Full Sample": if one wishes ALL weight matrices of each year from which data is available (it may extrapolate the sample period);
     \item "Sample Mean": if one wishes a SINGLE weight matrix containing the average of weights over of the entire sample period;
     \item Some year in particular (e.g. "1998", "2005" ...).
}}

\item{DataPath}{path of the Excel file containing the data (if any). The default is linked to the Excel file available in the package.}

\item{Data}{Data for computing the transition matrix. Default is set to NULL.}
}
\value{
matrix or list of matrices
}
\description{
Compute the transition matrix required in the estimation of the GVAR model
}
\details{
NOTE: if there is missing data for any country of the system for that particularly year,
then the transition matrix will include only NAs.
}
\examples{
data(CM_Trade)

t_First <- "2006"
t_Last <-  "2019"
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
type <- "Sample Mean"
Transition_Matrix(t_First, t_Last, Economies, type, DataPath = NULL, Data = TradeFlows)


}
