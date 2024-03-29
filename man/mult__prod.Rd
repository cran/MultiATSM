% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mult_Set_Functions.R
\name{mult__prod}
\alias{mult__prod}
\title{Efficient computation of matrix product for arrays}
\usage{
mult__prod(a, b, c)
}
\arguments{
\item{a}{array}

\item{b}{array}

\item{c}{array}
}
\description{
Efficient computation of matrix product for arrays
}
\details{
#' Efficiently computes matrix product for arrays a and b and c:
 d[,,i] = a[,,i] b[,,i] c[,,i].
(efficiently = without using an inefficient loop)
}
\references{
This function is modified version of the "mult__prod" function by Le and Singleton (2018). \cr
 "A Small Package of Matlab Routines for the Estimation of Some Term Structure Models."\cr
 (Euro Area Business Cycle Network Training School - Term Structure Modelling).
 Available at: https://cepr.org/40029
}
\keyword{internal}
