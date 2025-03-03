% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLEdensity.R
\name{mult_logabsdet}
\alias{mult_logabsdet}
\title{Inverse each 2D slice of an array (M) with arbitrary dimensions support}
\usage{
mult_logabsdet(M)
}
\arguments{
\item{M}{multi-dimensional array}
}
\value{
X  : (n-2)_D array ([p x q x  ...])
}
\description{
Inverse each 2D slice of an array (M) with arbitrary dimensions support
}
\details{
Inverse every 2D slice (the first two dimensions of M) for multi-dimension array M. In Matlab language:
  M[ , , p,q,...] * X[ , ,p,q,...] = repmat(diag(m),[1,1,p,q,...])
}
\references{
This function is based on the "mult_logabsdet" function by Le and Singleton (2018). \cr
 "A Small Package of Matlab Routines for the Estimation of Some Term Structure Models." \cr
 (Euro Area Business Cycle Network Training School - Term Structure Modelling).
 Available at: https://cepr.org/40029
}
\keyword{internal}
