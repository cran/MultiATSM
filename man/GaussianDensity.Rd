% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLEdensity.R
\name{GaussianDensity}
\alias{GaussianDensity}
\title{computes the density function of a gaussian process}
\usage{
GaussianDensity(res, SS, invSS, logabsdetSS)
}
\arguments{
\item{res}{matrix of residuals (N x T)}

\item{SS}{covariance matrice or array of covariance matrices (N x N) or (N x N x T)}

\item{invSS}{Inverse of SS (N x N) or (N x N x T) - optional input}

\item{logabsdetSS}{log(abs(|SS|)) (1 x T) - optional input}
}
\value{
y vector of density (1 x T)
}
\description{
computes the density function of a gaussian process
}
\references{
This function is based on the "Gaussian" function by Le and Singleton (2018).\cr
 "A Small Package of Matlab Routines for the Estimation of Some Term Structure Models." \cr
 (Euro Area Business Cycle Network Training School - Term Structure Modelling).
 Available at: https://cepr.org/40029
}
\keyword{internal}
