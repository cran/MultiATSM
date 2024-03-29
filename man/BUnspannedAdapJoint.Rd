% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NumOutputs.R
\name{BUnspannedAdapJoint}
\alias{BUnspannedAdapJoint}
\title{Transform B_spanned into B_unspanned for jointQ models}
\usage{
BUnspannedAdapJoint(G, M, N, C, J, BSpanned)
}
\arguments{
\item{G}{number of global unspanned factors}

\item{M}{number of domestic unspanned factors}

\item{N}{number of domestic spanned factors}

\item{C}{number of economies of the economic system}

\item{J}{number of country-specific observed bond yields}

\item{BSpanned}{B that accomodates only the map to the spanned factors only}
}
\description{
Transform B_spanned into B_unspanned for jointQ models
}
\keyword{internal}
