% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca_weights_one_country.R
\name{pca_weights_one_country}
\alias{pca_weights_one_country}
\title{Weigth matrix from principal components (matrix of eigenvectors)}
\usage{
pca_weights_one_country(Y, Economy)
}
\arguments{
\item{Y}{matrix dimension (J x T), where  J - the number of maturities and  T - time series length}

\item{Economy}{string-vector containg the name of one economy}
}
\value{
matrix (J x J)
}
\description{
Weigth matrix from principal components (matrix of eigenvectors)
}
\examples{
data("CM_Yields")
pca_weights_one_country(Yields, Economy= "Brazil")
}
