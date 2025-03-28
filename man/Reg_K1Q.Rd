% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Reg_K1Q.R
\name{Reg_K1Q}
\alias{Reg_K1Q}
\title{Estimate the risk-neutral feedbak matrix K1Q using linear regressions}
\usage{
Reg_K1Q(Y, mat, Z, dt, type)
}
\arguments{
\item{Y}{matrix of yields used in estimation  (J x T)}

\item{mat}{vector of maturities (in years) of yields used in estimation (J x 1)}

\item{Z}{pricing factors (can be yields-based or non-yields/macro variables) (N x T)}

\item{dt}{time unit of the model (scalar). For instance, if data is (i) monthly, dt <- 12; (ii) quarterly, dt <- 4; (iii) yearly, dt <- 1.}

\item{type}{'Jordan' -> K1Q will be of the Jordan type \cr}
}
\value{
Risk neutral feedback matrix K1Q.
}
\description{
Estimate the risk-neutral feedbak matrix K1Q using linear regressions
}
\references{
This function is modified version of the "Reg_K1Q" function by Le and Singleton (2018). \cr
 "A Small Package of Matlab Routines for the Estimation of Some Term Structure Models." \cr
 (Euro Area Business Cycle Network Training School - Term Structure Modelling).
 Available at: https://cepr.org/40029
}
\keyword{internal}
