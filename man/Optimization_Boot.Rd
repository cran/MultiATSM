% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Optimization.R
\name{Optimization_Boot}
\alias{Optimization_Boot}
\title{Peform the minimization of mean(f) (adapted for the bootstrap setting)}
\usage{
Optimization_Boot(
  f,
  tol,
  varargin,
  FactorLabels,
  Economies,
  ModelType,
  JLLinputs = NULL,
  GVARinputs = NULL
)
}
\arguments{
\item{f}{vector-valued objective function (function)}

\item{tol}{convergence tolerance (scalar). For ML estimation, a reasonable value is tol <- 1e-4}

\item{varargin}{list containg starting values and constraints:
                       for each input argument K (of f), we need four inputs that look like:
   \enumerate{
       \item a starting value: K0
       \item a variable label ('K0') followed by a ':' followed by a type of constraint. The constraint can be:
                 \itemize{
                     \item 'bounded': bounded matrix;
                     \item 'Jordan' or 'Jordan MultiCountry': a matrix of Jordan type;
                     \item 'psd': psd matrix;
                     \item 'stationary': largest eigenvalue of the risk-neutral feedback matrix is strictly smaller than 1;
                     \item 'diag' or 'BlockDiag': a diagonal or block diagonal matrix.
                     \item 'JLLstructure': to impose the zero-restrictions on the variance-voriance matrix along
                             the lines of the JLL models
                         }
       \item a lower bound lb (lb <- NULL -> no lower bound)
       \item an upper bound ub (ub <- NULL -> no upper bound)
       \item Specification of the optimization settings:
       \itemize{
                 \item 'iter off': hide the printouts of the numerical optimization routines;
                 \item 'fminunc only': only uses fminunc for the optimization;
                 \item ''fminsearch only': only uses fminsearch for the optimization.

  }
  }}

\item{FactorLabels}{string-list based which contains the labels of all the variables present in the model}

\item{Economies}{string-vector containing the names of the economies which are part of the economic system}

\item{ModelType}{string-vector containing the label of the model to be estimated}

\item{JLLinputs}{inputs used in the estimation of the JLL-based models; Default is set to NULL}

\item{GVARinputs}{inputs used in the estimation of the GVAR-based models; Default is set to NULL}
}
\value{
(i) out: list of second output produced by f (the first output of f must be the objective value to be minimized)\cr
(ii) x:  list containing parameter estimates
}
\description{
Peform the minimization of mean(f) (adapted for the bootstrap setting)
}
\details{
If a variable name starts with a '@', it means that that parameter will be analytically concentrated out in
the specification of f. In this case, no starting value is needed for this particular parameter.An  empty matrix
can be provided as a starting value
}
\references{
This function is based on the "LS__opt" function by Le and Singleton (2018).\cr
 "A Small Package of Matlab Routines for the Estimation of Some Term Structure Models." \cr
 (Euro Area Business Cycle Network Training School - Term Structure Modelling).
 Available at: https://cepr.org/40029
}
\keyword{internal}
