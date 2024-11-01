% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elpu2.R
\name{elpu2}
\alias{elpu2}
\title{Calculate the empirical likelihood under the assumption of DET-SCAR model}
\usage{
elpu2(dat, param, maxiter = 500, tol = 1e-04, updatePiVal = TRUE, flip = TRUE)
}
\arguments{
\item{dat}{a data matrix that contains all the observations}

\item{param}{a parameter vector that contains alp1, alp2, beta1, beta2 and piVal}

\item{maxiter}{the maximum iteration number}

\item{tol}{the tolerance of the changing in likelihood}

\item{updatePiVal}{whether or not to update piVal}

\item{flip}{whether to flip the pi (proportion) or not}
}
\value{
a list of values: piVal, alp1, alp2, beta1, beta2, llk and iteration number
}
\description{
Calculate the empirical likelihood under the assumption of DET-SCAR model
}
