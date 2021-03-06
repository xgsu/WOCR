% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighting.R
\name{weighting}
\alias{weighting}
\title{Computing the weights for different WOCR models.}
\usage{
weighting(lambda, d, gamma, model = "RR.d.lambda", a = 20)
}
\arguments{
\item{lambda}{The tuning parameter(s) to be optimized. It is 1-D in models \code{RR.d.lambda}, \code{RR.gamma.lambda},
\code{PCR.d.c}, \code{PCR.gamma.c} and 2-D in models \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.}

\item{d}{The vector of singular values of the design matrix \eqn{X}.}

\item{gamma}{The vector of coefficients, consisting of the inner product of each orthogonal component and the repsonse \eqn{y}.}

\item{model}{Specifies the WOCR model to be fitted. Six choices are possible: \code{RR.d.lambda},
\code{RR.gamma.lambda}, \code{PCR.d.c}, \code{PCR.gamma.c}, \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.}

\item{a}{The fixed shape parameter \eqn{a} in models \code{PCR.d.c} and \code{PCR.gamma.c} only. Its default value is 20.}
}
\value{
The (p-dimensional) vectir of weights for each component.
}
\description{
Computing the weights for different WOCR models.
}
\details{
This function computes the weights for each orthogonal component, depending on which WOCR model is used.
It is intended for internal use within this package only.
}
\references{
\itemize{
\item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
Submitted.
}
}
\seealso{
\code{\link[WOCR]{WOCR}}
}
