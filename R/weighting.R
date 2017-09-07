#' Computing the weights for different WOCR models.
#'
#' @param lambda The tuning parameter(s) to be optimized. It is 1-D in models \code{RR.d.lambda}, \code{RR.gamma.lambda},
#' \code{PCR.d.c}, \code{PCR.gamma.c} and 2-D in models \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.
#' @param d  The vector of singular values of the design matrix \eqn{X}.
#' @param gamma The vector of coefficients, consisting of the inner product of each orthogonal component and the repsonse \eqn{y}.
#' @param model Specifies the WOCR model to be fitted. Six choices are possible: \code{RR.d.lambda},
#' \code{RR.gamma.lambda}, \code{PCR.d.c}, \code{PCR.gamma.c}, \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.
#' @param a The fixed shape parameter \eqn{a} in models \code{PCR.d.c} and \code{PCR.gamma.c} only. Its default value is 20.
#'
#' @return The (p-dimensional) vectir of weights for each component.
#' @details
#' This function computes the weights for each orthogonal component, depending on which WOCR model is used.
#' It is intended for internal use within this package only.
#' @seealso \code{\link[WOCR]{WOCR}}
#' @references
#'\itemize{
#' \item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
#' Submitted.
#' }


weighting <- function(lambda, d, gamma, model="RR.d.lambda", a=20)
{
  # WEIGHTING FUNCTION
  if (model=="RR.d.lambda") w <- d^2/(d^2+lambda)
  else if (model=="RR.gamma.lambda") w <- gamma^2/(gamma^2+lambda)
  else if (model=="PCR.d.c") w <- expit(a*(d-lambda))
  else if (model=="PCR.d.a.c") w <- expit(lambda[1]*(d-lambda[2]))
  else if (model=="PCR.gamma.c") w <- expit(a*(gamma^2-lambda))
  else if (model=="PCR.gamma.a.c") w <- expit(lambda[1]*(gamma^2-lambda[2]))
  else stop("Wrong specification of the model= argument for WOCR!")
  return(w)
}
