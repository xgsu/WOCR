#' The objective function (an information criterion) for optimization in WOCR models.
#'
#' @param lambda The tuning parameter(s) to be optimized. It is 1-D in models \code{RR.d.lambda}, \code{RR.gamma.lambda},
#' \code{PCR.d.c}, \code{PCR.gamma.c} and 2-D in models \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.
#' @param d  The vector of singular values of the design matrix \eqn{X}.
#' @param gamma The vector of coefficients, consisting of the inner product of each orthogonal component and the repsonse \eqn{y}.
#' @param U The \eqn{U} matrix whose columns are the orthogonal components.
#' @param y The response vector \eqn{y}.
#' @param model Specifies the WOCR model to be fitted. Six choices are possible: \code{RR.d.lambda},
#' \code{RR.gamma.lambda}, \code{PCR.d.c}, \code{PCR.gamma.c}, \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.
#' @param a The fixed shape parameter \eqn{a} in models \code{PCR.d.c} and \code{PCR.gamma.c} only. By default, \eqn{a} is set to 20.
#' @param criterion Specifies the information criterion to be minimized. The appropriate choice is one from \code{"AIC"}, \code{"BIC"}, or \code{"GCV"}.
#' By default, \code{criterion} is left as \code{NULL} and the recommended one is used.
#'
#' @return The value of the information criterion as an objective function with respect to the tuning parameter \code{lamdba}.
#' @details
#' This function computes the value of the objective function for the tuning parameter \code{lamdba},
#' depending on which WOCR model is used. See Su et al. (2016+) for the recommended choices. Different models have different weighting strategies and
#' different degrees of freedom. This function is not intended for use outside of this package.
#' @seealso \code{\link[WOCR]{WOCR}}, \code{\link[WOCR]{weighting}}
#' @references
#'\itemize{
#' \item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
#' Submitted.
#' }


IC <- function(lambda, d, gamma, U, y, model="RR.d.lambda", a=20, criterion=NULL)
{
  n <- length(y)

  # WEIGHTING FUNCTION
  w <- weighting(lambda=lambda, d=d, gamma=gamma, model=model, a=a)

  # COMPUTE SSE
  # yhat <- as.vector(U%*%(w*gamma)); SSE <- sum((y-yhat)^2)  			########################
  if (is.element(model, c("PCR.d.c", "PCR.gamma.c"))) SSE <- sum(y^2) - sum(w*gamma^2)  # PCR
  else SSE <- sum(y^2)-sum(gamma^2*(1-(1-w)^2))

  # COMPUTE DF
  if (is.element(model, c("RR.d.lambda", "PCR.d.c", "PCR.d.a.c"))) DF <- sum(w)
  else if (model=="RR.gamma.lambda")  DF <- sum((gamma^4 + 3*lambda*gamma^2)/(gamma^2 + lambda)^2)
  else if (model=="PCR.gamma.c") DF <- sum(w*(2*a*gamma^2 + 1-2*a*w*gamma^2))
  else if (model=="PCR.gamma.a.c") DF <- sum(w*(2*lambda[1]*gamma^2 + 1-2*lambda[1]*w*gamma^2))
  else stop("Wrong specification of model= !")

  # THE CRITERION
  if (is.null(criterion)){
    # THE PREFERRED CRITERION FOR EACH MODEL
    if (model=="RR.d.lambda") criterion <- "GCV"
    else if (model=="RR.gamma.lambda") criterion <- "GCV"
    else if (model=="PCR.d.c") criterion <- "BIC"
    else if (model=="PCR.d.a.c") criterion <- "GCV"
    else if (model=="PCR.gamma.c") criterion <- "BIC"
    else if (model=="PCR.gamma.a.c") criterion <- "GCV"
  }

  if (criterion=="GCV") IC <- SSE/(n-DF)^2
  else if (criterion=="AIC") IC <- n*log(SSE) + 2*DF
  else if (criterion=="BIC") IC <- n*log(SSE) + log(n)*DF
  else stop("Wrong specification of criterion= !")

  # RETURN
  return(IC)
}

