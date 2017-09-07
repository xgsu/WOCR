#' The Generic \code{print} Function for Object of \code{WOCR} Class
#'
#' @name print.WOCR
#' @aliases print.wocr
#' @param fit an object of \code{WOCR} class.
#' @param digits	the minimal number of significant digits. See \code{\link[base]{print.default}}.
#' @param \code{...} further arguments passed to or from other methods.
#' @details
#' The (generic) print method for an \code{WOCR} object. The results include \code{d} (the singular value of standardized
#' \eqn{X}), \code{gamma} (the coefficients for the orthogonal components), \code{w.hat} (the estimated weights),
#' \code{beta.hat} (the estimated coefficients for the standardized \eqn{X} variables).
#'
#' @return A table with four columns (\code{d},  \code{gamma}, \code{w.hat}, \code{beta.hat}).
#' @references
#'\itemize{
#' \item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
#' Submitted.
#' }
#' @seealso \code{\link[WOCR]{WOCR}}
#' @export

print.WOCR <- function(fit, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nCall:\n", paste(deparse(fit$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  result <- data.frame(d=fit$d, gamma=fit$gamma, w.hat=fit$w.hat)
  print(result, digits=digits)
  cat("\n* Note that d is the singular values of scaled X; ")
  cat("\n gamma is the coefficent for each orthogonal component; ")
  cat("\n w.hat is the weight assigned to each orthogonal component; \n ")
  cat("\nThe coefficents for (scaled) X variable are \n\n")
  print(fit$beta.hat)
  cat("\n")
  invisible(fit)
}
