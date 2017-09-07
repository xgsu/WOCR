#' The Generic \code{plot} Function for Object of \code{WOCR} Class
#'
#' @name plot.WOCR
#' @aliases print.wocr
#' @param fit an object of \code{WOCR} class.
#' @param mar margin in terms of the number of lines to be specified on the four sides of the plot
#' @param horizontal  Logical indicator of horizontal alignment. Default is \code{TRUE}.
#' @param \code{...} further arguments passed to or from other methods.
#' @details
#' The (generic) plot method for an \code{WOCR} object. It generates three plots of \eqn{d}, \eqn{gamma}, and weight.
#' @return Plots of the singular values \eqn{d}, the coefficients \eqn{gamma} for components, and the
#' estimated weigt for each components, versus the components.
#' @references
#'\itemize{
#' \item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
#' Submitted.
#' }
#' @seealso \code{\link[WOCR]{WOCR}}
#' @export

plot.WOCR <- function(fit, horizontal=TRUE, mar=rep(4, 4), ...)
{
  if (horizontal) par(mfrow=c(1, 3), mar=mar)
  else par(mfrow=c(3, 1), mar=mar)
  # (a) SINGULAR VALUE
  d <- fit$d
  plot(1:length(d), d, type="p", pch=1, cex=0.8, col="blue", xlab="components",
       ylab="d", main="(a)")
  segments(x0=1:length(d), y0=0, x1=1:length(d), y1=d, col="gray35")

  # (b) ABSOLUTE VALUES OF GAMMA
  gamma <- abs(fit$gamma)
  plot(1:length(gamma), gamma, type="p", pch=1, cex=0.8, col="red", xlab="components",
       ylab=expression(paste("|", gamma, "|")), main="(b)")
  segments(x0=1:length(gamma), y0=0, x1=1:length(gamma), y1=gamma, col="gray35")

  # (c) WEIGHTS
  w.hat <- fit$w.hat
  plot(1:length(w.hat), w.hat, type="p", pch=1, cex=0.8, col="green", xlab="components",
       ylab="weight", main="(c)")
  segments(x0=1:length(w.hat), y0=0, x1=1:length(w.hat), y1=w.hat, col="gray35")
}
