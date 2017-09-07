
#' The expit or logistic function
#'
#' @param x A numerical scalar or vector.
#' @return The value or vector of expit(x).
#' @details
#' This function is written on the basis of the built-in hyperbolic tangent function \code{tanh}.
#' @seealso \code{\link[base]{tanh}}
#' @examples
#' x <- 1:3
#' expit(x)
#' @export


expit <- function(x) (tanh(x/2)+1)/2
