#' The Generic \code{predict} Function for Object of \code{WOCR} Class
#'
#' @name predict.WOCR
#' @aliases predict.wocr
#' @param fit an object of \code{WOCR} class.
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted, the fitted values
#' (based on the original WOCR fit) are returned.
#' @return The predicted values for observations in the new data.
#' @references
#'\itemize{
#' \item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
#' Submitted.
#' }
#' @seealso \code{\link[WOCR]{WOCR}}
#' @examples
#'    data(BostonHousing1)
#'    fit.WOCR <- WOCR(formula=cmedv~., data=BostonHousing1, model="RR.d.lambda")
#'    # THE SAME DATA IS USED FOR PREDICTION HERE JUST FOR ILLUSTRATION
#'    par(mfrow=c(1, 1), mar=rep(4, 4))
#'    y.pred <- predict(fit.WOCR, newdata=BostonHousing1)
#'    plot(BostonHousing1$cmedv, y.pred, pch=19, cex=.6, col="cadetblue3", xlab="observed", ylab="predicted")
#'    abline(a=0, b=1, col="chocolate1", lwd=1.8)
#'
#' @export

predict.WOCR <- function(fit, newdata=NULL){
  if (is.null(newdata)) y.pred <- fit$y.hat
  else {
    # OBTAIN THE DESIGN MATRIX X AND RESPONSE y
    formula <- fit$formula
    X <- model.matrix(object=fit$formula, data = newdata)
    if (colnames(X)[1]=="(Intercept)") X <- X[, -1]		# REMOVE INTERCEPT
    # SCALE DATA
    X <- scale(X, center = fit$mean.x, scale=fit$sd.x)
    y.pred <- as.vector(X%*%fit$beta.hat) + fit$mean.y
  }
  return(y.pred)
}
