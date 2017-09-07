#' Fitting the weighted orthoginal components regression (WOCR) models
#'
#' @name WOCR
#' @aliases wocr
#' @param formula An object of class \code{\link[stats]{formula}}, with the response on the left of
#' a \code{~} operator, and the terms on the right. The function assumes the intercept to be zero with scaled data.
#' If the interacept \code{~ +1} is included explicitly or by default, it will  be automatically removed.
#' @param data A data.frame in which to interpret the variables named in the \code{formula} argument.
#' @param scale Logical indicator of whether you want to scale the data. Here, \code{scale=TRUE} should always be done unless the
#' data have already scaled or standardized. This action involves standaradizing the columns in matrix \eqn{X}  and center the
#' response vector \eqn{y}. Note that this option also has an effect on the resultant prediction.
#' @param model Specifies the WOCR model to be fitted. Six choices are possible: \code{RR.d.lambda},
#' \code{RR.gamma.lambda}, \code{PCR.d.c}, \code{PCR.gamma.c}, \code{PCR.d.a.c} and \code{PCR.gamma.a.c}.
#' @param a The fixed shape parameter \eqn{a} in models \code{PCR.d.c} and \code{PCR.gamma.c} only. By default, \eqn{a} is set to 20.
#' @param criterion Specifies the information criterion to be minimized. The appropriate choice is one from \code{"AIC"}, \code{"BIC"}, or \code{"GCV"}.
#' By default, \code{criterion} is left as \code{NULL} and the recommended one is used.
#' @param use.GenSA Logical value indicating if the generalized simulated annealing \code{GenSA} is used for optimizing the
#' 2-D optimization problems in models \code{PCR.d.a.c} and \code{PCR.gamma.a.c}. The default is \code{TRUE}.
#' @param maxit.global  Maximum number of iterations allowed for the global optimization algorithm, which is either \code{GenSA} or
#' \code{SANN} in \code{\link[stats]{optim}}. Default value is 100.
#' @param epsilon Tolerance level for convergence. Default is 1e-6.
#' @param maxit.local Maximum number of iterations allowed for the local optimizaiton algorithm \code{BFGS}. Default value is 100.
#' @param LB The lower bounds for the search space in \code{GenSA} The default is \code{NULL} and recommended one is used.
#' @param UB The upper bounds for the search space in \code{GenSA} The default is \code{NULL} and recommended one is used.
#' @param details Logical value: if \code{TRUE}, detailed results will be printed out when running \code{coxphMIC}.
#' @details
#' The main idea of WOCR is to parameterize the weights for orthogonal components with a simple function whose specification is up to one or two
#' tuning parameters. The weighting strategy takes advantage of the inherent monotonicity among these orthogonal components.
#'
#' To solve the 2-D smooth yet nonconvex optimization, two options are made available. The first is a simulated annealing (\code{method="SANN"} option
#' in \code{\link[stats]{optim}}) global optimization algorithm is first applied. The resultant estimator is then used
#' as the starting point for another local optimization algorithm, where the quasi-Newton BFGS method (\code{method="BFGS"}
#' in \code{\link{optim}}) by default. Optionally, the generalized simulated annealing, implemented in \code{\link[GenSA]{GenSA}},
#' can be used instead. This latter approach tends to be slower. However, it does not need to be combined with another local optimization;
#' besides, it often yields the same final solution with different runs. Thus, when \code{use.GenSA=TRUE},
#' the output includes \code{opt.global} only, without \code{opt.local}.
#'
#' @return An object of class \code{WOCR} is returned, which may contain the following components depending on the options.
#' \describe{
#' \item{call}{the matched call.}
#' \item{opt.global}{Results from the preliminary run of a global optimization procedure (\code{SANN} as default or \code{GenSA}).}
#' \item{opt}{Results from the final optimization procedure. This could be the results from the 1-D optimization \code{\link[stats]{optimize}} or from the second run of the local optimizaiton \code{BFGS}.}
#' \item{m}{The rank of the design matrix \eqn{X}.}
#' \item{d}{The vector of singular values.}
#' \item{gamma}{The vector of coefficients, consisting of the inner product of each orthogonal component and the repsonse \eqn{y}.}
#' \item{formula}{The \code{formula} used in fitting WOCR. A copy of \code{formula} will be useful for prediction later on.}
#' \item{mean.y}{Mean value of the response \eqn{y}.}
#' \item{mean.x}{Mean vector of columns of matrix \eqn{X}.}
#' \item{sd.x}{Vector of standard deviations for each \eqn{X} variables or columns.}
#' \item{min.Q}{Value of the minimized objective function.}
#' \item{lambda.hat}{The 1-D or 2-D estimated tuning parameter(s).}
#' \item{beta.hat}{The coefficients for the \emph{standardized} \eqn{X} variables, instead of the orthogonal components.}
#' \item{w.hat}{The estimated weights.}
#' \item{n.comp}{The selected number of components for WOCR models \code{PCR.d.c} and \code{PCR.gamma.c} only.}
#' \item{y.hat}{The fitted value for the response. The mean response is added back. So \code{y.hat} can be compared directly to the response \eqn{y}.}
#' }
#' @examples
#'    data(BostonHousing1)
#'    fit.wocr <- WOCR(formula=cmedv~., data=BostonHousing1, model="RR.d.lambda")
#'    print(fit.wocr)
#'    plot(fit.wocr, horizontal=TRUE)
#'
#' @seealso \code{\link[pls]{pcr}}
#' @references
#'\itemize{
#' \item Su, X., Wonkye, Y., Wang, P., and Yin, X. (2016+). Weighted orthogonal component regression (WOCR).
#' Submitted.
#' }
#' @import GenSA
#' @import numDeriv
#' @import pls
#' @export


WOCR <- function(formula, data=NULL, scale=TRUE,
                 model="RR.d.lambda", a=20, criterion=NULL, 		# Five different models are available. Setting a=30 for approximation; with criterion=NULL, the recommended criterion will be used instead.
                 use.GenSA=TRUE, maxit.global=100, eps=1e-6,
                 LB=NULL, UB=NULL, details=FALSE)		# LB and UB are bounds to specify the search space for the optimization procedure.
{
  call <- match.call()
  # CHECK THE data= ARGUMENT
  if (missing(data)) data <- environment(formula)

  # OBTAIN THE DESIGN MATRIX X AND RESPONSE y
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.response(mf, "any")
  # print("y"); print(y)
  out <- as.list(NULL)
  out$call <- call

  X <- if (!is.empty.model(mt)) model.matrix(mt, mf)
  else matrix(, NROW(Y), 0L)
  if (colnames(X)[1]=="(Intercept)") X <- X[, -1]		# REMOVE INTERCEPT
  Xnames <- colnames(X)
  if (details) print(head(X))  ################

  # STANDARDIZE X AND CENTER y; ONE SHOULD ALWAYS DO SO UNLESS THIS IS DONE ALREADY.
  if (scale) {
    mean.y <- mean(y)						# OUTPUT
    y <- scale(y, center = TRUE, scale= FALSE)
    X <- scale(X, center=TRUE, scale=TRUE)
    mean.x <- attributes(X)$'scaled:center' 		# OUTPUT
    sd.x <- attributes(X)$`scaled:scale`  		# OUTPUT
  }

  # OBTAIN THE PRINCIPAL COMPONENTS VIA SVD
  fit.svd <- svd(X)
  U <- fit.svd$u; V <- fit.svd$v; d <- fit.svd$d;
  m <-  sum(d > 0)
  # m <- sum(d > eps)  # USE A THRESHOLD eps; NOTE rank(X)=m
  if (details) print(cbind(p=NCOL(X), m=m))
  U <- U[, 1:m]; d <- d[1:m]; V <- V[, 1:m]
  # print(dim(U)); print(dim(y))
  gamma <- as.vector(t(U)%*%y)
  # print("gamma"); if (details) print(gamma)  ################
  # print("d"); print(d)

  # ESTIMATE THE TUNING PARAMETERS
  if (model=="RR.d.lambda"){
    L0 <- 0; U0 <- max(max(d^2), 1000)
    # L0 <- min(d^2); U0 <- max(d^2)
    opt.fit <- optimize(f=IC, maximum = FALSE, lower=L0, upper=U0,
                        d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)  # ONE-DIMENSIONAL
    lambda.hat <- opt.fit$minimum; min.Q <- opt.fit$objective
  } else if (model=="PCR.d.c"){
    L0 <- min(d); U0 <- max(d)
    opt.fit <- optimize(f=IC, maximum = FALSE, lower=L0, upper=U0,
                        d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)  # ONE-DIMENSIONAL
    lambda.hat <- opt.fit$minimum; min.Q <- opt.fit$objective
  } else if (model=="RR.gamma.lambda"){
    L0 <- 0; U0 <- max(max(gamma^2), 1000)
    # L0 <- min(gamma^2); U0 <- max(gamma^2)
    opt.fit <- optimize(f=IC, maximum = FALSE, lower=L0, upper=U0,
                        d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)  # ONE-DIMENSIONAL
    lambda.hat <- opt.fit$minimum; min.Q <- opt.fit$objective
  } else if (model=="PCR.gamma.c") {
    L0 <- min(gamma^2); U0 <- max(gamma^2)
    opt.fit <- optimize(f=IC, maximum = FALSE, lower=L0, upper=U0,
                        d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)  # ONE-DIMENSIONAL
    lambda.hat <- opt.fit$minimum; min.Q <- opt.fit$objective
  } else if (model=="PCR.d.a.c") {
    lambda0 <- c(10, 0.5)
    if (use.GenSA) {
      # GenSA MINIMIZES  - lambda[1]=a; lambda[2]=c
      if (is.null(LB)) L0 <- c(0.001, min(d)) else L0 <- LB
      if (is.null(UB)) U0 <- c(200, max(d)) else U0 <- UB
      opt.fit <- GenSA(par=lambda0, fn=IC, lower = L0, upper = U0,
                       control=list(maxit=maxit.global, nb.stop.improvement=5),
                       d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)  # TWO-DIMENSIONAL
    } else {
      # SA FOLLOWED BY BFGS
      opt.fit1 <- optim(par=lambda0, fn=IC,
                        method = "SANN", control = list(maxit=maxit.global, trace=F, reltol=eps),
                        d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)	# TWO-DIMENSIONAL
      out$opt.global <- opt.fit1
      lambda1 <- opt.fit1$par; #
      opt.fit <- optim(par=lambda1, fn=IC,
                       method = "BFGS", control = list(maxit=maxit.local, trace=F, reltol=eps),
                       d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)
    }
    lambda.hat <- opt.fit$par; min.Q <- opt.fit$value
  } else if (model=="PCR.gamma.a.c") {
    lambda0 <- c(10, 0.5)
    if (use.GenSA) {
      # GenSA MINIMIZES  - lambda[1]=a; lambda[2]=c
      if (is.null(LB)) L0 <- c(0.001, min(gamma^2)) else L0 <- LB
      if (is.null(UB)) U0 <- c(200, max(gamma^2)) else U0 <- UB
      opt.fit <- GenSA(par=lambda0, fn=IC, lower = L0, upper = U0,
                       control=list(maxit=maxit.global, nb.stop.improvement=5),
                       d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)  	# TWO-DIMENSIONAL
    } else {
      # SA FOLLOWED BY BFGS
      opt.fit1 <- optim(par=lambda0, fn=IC,
                        method = "SANN", control = list(maxit=maxit.global, trace=F, reltol=eps),
                        d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)	# TWO-DIMENSIONAL
      out$opt.global <- opt.fit1
      lambda1 <- opt.fit1$par; #
      opt.fit <- optim(par=lambda1, fn=IC,
                       method = "BFGS", control = list(maxit=maxit.local, trace=F, reltol=eps),
                       d=d, gamma=gamma, U=U, y=y, model=model, a=a, criterion=criterion)
    }
    lambda.hat <- opt.fit$par; min.Q <- opt.fit$value
  } else stop("Wrong specification of model= !")
  out$opt <- opt.fit
  # OBTAIN THE ESTIMATED TUNING PARAMETERS
  # print(names(opt.fit)); print(lambda.hat); print(min.Q)

  # RE-COMPUTE THE WEIGHTS BASED ON ESTIMATED LAMBDA
  if (model=="PCR.d.c"){
    w.hat <- sign(d > lambda.hat) + 0
    n.comp <- sum(w.hat)
  } else if (model=="PCR.gamma.c") {
    w.hat <- sign(gamma^2 > lambda.hat) +0
    n.comp <- sum(w.hat)
  } else {
    w.hat <- weighting(lambda=lambda.hat, d=d, gamma=gamma, model=model, a=a)
    n.comp <- m
  }
  # print(lambda.hat)
  W <- diag(w.hat)   # MATRIX W

  # OBTAIN THE FITTING RESUTLS
  D <- diag(d); D.inv <- diag(1/d); A <- V%*%D.inv
  # print(dim(A)); print(dim(W))
  beta.hat <- as.vector(A %*% W %*% gamma)       ###############################
  names(beta.hat) <- Xnames
  # print("dim(U)"); print(dim(U))
  # print(beta.hat)
  y.hat <- as.vector(U%*%(gamma*w.hat)) + mean.y

  # OUTPUT
  out <- c(out, list(m=m, d=d, gamma=gamma, formula=formula,
       mean.y=mean.y, mean.x=mean.x, sd.x=sd.x,
       min.Q=min.Q, lambda.hat=lambda.hat, beta.hat=beta.hat,
       w.hat=w.hat, n.comp=n.comp, y.hat=y.hat))
  class(out) <- "WOCR"
  return(out)
}
