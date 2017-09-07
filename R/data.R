#' Housing data for census tracts of Boston from the 1970 census
#'
#' @format The data are 506 observations on 14 variables, \code{cmedv} being the target variable:
#' \describe{
#' \item{\code{crim}}{per capita crime rate by town}
#' \item{\code{zn}}{proportion of residential land zoned for lots over 25,000 sq.ft}
#' \item{\code{indus}}{proportion of non-retail business acres per town}
#' \item{\code{chas}}{Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)}
#' \item{\code{nox}}{nitric oxides concentration (parts per 10 million)}
#' \item{\code{rm}}{average number of rooms per dwelling}
#' \item{\code{age}}{proportion of owner-occupied units built prior to 1940}
#' \item{\code{dis}}{weighted distances to five Boston employment centres}
#' \item{\code{rad}}{index of accessibility to radial highways}
#' \item{\code{tax}}{full-value property-tax rate per USD 10,000}
#' \item{\code{ptratio}}{pupil-teacher ratio by town}
#' \item{\code{b}}{\eqn{1000(B - 0.63)^2} where B is the proportion of blacks by town}
#' \item{\code{lstat}}{percentage of lower status of the population}
#' \item{\code{cmedv}}{corrected median value of owner-occupied homes in USD 1000's}
#' }
#' @source
#' This data set is modified from \code{\link[mlbench]{BostonHousing2}} in the \bold{mlbench}
#' package, which is the corrected version with additional spatial information from the original data contributed by Harrison and Rubinfeld (1979).
#' In the dataframe \code{BostonHousing1}, we have removed \code{town} (name of town), \code{tract} (census tract)
#' \code{lon} (longitude of census tract), \code{lat} (latitude of census tract),  and the
#' \code{medv} (median value with errors).
#'
#' @references
#'\itemize{
#' \item Harrison, D. and Rubinfeld, D. L. (1978). Hedonic prices and the demand for clean air. \emph{Journal
#' of Environmental Economics and Management}, \bold{5}: 81-102.
#' }
#' @examples
#'  data(BostonHousing1)
#'  summary(BostonHousing1)
#'
"BostonHousing1"
