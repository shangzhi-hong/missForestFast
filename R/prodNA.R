#' Introduce Missing Values Completely at Random
#'
#' @description
#' 'prodNA' artificially introduces missing values. Entries in the given
#' dataframe are deleted completely at random up to the specified amount.
#'
#' @param x dataframe subjected to missing value introduction.
#' @param noNA proportion of missing values w.r.t. the number of entries of 'x'.
#'
#' @name prodNA
#'
#' @return
#' dataframe with missing values.
#'
#' @keywords classes, NA
#'
#' @seealso \code{\link{missForest}}
#'
#' @note
#' This function is internally used by \code{\link{missForest}} and
#' \code{\link{mixError}}.
#'
#' @examples
#' \donttest{
#' data(iris)
#' ## Introduce 5% of missing values to the iris data set
#' iris.mis <- prodNA(iris, 0.05)
#' summary(iris.mis)
#' }
#' @author Daniel J. Stekhoven
#'
#' @export
prodNA <- function(x, noNA = 0.1){
  n <- nrow(x)
  p <- ncol(x)
  NAloc <- rep(FALSE, n*p)
  NAloc[sample(n*p, floor(n*p*noNA))] <- TRUE
  x[matrix(NAloc, nrow = n, ncol = p)] <- NA
  return(x)
}
