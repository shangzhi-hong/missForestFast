#' Extract Variable Types from a Dataframe
#'
#' @description
#' 'varClass' returns the variable types of a dataframe. It is used internally
#' in several functions of the 'missForest'-package.
#'
#' @param x data frame with variables in the columns.
#'
#' @name varClass
#'
#' @return
#' a vector of length p where p denotes the number of columns in 'x'. The
#' entries are "numeric" for continuous variables and "factor" for
#' categorical variables.
#'
#' @keywords classes
#'
#' @references
#' Oba et al. (2003), 'A Bayesian missing value estimation method for gene
#' expression profile data', Bioinformatics, 19(16), 2088-2096
#'
#' @seealso \code{\link{missForest}}, \code{\link{mixError}}, \code{\link{nrmse}}
#'
#' @note
#' This function is internally used by \code{\link{missForest}} and
#' \code{\link{mixError}}.
#'
#' @examples
#' \donttest{
#' data(iris)
#' varClass(iris)
#' ## We have four continuous and one categorical variable.
#' }
#' @author Daniel J. Stekhoven
#'
#' @export
varClass <- function(x){
  xAttrib <- lapply(x, attributes)
  p <- ncol(x)
  x.types <- character(p)
  for (t.co in 1:p){
    if (is.null(xAttrib[[t.co]])){
      x.types[t.co] <- 'numeric'
    } else {
      x.types[t.co] <- 'factor'
    }
  }
  return(x.types)
}
