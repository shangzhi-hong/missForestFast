#' Normalized Root Mean Squared Error
#'
#' @description
#' 'nrmse' computes the normalized root mean squared error for a given complete
#' data matrix, imputed data matrix and the data matrix containing missing
#' values.
#'
#' @param ximp imputed data matrix with variables in the columns and observations
#' in the rows. Note there should not be any missing values.
#' @param xmis data matrix with missing values.
#' @param xtrue complete data matrix. Note there should not be any missing values.
#'
#' @name nrmse
#'
#' @return
#' see Title.
#'
#' @keywords error
#'
#' @references
#' Oba et al. (2003), 'A Bayesian missing value estimation method for gene
#' expression profile data', Bioinformatics, 19(16), 2088-2096
#'
#' @seealso \code{\link{mixError}}
#'
#' @note
#' The NRMSE can only be computed for continuous data. For categorical or
#' mixed-type data see \code{\link{mixError}}.
#' This function is internally used by \code{\link{mixError}}.
#'
#' @author Daniel J. Stekhoven
#'
#' @export
nrmse <- function(ximp, xmis, xtrue){
  mis <- is.na(xmis)
  sqrt(mean((ximp[mis]-xtrue[mis])^{2})/var(xtrue[mis]))
}
