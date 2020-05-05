#' Compute Imputation Error for Mixed-type Data
#'
#' @description
#' `mixError` is used to calculate the imputation error particularly in the case
#' of mixed-type data. Given the complete data matrix and the data matrix
#' containing the missing values the normalized root mean squared error for
#' the continuous and the proportion of falsely classified entries for the
#' categorical variables are computed.
#'
#' @param ximp imputed data matrix with variables in the columns and observations
#' in the rows. Note there should not be any missing values.
#' @param xmis data matrix with missing values.
#' @param xtrue complete data matrix. Note there should not be any missing values.
#'
#' @name mixError
#'
#' @return
#' imputation error. In case of continuous variables only this is the normalized
#' root mean squared error (NRMSE, see 'help(missForest)' for further details).
#' In case of categorical variables onlty this is the proportion of falsely
#' classified entries (PFC). In case of mixed-type variables both error measures
#' are supplied.
#'
#' @keywords classes, NA
#'
#' @seealso \code{\link{missForest}}
#'
#' @author Daniel J. Stekhoven
#'
#' @examples
#' \dontest{
#' ## Compute imputation error for mixed-type data:
#' data(iris)
#'
#' ## Artificially produce missing values using the 'prodNA' function:
#' set.seed(81)
#' iris.mis <- prodNA(iris, noNA = 0.2)
#'
#' ## Impute missing values using 'missForest':
#' iris.imp <- missForest(iris.mis)
#'
#' ## Compute the true imputation error manually:
#' err.imp <- mixError(iris.imp$ximp, iris.mis, iris)
#' err.imp
#' }
#' @export
mixError <- function(ximp, xmis, xtrue)
{
  ## Purpose:
  ## Calculates the difference between to matrices. For all numeric
  ## variables the NRMSE is used and for all categorical variables
  ## the relative number of false entries is returned.
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ximp      = (imputed) matrix
  ## xmis      = matrix with missing values
  ## xtrue     = true matrix (or any matrix to be compared with ximp)
  ## ----------------------------------------------------------------------
  ## Author: Daniel Stekhoven, Date: 26 Jul 2010, 10:10

  if (class(ximp)=='missForest')
    stop("'xmis' is not of class 'missForest' - maybe you forgot to point at the\n  list element $ximp from the missForest output object.")
  x.types <- varClass(ximp)
  n <- nrow(ximp)
  k <- length(unique(x.types))
  err <- rep(Inf, k)
  t.co <- 1
  if (k == 1){
    if (unique(x.types) == 'numeric'){
      names(err) <- c('NRMSE')
    } else {
      names(err) <- c('PFC')
      t.co <- 1
    }
  } else {
    names(err) <- c('NRMSE', 'PFC')
    t.co <- 2
  }
  ## for (t.type in names(err)){
  for (t.type in x.types){
    t.ind <- which(x.types == t.type)
    if (t.type == "numeric"){
      err[1] <- nrmse(ximp[,t.ind], xmis[,t.ind], xtrue[,t.ind])
    } else {
      dist <- sum(as.character(as.matrix(ximp[,t.ind])) != as.character(as.matrix(xtrue[,t.ind])))
      no.na <- sum(is.na(xmis[,x.types == 'factor']))
      if (no.na == 0){
        err[t.co] <- 0
      } else {
        err[t.co] <- dist / no.na
      }
    }
  }
  return(err)
}
