TidyStats <- function(obj, itemName) {
    df <- as.data.frame(
        cbind(
            Iter = seq_along(obj[[itemName]]),
            do.call(rbind, obj[[itemName]])
        ))
    return(df)
}

#' Tidy error from true data during iterations
#'
#' @param obj Input object from `missForestRanger` or `missForestSrc`.
#'
#' @return A data frame containing error from true data.
#' @export
TidyErrAll <- function(obj) {
    return(TidyStats(obj = obj, itemName = "errAll"))
}


#' Tidy out-of-bag error estimates during iterations
#'
#' @param obj Input object from `missForestRanger` or `missForestSrc`.
#'
#' @return A data frame containing out-of-bag error estimates during iterations.
#' @export
TidyOobErrAll <- function(obj) {
    return(TidyStats(obj = obj, itemName = "oobErrAll"))
}


#' Tidy differences between iterations
#'
#' @param obj Input object from `missForestRanger` or `missForestSrc`.
#'
#' @return A data frame containing differences between iterations.
#' @export
TidyDiffAll <- function(obj) {
    return(TidyStats(obj = obj, itemName = "diffAll"))
}
