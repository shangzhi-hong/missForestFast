#' Nonparametric Missing Value Imputation using RandomFoerstSRC
#'
#' @description
#' 'missForest' is used to impute missing values particularly in the case of
#' mixed-type data. It can be used to impute continuous and/or categorical data
#' including complex interactions and nonlinear relations. It yields an
#' out-of-bag (OOB) imputation error estimate. Moreover, it can be run parallel
#' to save computation time.
#'
#' @param xmis a data matrix with missing values. The columns correspond to the
#' variables and the rows to the observations.
#' @param maxiter maximum number of iterations to be performed given the
#' stopping criterion is not met beforehand.
#' @param ntree number of trees to grow in each forest.
#' @param variablewise logical. If 'TRUE' the OOB error is returned for each
#' variable separately. This can be useful as a reliability check for the
#' imputed variables w.r.t. to a subsequent data analysis.
#' @param decreasing logical. If 'FALSE' then the variables are sorted w.r.t.
#' increasing amount of missing entries during computation.
#' @param verbose logical. If 'TRUE' the user is supplied with additional
#' output between iterations, i.e., estimated imputation error, runtime and if
#' complete data matrix is supplied the true imputation error. See 'xtrue'.
#' @param mtry number of variables randomly sampled at each split. This argument
#' is directly supplied to the 'randomForest' function. Note that the default
#' value is sqrt(p) for both categorical and continuous variables where p is the
#' number of variables in 'xmis'.
#' @param replace logical. If 'TRUE' bootstrap sampling (with replacements) is
#' performed else subsampling (without replacements).
#' @param xtrue complete data matrix
#' @param keepAll logical. If 'TRUE', keep intermediate results.
#' @param forceIter logical. If 'TRUE', force to run until the maximum iteration.
#' @param randInit logical. If 'TRUE', use random samples for initialization.
#' @param varOrderSeq logical. If 'TRUE', impute variables sequentially as the
#' original order.
#' @param ... paramters to pass down.
#'
#' @details
#' After each iteration the difference between the previous and the new
#' imputed data matrix is assessed for the continuous and categorical
#' parts. The stopping criterion is defined such that the imputation
#' process is stopped as soon as both differences have become larger
#' once. In case of only one type of variable the computation stops as
#' soon as the corresponding difference goes up for the first
#' time. However, the imputation last performed where both differences
#' went up is generally less accurate than the previous one. Therefore,
#' whenever the computation stops due to the stopping criterion (and not
#' due to 'maxiter') the before last imputation matrix is returned.
#'
#' The normalized root mean squared error (NRMSE) is defined as:
#'
#'     \deqn{\sqrt{\frac{mean((X_{true} - X_{imp})^2)}{var(X_{true})}}}
#'
#' where \eqn{X_{true}} the complete data matrix, \eqn{X_{imp}} the
#' imputed data matrix and 'mean'/'var' being used as short notation for
#' the empirical mean and variance computed over the continuous missing
#' values only.
#'
#' The proportion of falsely classified (PFC) is also computed over the
#' categorical missing values only.
#'
#' For feasibility reasons 'ntree', 'mtry', 'nodesize' and 'maxnodes' can
#' be chosen smaller. The number of trees can be chosen fairly small
#' since growing many forests (e.g. p forests in each iteration) all
#' observations get predicted a few times. The runtime behaves linear
#' with 'ntree'. In case of high-dimensional data we recommend using a
#' small 'mtry' (e.g. 100 should work) to obtain an appropriate
#' imputation result within a feasible amount of time.
#'
#' Using an appropriate backend 'missForest' can be run parallel.
#' There are two possible ways to do this. One way is to create the random forest
#' object in parallel (parallelize = "forests"). This is most useful
#' if a single forest object takes long to compute and there are not
#' many variables in the data. The second way is to compute multiple random
#' forest classifiers parallel on different variables (parallelize = "variables").
#' This is most useful if the data contains many variables and computing
#' the random forests is not taking too long. For details on how to register
#' a parallel backend see for instance the documentation of 'doParallel').
#' See the vignette for further examples on how to use missForest.
#' I thank Steve Weston for his input regarding parallel computation of 'missForest'.
#'
#' @name missForestSrc
#' @aliases missForestSrc
#'
#' @return
#' \item{ximp}{
#'     imputed data matrix of same type as 'xmis'.
#' }
#' \item{OOBerror}{
#'     estimated OOB imputation error. For the set of continuous
#'     variables in 'xmis' the NRMSE and for the set of categorical variables
#'     the proportion of falsely classified entries is returned. See Details
#'     for the exact definition of these error measures. If 'variablewise'
#'     is set to 'TRUE' then this will be a vector of length 'p' where 'p' is
#'     the number of variables and the entries will be the OOB error for each
#'     variable separately.
#' }
#' \item{error}{
#'     true imputation error. This is only available if 'xtrue'
#'     was supplied. The error measures are the same as for 'OOBerror'.
#' }
#' \item{errAll}{
#'     true imputation error for all imputation states.
#' }
#' \item{oobErrAll}{
#'     estimated OOB imputation error for all imputation states.
#' }
#' \item{totalIter}{
#'     the total of iterations performed.
#' }
#' \item{diffAll}{
#'     differences between imputations.
#' }
#' \item{impVarOrder}{
#'     order of variables under imputation.
#' }
#' \item{timeElapsed}{
#'     time elapsed for imputation in secs.
#' }
#' \item{maxIter}{
#'     whether reached maximum iteration.
#' }
#' @examples
#' \donttest{
#' ## Nonparametric missing value imputation on mixed-type data:
#' data(iris)
#' summary(iris)
#'
#' ## The data contains four continuous and one categorical variable.
#'
#' ## Artificially produce missing values using the 'prodNA' function:
#' set.seed(81)
#' iris.mis <- prodNA(iris, noNA = 0.2)
#' summary(iris.mis)
#'
#' ## Impute missing values providing the complete matrix for
#' ## illustration. Use 'verbose' to see what happens between iterations:
#' iris.imp <- missForestSrc(iris.mis, xtrue = iris, verbose = TRUE)
#'
#' ## The imputation is finished after five iterations having a final
#' ## true NRMSE of 0.143 and a PFC of 0.036. The estimated final NRMSE
#' ## is 0.157 and the PFC is 0.025 (see Details for the reason taking
#' ## iteration 4 instead of iteration 5 as final value).
#'
#' ## The final results can be accessed directly. The estimated error:
#' iris.imp$OOBerror
#'
#' ## The true imputation error (if available):
#' iris.imp$error
#'
#' ## And of course the imputed data matrix (do not run this):
#' ## iris.imp$Ximp
#' }
#'
#' @author Daniel J. Stekhoven, Shangzhi Hong
#'
#' @seealso
#'  \code{\link{mixError}}, \code{\link{prodNA}}, \code{\link{randomForest}}
#'
#' @keywords nonparametric, classes, NA
#'
#' @export
missForestSrc <-
    function(xmis,
             maxiter = 10,
             ntree = 100,
             variablewise = FALSE,
             decreasing = FALSE,
             verbose = FALSE,
             mtry = floor(sqrt(ncol(xmis))),
             replace = TRUE,
             xtrue = NA,
             keepAll = FALSE,
             forceIter = FALSE,
             randInit = FALSE,
             varOrderSeq = FALSE,
             ...
    )
    {
        ## ----------------------------------------------------------------------
        ## Arguments:
        ## xmis         = data matrix with missing values
        ## maxiter      = stop after how many iterations (default = 10)
        ## ntree        = how many trees are grown in the forest (default = 100)
        ## variablewise = (boolean) return OOB errors for each variable separately
        ## decreasing   = (boolean) if TRUE the columns are sorted with decreasing
        ##                amount of missing values
        ## verbose      = (boolean) if TRUE then missForest returns error estimates,
        ##                runtime and if available true error during iterations
        ## mtry         = how many variables should be tried randomly at each node
        ## replace      = (boolean) if TRUE bootstrap sampling (with replacements)
        ##                is performed, else subsampling (without replacements)
        ## xtrue        = complete data matrix
        ##
        ## ----------------------------------------------------------------------
        ## Author: Daniel Stekhoven, stekhoven@stat.math.ethz.ch

        timeInit <- Sys.time()
        ## stop in case of wrong inputs passed to randomForest
        n <- nrow(xmis)
        p <- ncol(xmis)

        ## remove completely missing variables
        if (any(apply(is.na(xmis), 2, sum) == n)) {
            indCmis <- which(apply(is.na(xmis), 2, sum) == n)
            xmis <- xmis[, -indCmis]
            p <- ncol(xmis)
            cat('  removed variable(s)',
                indCmis,
                'due to the missingness of all entries\n')
        }

        ## perform initial S.W.A.G. on xmis (mean imputation)
        ximp <- xmis
        xAttrib <- lapply(xmis, attributes)
        varType <- character(p)
        if (randInit) {
            ximp[] <- lapply(ximp, function(vec) {
                naLoc <- is.na(vec)
                naNum <- sum(naLoc)
                if (naNum > 0) {
                    vec[naLoc] <- sample(vec[!naLoc], size = naNum, replace = TRUE)
                    vec
                }
            })
            for (t.co in 1:p) {
                if (is.null(xAttrib[[t.co]])) {
                    varType[t.co] <- 'numeric'
                } else {
                    varType[t.co] <- 'factor'
                }
            }
        } else {
            for (t.co in 1:p) {
                if (is.null(xAttrib[[t.co]])) {
                    varType[t.co] <- 'numeric'
                    ximp[is.na(xmis[, t.co]), t.co] <-
                        mean(xmis[, t.co], na.rm = TRUE)
                } else {
                    varType[t.co] <- 'factor'
                    ## take the level which is more 'likely' (majority vote)
                    max.level <- max(table(ximp[, t.co]))
                    ## if there are several classes which are major, sample one at random
                    class.assign <-
                        sample(names(which(max.level == summary(ximp[, t.co]))), 1)
                    ## it shouldn't be the NA class
                    if (class.assign != "NA's") {
                        ximp[is.na(xmis[, t.co]), t.co] <- class.assign
                    } else {
                        while (class.assign == "NA's") {
                            class.assign <- sample(names(which(
                                max.level ==
                                    summary(ximp[, t.co])
                            )), 1)
                        }
                        ximp[is.na(xmis[, t.co]), t.co] <- class.assign
                    }
                }
            }
        }

        # extract missingness pattern
        NAloc <- is.na(xmis)
        # how many are missing in the vars
        noNAvar <- apply(NAloc, 2, sum)
        # indices of increasing amount of NA in vars
        if (varOrderSeq) {
            sort.j <- seq_len(p)
        } else {
            sort.j <- order(noNAvar)
            if (decreasing) sort.j <- rev(sort.j)
        }

        ## output
        Ximp <- vector('list', maxiter)

        ## initialize parameters of interest
        iter <- 0
        k <- length(unique(varType))
        convNew <- rep(0, k)
        convOld <- rep(Inf, k)
        OOBerror <- numeric(p)
        names(OOBerror) <- varType

        # Lists for intermediate stats
        errAll <- vector(mode = "list", length = maxiter)
        oobErrAll <- vector(mode = "list", length = maxiter)
        diffAll <- vector(mode = "list", length = maxiter)

        ## setup convergence variables w.r.t. variable types
        if (k == 1) {
            if (unique(varType) == 'numeric') {
                names(convNew) <- c('numeric')
            } else {
                names(convNew) <- c('factor')
            }
            convergence <- c()
            OOBerr <- numeric(1)
        } else {
            names(convNew) <- c('numeric', 'factor')
            convergence <- matrix(NA, ncol = 2)
            OOBerr <- numeric(2)
        }

        ## function to yield the stopping criterion in the following 'while' loop
        stopCriterion <-
            function(varType,
                     convNew,
                     convOld,
                     iter,
                     maxiter,
                     forceIter) {
                if (forceIter) {
                    (iter < maxiter)
                } else {
                    k <- length(unique(varType))
                    if (k == 1) {
                        (convNew < convOld) & (iter < maxiter)
                    } else {
                        ((convNew[1] < convOld[1]) |
                             (convNew[2] < convOld[2])) &
                            (iter < maxiter)
                    }
                }
            }

        varNames <- colnames(ximp)

        # Parameter adapter for rfsrc
        bootstrap <- "by.root"
        if (replace) samptype <- "swr"
        else samptype <- "swor"

        ## iterate missForest
        while (stopCriterion(varType, convNew, convOld, iter, maxiter, forceIter)) {
            if (iter != 0) {
                convOld <- convNew
                OOBerrOld <- OOBerr
            }
            if (verbose) cat("  missForest iteration", iter + 1, "in progress...")
            t.start <- proc.time()
            ximp.old <- ximp
            for (s in 1:p) {
                varInd <- sort.j[s]
                if (noNAvar[[varInd]] != 0) {
                    obsi <- !NAloc[, varInd]
                    misi <- NAloc[, varInd]
                    misX <- ximp[misi, seq(1, p)[-varInd]]
                    typeY <- varType[varInd]

                    yVarName <- varNames[varInd]
                    impFormula <- as.formula(paste0(yVarName, " ~ ."))
                    obsDf <- ximp[obsi, ]

                    if (typeY == "numeric") {
                        xntree <- NULL
                        RF <-
                            rfsrc.fast(
                                formula = impFormula,
                                data = obsDf,
                                ntree = ntree,
                                mtry = mtry,
                                bootstrap = bootstrap,
                                samptype = samptype,
                                forest = TRUE,
                                ...
                            )
                        ## record out-of-bag error
                        OOBerror[varInd] <- RF$err.rate[ntree]
                        misY <- predict(RF, misX)$predicted
                    } else {
                        obsY <- ximp[obsi, varInd]
                        obsY <- factor(obsY)
                        summarY <- summary(obsY)
                        if (length(summarY) == 1) {
                            misY <- factor(rep(names(summarY), sum(misi)))
                        } else {
                            RF <-
                                rfsrc.fast(
                                    formula = impFormula,
                                    data = obsDf,
                                    ntree = ntree,
                                    mtry = mtry,
                                    bootstrap = bootstrap,
                                    samptype = samptype,
                                    forest = TRUE,
                                    ...
                                )
                            ## record out-of-bag error
                            OOBerror[varInd] <- RF$err.rate[[ntree, 1]]
                            ## predict missing parts of Y
                            misY <- predict(RF, misX)$class
                        }
                    }
                    ximp[misi, varInd] <- misY
                }
            }

            if (verbose) cat('done!\n')

            iter <- iter + 1
            Ximp[[iter]] <- ximp

            t.co2 <- 1
            ## check the difference between iteration steps
            for (t.type in names(convNew)) {
                t.ind <- which(varType == t.type)
                if (t.type == "numeric") {
                    convNew[t.co2] <-
                        sum((ximp[, t.ind] - ximp.old[, t.ind]) ^ 2) / sum(ximp[, t.ind] ^ 2)
                } else {
                    dist <-
                        sum(as.character(as.matrix(ximp[, t.ind])) != as.character(as.matrix(ximp.old[, t.ind])))
                    convNew[t.co2] <-
                        dist / (n * sum(varType == 'factor'))
                }
                t.co2 <- t.co2 + 1
            }

            ## compute estimated imputation error
            if (!variablewise) {
                NRMSE <- sqrt(mean(OOBerror[varType == 'numeric']) /
                                  var(as.vector(as.matrix(xmis[, varType == 'numeric'])),
                                      na.rm = TRUE))
                PFC <- mean(OOBerror[varType == 'factor'])
                if (k == 1) {
                    if (unique(varType) == 'numeric') {
                        OOBerr <- NRMSE
                        names(OOBerr) <- 'NRMSE'
                    } else {
                        OOBerr <- PFC
                        names(OOBerr) <- 'PFC'
                    }
                } else {
                    OOBerr <- c(NRMSE, PFC)
                    names(OOBerr) <- c('NRMSE', 'PFC')
                }
            } else {
                OOBerr <- OOBerror
                names(OOBerr)[varType == 'numeric'] <- 'MSE'
                names(OOBerr)[varType == 'factor'] <- 'PFC'
            }

            if (any(!is.na(xtrue))) {
                err <- suppressWarnings(mixError(ximp, xmis, xtrue))
                errAll[[iter]] <- err
            }

            oobErrAll[[iter]] <- OOBerr

            ## return status output, if desired
            if (verbose) {
                delta.start <- proc.time() - t.start
                if (any(!is.na(xtrue))) {
                    cat("    error(s):", err, "\n")
                }
                cat("    estimated error(s):", OOBerr, "\n")
                cat("    difference(s):", convNew, "\n")
                cat("    time:", delta.start[3], "seconds\n\n")
            }
        }#end while((convNew<convOld)&(iter<maxiter)){

        ## produce output w.r.t. stopping rule
        if (iter == maxiter) {
            if (any(is.na(xtrue))) {
                out <- list(ximp = Ximp[[iter]],
                            OOBerror = OOBerr,
                            totalIter = iter,
                            diffAll = diffAll,
                            impVarOrder = colnames(xmis)[sort.j],
                            timeElapsed = difftime(Sys.time(), timeInit, units = "secs"),
                            maxIter = TRUE)
            } else {
                out <- list(ximp = Ximp[[iter]],
                            OOBerror = OOBerr,
                            error = err,
                            errAll = errAll,
                            oobErrAll = oobErrAll,
                            totalIter = iter,
                            diffAll = diffAll,
                            impVarOrder = colnames(xmis)[sort.j],
                            timeElapsed = difftime(Sys.time(), timeInit, units = "secs"),
                            maxIter = TRUE)
            }
            if (keepAll) out[["ximpAll"]] <- Ximp
        } else {
            if (any(is.na(xtrue))) {
                out <- list(ximp = Ximp[[iter - 1]],
                            OOBerror = OOBerrOld,
                            errAll = errAll[seq_len(iter - 1)],
                            oobErrAll = oobErrAll[seq_len(iter - 1)],
                            totalIter = iter,
                            diffAll = diffAll[seq_len(iter - 1)],
                            impVarOrder = colnames(xmis)[sort.j],
                            timeElapsed = difftime(Sys.time(), timeInit, units = "secs"),
                            maxIter = FALSE)
            } else {
                out <- list(
                    ximp = Ximp[[iter - 1]],
                    OOBerror = OOBerrOld,
                    error = suppressWarnings(mixError(Ximp[[iter - 1]], xmis, xtrue)),
                    errAll = errAll[seq_len(iter - 1)],
                    oobErrAll = oobErrAll[seq_len(iter - 1)],
                    totalIter = iter,
                    diffAll = diffAll[seq_len(iter - 1)],
                    impVarOrder = colnames(xmis)[sort.j],
                    timeElapsed = difftime(Sys.time(), timeInit, units = "secs"),
                    maxIter = FALSE)

            }
            if (keepAll) out[["ximpAll"]] <- Ximp[seq_len(iter - 1)]
        }
        class(out) <- 'missForest'
        return(out)
    }
