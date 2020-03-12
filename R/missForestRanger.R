##
## MissForest - nonparametric missing value imputation for mixed-type data
##
## This R script contains the actual missForest function.
##
## Author: D.Stekhoven, stekhoven@stat.math.ethz.ch
##
## Acknowledgement: Steve Weston for input regarding parallel execution (2012)
##############################################################################
#
# missForestFast using ranger
#
# Shangzhi Hong, Feb 2020
#
##############################################################################

missForestRanger <-
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

        ## extract missingness pattern
        NAloc <- is.na(xmis)            # where are missings
        noNAvar <-
            apply(NAloc, 2, sum) # how many are missing in the vars
        sort.j <-
            order(noNAvar)        # indices of increasing amount of NA in vars
        if (decreasing)
            sort.j <- rev(sort.j)
        sort.noNAvar <- noNAvar[sort.j]

        ## compute a list of column indices for variable parallelization
        nzsort.j <- sort.j[sort.noNAvar > 0]


        ## output
        Ximp <- vector('list', maxiter)

        ## initialize parameters of interest
        iter <- 0
        k <- length(unique(varType))
        convNew <- rep(0, k)
        convOld <- rep(Inf, k)
        OOBerror <- numeric(p)
        names(OOBerror) <- varType

        errAll <- vector(mode = "list", length = maxiter)
        oobErrAll <- vector(mode = "list", length = maxiter)

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
                             (convNew[2] < convOld[2])) & (iter < maxiter)
                    }
                }
            }

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
                    obsY <- ximp[obsi, varInd]
                    obsX <- ximp[obsi, seq(1, p)[-varInd]]
                    misX <- ximp[misi, seq(1, p)[-varInd]]
                    typeY <- varType[varInd]
                    if (typeY == "numeric") {
                        RF <- ranger(
                            x = obsX,
                            y = obsY,
                            num.trees = ntree,
                            mtry = mtry,
                            replace = replace,
                            ...
                        )
                        ## record out-of-bag error
                        OOBerror[varInd] <- RF$prediction.error
                        misY <- predictions(predict(RF, misX))
                    } else {
                        obsY <- factor(obsY)
                        summarY <- summary(obsY)
                        if (length(summarY) == 1) {
                            misY <- factor(rep(names(summarY), sum(misi)))
                        } else {
                            RF <- ranger(
                                x = obsX,
                                y = obsY,
                                num.trees = ntree,
                                mtry = mtry,
                                replace = replace,
                                ...
                            )
                            ## record out-of-bag error
                            OOBerror[varInd] <- RF$prediction.error
                            ## predict missing parts of Y
                            misY <- predictions(predict(RF, misX))
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
                            timeElapsed = difftime(Sys.time(), timeInit, units = "secs"))
            } else {
                out <- list(ximp = Ximp[[iter]],
                            OOBerror = OOBerr,
                            error = err,
                            errAll = errAll,
                            oobErrAll = oobErrAll,
                            totalIter = iter,
                            timeElapsed = difftime(Sys.time(), timeInit, units = "secs")
                            )
            }
            if (keepAll) out[["ximpAll"]] <- Ximp
        } else {
            if (any(is.na(xtrue))) {
                out <- list(ximp = Ximp[[iter - 1]],
                            OOBerror = OOBerrOld,
                            errAll = errAll[seq_len(iter - 1)],
                            oobErrAll = oobErrAll[seq_len(iter - 1)],
                            totalIter = iter,
                            timeElapsed = difftime(Sys.time(), timeInit, units = "secs")
                            )
            } else {
                out <- list(
                    ximp = Ximp[[iter - 1]],
                    OOBerror = OOBerrOld,
                    error = suppressWarnings(mixError(Ximp[[iter - 1]], xmis, xtrue)),
                    errAll = errAll[seq_len(iter - 1)],
                    oobErrAll = oobErrAll[seq_len(iter - 1)],
                    totalIter = iter,
                    timeElapsed = difftime(Sys.time(), timeInit, units = "secs")
                    )

            }
            if (keepAll) out[["ximpAll"]] <- Ximp[seq_len(iter - 1)]
        }
        class(out) <- 'missForest'
        return(out)
    }
