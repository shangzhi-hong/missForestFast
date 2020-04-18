context("Test run randomForestSRC implementation")


test_that("Test for randomForestSRC implementation", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    for (varOrderSeq in c(TRUE, FALSE)) {
        for (randInit in c(TRUE, FALSE)) {
            impSrc <- missForestSrc(
                iris.mis,
                xtrue = iris,
                keepAll = TRUE,
                varOrderSeq = varOrderSeq,
                randInit = randInit
            )
            expect_true(sum(is.na(impSrc$ximp)) == 0L)
            expect_true(sum(is.na(impSrc$OOBerror)) == 0L)
            expect_true(sum(is.na(impSrc$error)) == 0L)
            expect_true(
                isTRUE(length(impSrc$errAll) == impSrc$totalIter) ||
                    isTRUE(length(impSrc$errAll) == impSrc$totalIter - 1)
            )
            expect_true(
                isTRUE(length(impSrc$oobErrAll) == impSrc$totalIter) ||
                    isTRUE(length(impSrc$oobErrAll) == impSrc$totalIter - 1)
            )
            expect_true(
                isTRUE(length(impSrc$ximpAll) == impSrc$totalIter) ||
                    isTRUE(length(impSrc$ximpAll) == impSrc$totalIter - 1)
            )
            expect_true(
                isTRUE(length(impSrc$diffAll) == impSrc$totalIter) ||
                    isTRUE(
                        length(impSrc$diffAll) == impSrc$totalIter - 1
                    )
            )
            expect_equal(length(impSrc[["impVarOrder"]]), ncol(iris.mis))
            targetIter <- 20
            impSrc <- missForestSrc(
                iris.mis,
                xtrue = iris,
                maxiter = targetIter,
                keepAll = TRUE,
                forceIter = TRUE,
                varOrderSeq = varOrderSeq,
                randInit = randInit
            )
            expect_equal(length(impSrc[["oobErrAll"]]), targetIter)
            expect_equal(length(impSrc[["errAll"]]), targetIter)
            expect_equal(length(impSrc[["diffAll"]]), targetIter)
        }
    }
})
