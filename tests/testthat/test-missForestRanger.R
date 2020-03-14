context("Test run ranger implementation")

test_that("Test for ranger implementation", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    for (varOrderSeq in c(TRUE, FALSE)) {
        for (randInit in c(TRUE, FALSE)) {
            impRanger <-
                missForestRanger(
                    iris.mis,
                    xtrue = iris,
                    keepAll = TRUE,
                    varOrderSeq = varOrderSeq,
                    randInit = randInit
                )
            expect_true(sum(is.na(impRanger$ximp)) == 0L)
            expect_true(sum(is.na(impRanger$OOBerror)) == 0L)
            expect_true(sum(is.na(impRanger$error)) == 0L)
            expect_true(
                isTRUE(length(impRanger$errAll) == impRanger$totalIter) ||
                    isTRUE(
                        length(impRanger$errAll) == impRanger$totalIter - 1
                    )
            )
            expect_true(
                isTRUE(
                    length(impRanger$oobErrAll) == impRanger$totalIter
                ) ||
                    isTRUE(
                        length(impRanger$oobErrAll) == impRanger$totalIter - 1
                    )
            )
            expect_true(
                isTRUE(length(impRanger$ximpAll) == impRanger$totalIter) ||
                    isTRUE(
                        length(impRanger$ximpAll) == impRanger$totalIter - 1
                    )
            )
            expect_equal(length(impRanger[["impVarOrder"]]), ncol(iris.mis))
            targetIter <- 20
            impRanger <- missForestRanger(
                iris.mis,
                xtrue = iris,
                maxiter = targetIter,
                keepAll = TRUE,
                forceIter = TRUE,
                varOrderSeq = varOrderSeq,
                randInit = randInit
            )
            expect_equal(length(impRanger[["oobErrAll"]]), targetIter)
            expect_equal(length(impRanger[["errAll"]]), targetIter)
        }
    }
})

