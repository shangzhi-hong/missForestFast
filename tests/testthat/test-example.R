context("Test example")

test_that("Test example for ranger implementation", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    impRanger <- missForestRanger(iris.mis, xtrue = iris, keepAll = T)
    expect_true(sum(is.na(impRanger$ximp)) == 0L)
    expect_true(sum(is.na(impRanger$OOBerror)) == 0L)
    expect_true(sum(is.na(impRanger$error)) == 0L)
    expect_true(isTRUE(length(impRanger$errAll) == impRanger$totalIter) ||
                    isTRUE(length(impRanger$errAll) == impRanger$totalIter - 1))
    expect_true(isTRUE(length(impRanger$oobErrAll) == impRanger$totalIter) ||
                    isTRUE(length(impRanger$oobErrAll) == impRanger$totalIter - 1))
    expect_true(isTRUE(length(impRanger$ximpAll) == impRanger$totalIter) ||
                    isTRUE(length(impRanger$ximpAll) == impRanger$totalIter - 1))
})



test_that("Test example for randomForestSRC implementation", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    impSrc <- missForestSrc(iris.mis, xtrue = iris, keepAll = T)
    expect_true(sum(is.na(impSrc$ximp)) == 0L)
    expect_true(sum(is.na(impSrc$OOBerror)) == 0L)
    expect_true(sum(is.na(impSrc$error)) == 0L)
    expect_true(isTRUE(length(impSrc$errAll) == impSrc$totalIter) ||
                    isTRUE(length(impSrc$errAll) == impSrc$totalIter - 1))
    expect_true(isTRUE(length(impSrc$oobErrAll) == impSrc$totalIter) ||
                    isTRUE(length(impSrc$oobErrAll) == impSrc$totalIter - 1))
    expect_true(isTRUE(length(impSrc$ximpAll) == impSrc$totalIter) ||
                    isTRUE(length(impSrc$ximpAll) == impSrc$totalIter - 1))
})
