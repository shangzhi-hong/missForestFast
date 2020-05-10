context("Test for tidy funcs")


test_that("TidyFuncs works", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    iterNum <- 10
    impObj <- missForestRanger(
        iris.mis,
        xtrue = iris,
        keepAll = TRUE,
        maxiter = iterNum,
        forceIter = TRUE)
    dfDiff <- TidyDiffAll(impObj)
    expect_s3_class(dfDiff, "data.frame")
    expect_equal(nrow(dfDiff), iterNum)
    dfErr <- TidyErrAll(impObj)
    expect_s3_class(dfErr, "data.frame")
    expect_equal(nrow(dfErr), iterNum)
    dfOobErr <- TidyOobErrAll(impObj)
    expect_s3_class(dfOobErr, "data.frame")
    expect_equal(nrow(dfOobErr), iterNum)
})
