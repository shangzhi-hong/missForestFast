context("Test example")

test_that("Test example for ranger implementation", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    irisImpRanger <- missForestRanger(iris.mis, xtrue = iris)
    expect_true(sum(is.na(irisImpRanger$ximp)) == 0L)
    expect_true(sum(is.na(irisImpRanger$OOBerror)) == 0L)
    expect_true(sum(is.na(irisImpRanger$error)) == 0L)
})



test_that("Test example for ranger implementation", {
    data(iris)
    set.seed(81)
    iris.mis <- prodNA(iris, noNA = 0.2)
    irisImpSrc <- missForestSrc(iris.mis, xtrue = iris)
    expect_true(sum(is.na(irisImpSrc$ximp)) == 0L)
    expect_true(sum(is.na(irisImpSrc$OOBerror)) == 0L)
    expect_true(sum(is.na(irisImpSrc$error)) == 0L)
})
