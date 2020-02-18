
<!-- README.md is generated from README.Rmd. Please edit that file -->

# missForestFast

`missForestFast` is a project for a case study for improving existing
random-forst-based methods, and added the ability to keep intermediate
results for further study the iterative imputation method.

## Accleration of random-forest-based methods

[`missForestFast`](https://github.com/shangzhi-hong/missForestFast)
provides user with the accelerated version of missing data imputation
powered by [`ranger`](https://CRAN.R-project.org/package=ranger) and
[`randomForestSRC`](https://CRAN.R-project.org/package=randomForestSRC)

## Installation for development version from GitHub

Currently the `missForestFast` package is only available as source files
on GitHub, any interested user can install and test the `RfEmpImp`
package by running the following code in R:

``` r
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("shangzhi-hong/missForestFast")
```

Run example:

``` r
library(missForestFast)
data(iris)
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
impRanger <- missForestRanger(iris.mis, xtrue = iris, keepAll = TRUE)
impSrc <- missForestSrc(iris.mis, xtrue = iris, keepAll = TRUE)
```
