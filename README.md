
<!-- README.md is generated from README.Rmd. Please edit that file -->

# missForestFast

## Accleration of random-forest-based methods, a case study for iterative missing data imputation

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
