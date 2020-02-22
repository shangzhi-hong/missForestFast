
# missForestFast

`missForestFast` is a project for a case study for improving existing
random-forst-based method
[`missForest`](https://CRAN.R-project.org/package=missForest), and added
the functionalities like outputting intermediate results.

## Accleration of random-forest-based methods

[`missForestFast`](https://github.com/shangzhi-hong/missForestFast)
provides user with the accelerated version of random-forest-based
missing data imputation powered by
[`ranger`](https://CRAN.R-project.org/package=ranger) and
[`randomForestSRC`](https://CRAN.R-project.org/package=randomForestSRC)

## Installation for development version from GitHub

Currently the `missForestFast` package is only available as source files
on GitHub, any interested user can install and test the `missForestFast`
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
