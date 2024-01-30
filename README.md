# geboes.score

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/geboes.score/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/geboes.score/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/geboes.score/branch/main/graph/badge.svg)](https://app.codecov.io/gh/billdenney/geboes.score?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/geboes.score)](https://CRAN.R-project.org/package=geboes.score)
<!-- badges: end -->

The goal of geboes.score is to enable evaluation and validation of the Geboes
score for histological assessment of inflammation in ulcerative colitis.

## Installation

You can install the development version of geboes.score like so:

``` r
remotes::install_github("billdenney/geboes.score")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geboes.score)
max_geboes_score(c("0.0", "1.2", "2A.2", "2B.1", "3.3", "4.1", "5.0")) # "4.1"
```
