
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfse <img src="man/figures/logo.png" width="160px" align="right" />

[![Travis build
status](https://travis-ci.org/mkearney/tfse.svg?branch=master)](https://travis-ci.org/mkearney/tfse)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Coverage
status](https://codecov.io/gh/mkearney/tfse/branch/master/graph/badge.svg)](https://codecov.io/github/mkearney/tfse?branch=master)

> Various useful functions for working with data and writing functions

## Install

To install the package

``` r
## install from github
remotes::install_github("mkearney/tfse")
```

## Usage

Load the package and go

``` r
## load pkg
library(tfse)

## use functions like col2hex
col2hex("greenyellow")
#> [1] "#ADFF2F"

## rescale values
x <- sample(-10:100, 20)

## rescale standard
rescale_standard(x)
#>  [1] 1.0000000 0.9708738 0.2912621 0.2815534 0.4466019 0.5825243 0.6407767
#>  [8] 0.6504854 0.4368932 0.5922330 0.6116505 0.1553398 0.3009709 0.2427184
#> [15] 0.0000000 0.8446602 0.0873786 0.0485437 0.3980583 0.4563107

## rescale standard
rescale_pointscale(x, 1, 10)
#>  [1] 10.00000  9.73786  3.62136  3.53398  5.01942  6.24272  6.76699
#>  [8]  6.85437  4.93204  6.33010  6.50485  2.39806  3.70874  3.18447
#> [15]  1.00000  8.60194  1.78641  1.43689  4.58252  5.10680
```

## Help

View the help documentation

``` r
## view R help documentatoin
help(package = "tfse")
```

## about tfse

  - This is my personal R package of utility functions
  - Why *tfse*? The acronym was creatively reconfigured from its
    originally intended state
