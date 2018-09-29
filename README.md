
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
#>  [1] 0.7809524 0.3809524 0.1523810 0.2952381 0.3523810 0.2000000 0.3333333
#>  [8] 0.0952381 0.4476190 0.5619048 0.4190476 0.6380952 0.0000000 0.4000000
#> [15] 0.9714286 0.1047619 0.2666667 1.0000000 0.5904762 0.6285714

## rescale standard
rescale_pointscale(x, 1, 10)
#>  [1]  8.02857  4.42857  2.37143  3.65714  4.17143  2.80000  4.00000
#>  [8]  1.85714  5.02857  6.05714  4.77143  6.74286  1.00000  4.60000
#> [15]  9.74286  1.94286  3.40000 10.00000  6.31429  6.65714
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
