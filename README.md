
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfse <img src="man/figures/logo.png" width="160px" align="right" />

[![Travis build
status](https://travis-ci.org/mkearney/tfse.svg?branch=master)](https://travis-ci.org/mkearney/tfse)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

> tfse = **T**he **F**irst **S**criptorium **E**mporium

## Install

To install the package

``` r
## install from github
devtools::install_github("mkearney/tfse")
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
#>  [1] 0.3932584 0.0112360 0.0000000 0.7752809 0.6292135 0.6741573 0.8089888
#>  [8] 1.0000000 0.1460674 0.9101124 0.2808989 0.6067416 0.1685393 0.7078652
#> [15] 0.9213483 0.0898876 0.8651685 0.8426966 0.7528090 0.6179775

## rescale standard
rescale_pointscale(x, 1, 10)
#>  [1]  4.53933  1.10112  1.00000  7.97753  6.66292  7.06742  8.28090
#>  [8] 10.00000  2.31461  9.19101  3.52809  6.46067  2.51685  7.37079
#> [15]  9.29213  1.80899  8.78652  8.58427  7.77528  6.56180
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
