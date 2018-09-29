
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfse <img src="man/figures/logo.png" width="160px" align="right" />

[![Travis build
status](https://travis-ci.org/mkearney/tfse.svg?branch=master)](https://travis-ci.org/mkearney/tfse)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

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
#>  [1] 0.0000000 0.8130841 0.8037383 0.7850467 0.9252336 0.1121495 0.4112150
#>  [8] 1.0000000 0.1682243 0.2523364 0.9906542 0.2616822 0.5233645 0.3271028
#> [15] 0.9065421 0.0934579 0.1588785 0.7009346 0.0280374 0.3177570

## rescale standard
rescale_pointscale(x, 1, 10)
#>  [1]  1.00000  8.31776  8.23364  8.06542  9.32710  2.00935  4.70093
#>  [8] 10.00000  2.51402  3.27103  9.91589  3.35514  5.71028  3.94393
#> [15]  9.15888  1.84112  2.42991  7.30841  1.25234  3.85981
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
