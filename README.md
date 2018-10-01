
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tfse <img src="man/figures/logo.png" width="160px" align="right" />

[![Travis build
status](https://travis-ci.org/mkearney/tfse.svg?branch=master)](https://travis-ci.org/mkearney/tfse)
[![Coverage
status](https://codecov.io/gh/mkearney/tfse/branch/master/graph/badge.svg)](https://codecov.io/github/mkearney/tfse?branch=master)

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Various useful functions for working with data and writing functions

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
#>  [1] 0.110092 0.623853 0.605505 0.614679 0.844037 0.981651 0.000000
#>  [8] 0.220183 1.000000 0.477064 0.642202 0.495413 0.247706 0.825688
#> [15] 0.256881 0.733945 0.899083 0.229358 0.155963 0.192661

## rescale standard
rescale_pointscale(x, 1, 10)
#>  [1]  1.99083  6.61468  6.44954  6.53211  8.59633  9.83486  1.00000
#>  [8]  2.98165 10.00000  5.29358  6.77982  5.45872  3.22936  8.43119
#> [15]  3.31193  7.60550  9.09174  3.06422  2.40367  2.73394
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
