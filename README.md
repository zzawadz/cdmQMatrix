# cdmQMatrix

[![Build Status](https://travis-ci.org/zzawadz/cdmQMatrix.svg?branch=master)](https://travis-ci.org/zzawadz/cdmQMatrix)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/zzawadz/cdmQMatrix?branch=master&svg=true)](https://ci.appveyor.com/project/zzawadz/cdmQMatrix)
[![Coverage status](https://codecov.io/gh/zzawadz/cdmQMatrix/branch/master/graph/badge.svg)](https://codecov.io/github/zzawadz/cdmQMatrix?branch=master)

## Installation

You can install cdmQMatrix from github with:

``` r
# install.packages("devtools")
devtools::install_github("zzawadz/cdmQMatrix")
```

## Example

This is a basic example which shows you how to solve a common problem:

```r
## basic example
library(ltm)
library(cdmQMatrix)
data("WIRS")
 
fitQmat(WIRS, verbose = TRUE)$final.mat
#       Item 1 Item 2 Item 3 Item 4 Item 5 Item 6
# Item1      1      0      1      0      1      1
# Item2      0      0      0      1      0      1
```
