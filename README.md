# cdmQMatrix

## Why package?

I don't remember. I think that I had to estimate a Q-Matrix for a Cognitive Diagnostic Model.  I probably hooked on a one, very specific algorithm, and because I was unable to find it's implementation in R I created my own. But the problem is, that there are better algorithms implemented in existing R packages. Here are some references (I wish I had used one of them):

- https://www.jstatsoft.org/index.php/jss/article/view/v074i02/v74i02.pdf - link to the description of the CDM package from the Journal of Statistical Software.
- http://pareonline.net/getvn.asp?v=20&n=11 - another paper about CDM package.
- http://pareonline.net/getvn.asp?v=15&n=3 - Computer Adaptive Testing with Cognitive Diagnostic Models.
- http://www.assess.com/what-are-cognitive-diagnostic-models/ - short description of the basic idea behind Cognitive Diagnostic Models.

Every cloud has a silver lining. Now I have here some resources if I ever needed to go back to the subject.

## Please don't use this package;)

### Package info:

[![Build Status](https://travis-ci.org/zzawadz/cdmQMatrix.svg?branch=master)](https://travis-ci.org/zzawadz/cdmQMatrix)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/zzawadz/cdmQMatrix?branch=master&svg=true)](https://ci.appveyor.com/project/zzawadz/cdmQMatrix)
[![Coverage status](https://codecov.io/gh/zzawadz/cdmQMatrix/branch/master/graph/badge.svg)](https://codecov.io/github/zzawadz/cdmQMatrix?branch=master)

#### Installation

You can install cdmQMatrix from github with:

``` r
# install.packages("devtools")
devtools::install_github("zzawadz/cdmQMatrix")
```

#### Example

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
