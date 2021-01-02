
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdb2021

<!-- badges: start -->

[![R-CMD-check](https://github.com/hjmbigdatabowl/bdb2021/workflows/R-CMD-check/badge.svg)](https://github.com/hjmbigdatabowl/bdb2021/actions)
<!-- badges: end -->

The goal of bdb2021 is to …

## Installation

You can install the released version of bdb2021 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bdb2021")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hjmbigdatabowl/bdb2021")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bdb2021)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

Testing embedding a Shiny app:

<style>
.shiny-app-frame {
  position: fixed;
  left: 0;
  top: 50px;
  bottom: 0;
  right: 0;
}
.shiny-app-frame iframe {
  width: 100%;
  height: 100%;
  border: none;
}
</style>

<div class="shiny-app-frame">

<iframe src="https://gallery.shinyapps.io/083-front-page">

</iframe>

</div>
