
<!-- README.md is generated from README.Rmd. Please edit that file -->

# valuaciones

<!-- badges: start -->
<!-- badges: end -->

The goal of valuaciones is to …

## Installation

You can install the development version of valuaciones like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(valuaciones)
#> Warning: replacing previous import 'data.table::first' by 'dplyr::first' when
#> loading 'valuaciones'
#> Warning: replacing previous import 'data.table::last' by 'dplyr::last' when
#> loading 'valuaciones'
#> Warning: replacing previous import 'data.table::between' by 'dplyr::between'
#> when loading 'valuaciones'
#> Warning: replacing previous import 'dplyr::group_rows' by
#> 'kableExtra::group_rows' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::set.coresOption' by
#> 'spdep::set.coresOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::set.ClusterOption' by
#> 'spdep::set.ClusterOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::set.mcOption' by
#> 'spdep::set.mcOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::get.ClusterOption' by
#> 'spdep::get.ClusterOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::get.coresOption' by
#> 'spdep::get.coresOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::set.ZeroPolicyOption' by
#> 'spdep::set.ZeroPolicyOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::get.ZeroPolicyOption' by
#> 'spdep::get.ZeroPolicyOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::get.VerboseOption' by
#> 'spdep::get.VerboseOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::set.VerboseOption' by
#> 'spdep::set.VerboseOption' when loading 'valuaciones'
#> Warning: replacing previous import 'spatialreg::get.mcOption' by
#> 'spdep::get.mcOption' when loading 'valuaciones'
#> Warning: replacing previous import 'data.table::shift' by 'terra::shift' when
#> loading 'valuaciones'
#> Warning: replacing previous import 'dplyr::intersect' by 'terra::intersect'
#> when loading 'valuaciones'
#> Warning: replacing previous import 'dplyr::union' by 'terra::union' when
#> loading 'valuaciones'
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
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
