
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rWCVP

<!-- badges: start -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/matildabrown/rWCVP/workflows/R-CMD-check/badge.svg)](https://github.com/matildabrown/rWCVP/actions)
<!-- badges: end -->

<!-- badges: end -->

rWCVP is a package for accessing and using plant name and distribution
data from the [World Checklist of Vascular
Plants](https://wcvp.science.kew.org/)

## Installation

You can install the development version of rWCVP from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
devtools::install_github("matildabrown/rWCVP")
```

## Example

rWCVP makes it easy to get and plot the known distribution of plant
species.

``` r
library(rWCVP)

distribution <- get_distribution("Myrcia guanensis", rank="Species")
plots <- plot_distribution(distribution)

# global map
plots[[1]]

# zoomed-in map
plots[[2]]
```
