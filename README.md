
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 🚀 VoyageR

VoyageR is a wrapper for [Voyager 2](https://github.com/vega/voyager), a
JavaScript tool for exploratory data analysis that makes it easy to
specify a [vega](https://github.com/vega/vega) chart specification,
while generating and suggesting possible alternative charts.

## 🧑‍🚀 Screenshot

![Screenshot of the shiny app running in a
browser](./man/figures/screenshot-app.png)

## 🛰️ Installation

You can install the latest version of VoyageR from github with:

``` r
remotes::install_github("jansim/VoyageR")
```

## 🔭 Example

``` r
# Load some data
data("mtcars")

# Start VoyageR
VoyageR::VoyageR()

# Start VoyageR and immediately load a dataset
VoyageR::VoyageR(mtcars)

# piping is also possible
library(magrittr)
mtcars %>% VoyageR::VoyageR()
```
