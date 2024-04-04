
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gcube <a href="https://b-cubed-eu.github.io/gcube/"><img src="man/figures/logo.png" align="right" height="120" alt="gcube website"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gcube)](https://CRAN.R-project.org/package=gcube)
[![R-CMD-check](https://github.com/b-cubed-eu/gcube/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/b-cubed-eu/gcube/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/b-cubed-eu/gcube/branch/main/graph/badge.svg)](https://app.codecov.io/gh/b-cubed-eu/gcube/)
[![repo
status](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

The goal of **gcube** is to provide a simulation framework for
biodiversity data cubes using the R programming language. This can start
from simulating multiple species distributed in a landscape over a
temporal scope. In a second phase, the simulation of a variety of
observation processes and effort can generate actual occurrence
datasets. Based on their (simulated) spatial uncertainty, occurrences
can then be designated to a grid to form a data cube.

Simulation studies offer numerous benefits due to their ability to mimic
real-world scenarios in controlled and customizable environments.
Ecosystems and biodiversity data are very complex and involve a
multitude of interacting factors. Simulations allow researchers to model
and understand the complexity of ecological systems by varying
parameters such as spatial and/or temporal clustering, species
prevalence, etc.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("b-cubed-eu/gcube")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gcube)
```

…
