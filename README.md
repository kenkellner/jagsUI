jagsUI
==========

[![CRAN status](https://www.r-pkg.org/badges/version/jagsUI)](https://cran.r-project.org/web/packages/jagsUI/index.html)
[![Build Status](https://travis-ci.org/kenkellner/jagsUI.svg?branch=master)](https://travis-ci.org/kenkellner/jagsUI)
[![codecov](https://codecov.io/gh/kenkellner/jagsUI/branch/master/graph/badge.svg)](https://codecov.io/gh/kenkellner/jagsUI)

This package runs `JAGS` (Just Another Gibbs Sampler) analyses from within `R`. It acts as a wrapper and alternative interface for the functions in the `rjags` package and adds some custom output and graphical options. It also makes running chains in parallel quick and easy.

You can install the package from [CRAN](https://cran.r-project.org/web/packages/jagsUI/index.html), or get the development version from Github:

```r
devtools::install_github('kenkellner/jagsUI')
```

I am currently working towards a 2.0 release. The major user-facing changes I plan to include in this release (and which are already in the development version) include:

1. A new function `shinyjags()` providing access to a [Shiny](https://shiny.rstudio.com/) GUI for examining and visualizing model output and diagnostics. The dashboard is heavily inspired by [shinystan](https://mc-stan.org/users/interfaces/shinystan). 
2. Ability to track progress of `JAGS` models run in parallel, including estimates of remaining time to completion (may not work in some environments).
3. More robust error handling.
4. Some minor changes to output structure and argument names.

Backend code/development changes include:

1. A complete re-write of the entire codebase, hopefully to be more concise, documented, and maintainable.
2. A complete test suite and use of continuous integration.

I would appreciate any feedback and/or bug reports if you decide to try the development version.

Acknowledgments: Martyn Plummer, developer of the excellent JAGS software package and the `rjags` R package;  Andrew Gelman, Sibylle Sturtz, Uwe Ligges, Yu-Sung Su, and Masanao Yajima, developers of the `R2WinBUGS` and `R2jags` packages on which the package was originally based; Robert Swihart, Marc Kery, Jerome Guelat, Michael Schaub, and Mike Meredith who tested and provided helpful suggestions and improvements for the package.
