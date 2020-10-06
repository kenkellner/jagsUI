jagsUI
==========

[![CRAN status](https://www.r-pkg.org/badges/version/jagsUI)](https://cran.r-project.org/web/packages/jagsUI/index.html)
[![Build Status](https://travis-ci.org/kenkellner/jagsUI.svg?branch=master)](https://travis-ci.org/kenkellner/jagsUI)

This package runs `JAGS` (Just Another Gibbs Sampler) analyses from within `R`. It acts as a wrapper and alternative interface for the functions in the `rjags` package and adds some custom output and graphical options. It also makes running chains in parallel quick and easy.

You can install the package from [CRAN](https://cran.r-project.org/web/packages/jagsUI/index.html), or get the development version from Github:

```r
devtools::install_github('kenkellner/jagsUI')
```

Acknowledgments: Martyn Plummer, developer of the excellent JAGS software package and the `rjags` R package;  Andrew Gelman, Sibylle Sturtz, Uwe Ligges, Yu-Sung Su, and Masanao Yajima, developers of the `R2WinBUGS` and `R2jags` packages on which the package was originally based; Robert Swihart, Marc Kery, Jerome Guelat, Michael Schaub, and Mike Meredith who tested and provided helpful suggestions and improvements for the package.
