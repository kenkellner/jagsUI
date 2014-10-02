jagsUI
==========

This package runs JAGS (Just Another Gibbs Sampler) analyses from within R. It acts as a wrapper and alternative interface for the functions in the rjags package and adds some custom output and graphical options. 

There are several other similar packages (R2jags, runjags, and of course rjags itself). I wrote this one to allow tighter control over the JAGS adaptive and burn-in periods (see the issue described here: http://stats.stackexchange.com/questions/45193/r2jags-does-not-remove-the-burn-in-part-sometimes) and to simplify other common tasks such as formatting data, plotting results, and running chains in parallel.

To install the package independent of CRAN:

1. Download the latest source package .tar.gz or Windows binary .zip file from the 'Release' tab of the repository (https://github.com/kenkellner/jagsUI). 
2. The jagsUI library requires packages rjags and coda. If you have previously installed the coda and/or rjags packages, I highly recommend removing and re-installing them first:

        remove.packages(c('coda','rjags'))
        install.packages(c('coda','rjags'))
3. Finally, in R or Rstudio, choose to install a package from a local/archive file and select the downloaded package.
Alternatively, you can install directly from R, using package devtools (which installs the other required packages automatically). Devtools may complain that Rtools are not installed, but they are not necessary to install this package, so you can safely ignore the warning.
```
remove.packages(c('coda','rjags'))
library(devtools)
install_github("kenkellner/jagsUI")
```
Acknowledgments: Martyn Plummer, developer of the excellent JAGS software package and the rjags R package;  Andrew Gelman, Sibylle Sturtz, Uwe Ligges, Yu-Sung Su, and Masanao Yajima, developers of the R2WinBUGS and R2jags packages on which my work is based; Robert Swihart, Marc Kery, Jerome Guelat, and Michael Schaub, who tested and provided helpful suggestions for early versions of the package.
