jagsUI
==========

This package runs JAGS (Just Another Gibbs Sampler) analyses from within R. It acts as a wrapper and alternative interface for the functions in the rjags package and adds some custom output and graphical options. 

There are several other similar packages (R2jags, runjags, and of course rjags itself). I wrote this one to allow tighter control over the JAGS adaptive and burn-in periods (see the issue described here: http://stats.stackexchange.com/questions/45193/r2jags-does-not-remove-the-burn-in-part-sometimes).

The package is not (yet?) on CRAN. To install it, download the binary .zip file from the 'Downloads' tab of the repository (http://code.kenkellner.com/jagsui/downloads). In R or Rstudio, choose to install a package from a local/archive file and select the downloaded package. The simplejags library requires packages rjags, coda, and lattice.

