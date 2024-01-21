---
output: 
    md_document:
        variant: gfm
---



# jagsUI: Run JAGS from R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/jagsUI)](https://cran.r-project.org/web/packages/jagsUI/index.html)
[![R build status](https://github.com/kenkellner/jagsUI/workflows/R-CMD-check/badge.svg)](https://github.com/kenkellner/jagsUI/actions)
<!-- badges: end -->

This package runs `JAGS` (Just Another Gibbs Sampler) analyses from within `R`. It acts as a wrapper and alternative interface for the functions in the `rjags` package and adds some custom output and graphical options. It also makes running chains in parallel quick and easy.

## Installation

You can install the package from [CRAN](https://cran.r-project.org/web/packages/jagsUI/index.html), or get the development version from Github:

```r
devtools::install_github('kenkellner/jagsUI')
```

You will also need to separately install JAGS, which you can download [here](https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/).

## Example


```r
library(jagsUI)
```

Format data:


```r
jags_data <- list(
  gnp = longley$GNP,
  employed = longley$Employed,
  n = nrow(longley)
)
```

Write BUGS model file:


```r
modfile <- tempfile()
writeLines("
model{

  # Likelihood
  for (i in 1:n){ 
    # Model data
    employed[i] ~ dnorm(mu[i], tau)
    # Calculate linear predictor
    mu[i] <- alpha + beta*gnp[i]
  }
    
  # Priors
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)

}
", con=modfile)
```

Set initial values and parameters to save:


```r
inits <- function(){  
  list(alpha=rnorm(1,0,1),
       beta=rnorm(1,0,1),
       sigma=runif(1,0,3)
  )  
}

params <- c('alpha','beta','sigma')
```

Run JAGS:


```r
out <- jags(data = jags_data,
            inits = inits,
            parameters.to.save = params,
            model.file = modfile,
            n.chains = 3,
            n.adapt = 100,
            n.iter = 1000,
            n.burnin = 500,
            n.thin = 2)
```

```
## 
## Processing function input....... 
## 
## Done. 
##  
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 16
##    Unobserved stochastic nodes: 3
##    Total graph size: 74
## 
## Initializing model
## 
## Adaptive phase, 100 iterations x 3 chains 
## If no progress bar appears JAGS has decided not to adapt 
##  
## 
##  Burn-in phase, 500 iterations x 3 chains 
##  
## 
## Sampling from joint posterior, 500 iterations x 3 chains 
##  
## 
## Calculating statistics....... 
## 
## Done.
```

View output:


```r
out
```

```
## JAGS output for model '/tmp/Rtmp16oToP/file1549f24ce59d9', generated by jagsUI.
## Estimates based on 3 chains of 1000 iterations,
## adaptation = 100 iterations (sufficient),
## burn-in = 500 iterations and thin rate = 2,
## yielding 750 total samples from the joint posterior. 
## MCMC ran for 0.001 minutes at time 2024-01-21 17:14:33.903207.
## 
##            mean    sd   2.5%    50%  97.5% overlap0 f  Rhat n.eff
## alpha    51.817 0.780 50.274 51.832 53.378    FALSE 1 1.002   750
## beta      0.035 0.002  0.031  0.035  0.039    FALSE 1 1.003   750
## sigma     0.729 0.154  0.499  0.705  1.119    FALSE 1 1.008   349
## deviance 33.343 2.886 29.989 32.613 40.721    FALSE 1 1.006   446
## 
## Successful convergence based on Rhat values (all < 1.1). 
## Rhat is the potential scale reduction factor (at convergence, Rhat=1). 
## For each parameter, n.eff is a crude measure of effective sample size. 
## 
## overlap0 checks if 0 falls in the parameter's 95% credible interval.
## f is the proportion of the posterior with the same sign as the mean;
## i.e., our confidence that the parameter is positive or negative.
## 
## DIC info: (pD = var(deviance)/2) 
## pD = 4.2 and DIC = 37.498 
## DIC is an estimate of expected predictive error (lower is better).
```

## Acknowledgments

* Martyn Plummer, developer of the excellent JAGS software package and the `rjags` R package.
* Andrew Gelman, Sibylle Sturtz, Uwe Ligges, Yu-Sung Su, and Masanao Yajima, developers of the `R2WinBUGS` and `R2jags` packages on which the package was originally based.
* Robert Swihart, Marc Kery, Jerome Guelat, Michael Schaub, and Mike Meredith who tested and provided helpful suggestions and improvements for the package.
