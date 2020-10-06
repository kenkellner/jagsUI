\name{whiskerplot}
% functions
\alias{whiskerplot}

\title{Whisker plots of parameter posterior distributions}

\usage{
  whiskerplot(x, parameters, quantiles=c(0.025,0.975), zeroline=TRUE, ...)
}

\arguments{
  \item{x}{A jagsUI object}
  \item{parameters}{A vector of names (as characters) of parameters to include in the plot. Parameter names must match parameters included in the model. Calling non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all values of \code{alpha}.}
  \item{quantiles}{A vector with two values specifying the quantile values (lower and upper).}
  \item{zeroline}{If TRUE, a horizontal line at zero is drawn on the plot.}
  \item{\dots}{Additional arguments passed to \link{plot.default}}
}

\description{
  Displays whisker plots for specified parameters on the same plot, with a point at the mean value for the posterior distribution and whiskers extending to the specified quantiles of the distribution.
}

\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}

\examples{  
#Analyze Longley economic data in JAGS
#Number employed as a function of GNP
#See ?jags for a more detailed example

#Get data
data(longley)
gnp <- longley$GNP
employed <- longley$Employed
n <- length(employed)
data <- list(gnp=gnp,employed=employed,n=n)

#Identify filepath of model file
modfile <- tempfile()

writeLines("
model{

  #Likelihood
  for (i in 1:n){ 

    employed[i] ~ dnorm(mu[i], tau)     
    mu[i] <- alpha + beta*gnp[i]

  }
    
  #Priors
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)

}
", con=modfile)

#Set parameters to monitor
params <- c('alpha','beta','sigma','mu')

#Run analysis

out <- jags(data = data,
            inits = NULL,
            parameters.to.save = params,
            model.file = modfile,
            n.chains = 3,
            n.adapt = 100,
            n.iter = 1000,
            n.burnin = 500,
            n.thin = 2)

#Examine output summary

out

#Generate whisker plots

#Plot alpha

whiskerplot(out,parameters=c('alpha'))

#Plot all values of mu

whiskerplot(out,parameters='mu')

#Plot a subset of mu

whiskerplot(out,parameters=c('mu[1]','mu[7]'))

#Plot mu and alpha together

whiskerplot(out,parameters=c('mu','alpha'))

}
