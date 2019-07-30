\name{pp.check}
% functions
\alias{pp.check}

\title{Posterior Predictive Checks for Bayesian Analyses fit in JAGS}

\usage{
  pp.check(x, observed, simulated, xlab=NULL, ylab=NULL, main=NULL, ...)}

\arguments{
  \item{x}{A jagsUI object generated using the \code{jags} function}
  \item{observed}{The name of the parameter (as a string, in the JAGS model) representing the fit of the observed data (e.g. residuals)}
  \item{simulated}{The name of the corresponding parameter (as a string, in the JAGS model) representing the fit of the new simulated data}
  \item{xlab}{Customize x-axis label}
  \item{ylab}{Customize y-axis label}
  \item{main}{Customize plot title}
  \item{\dots}{Additional arguments passed to \link{plot.default}}
}

\description{
  A simple interface for generating a posterior predictive check plot for a JAGS analysis fit using jagsUI, based on the posterior distributions of discrepency metrics specified by the user and calculated and returned by JAGS (for example, sums of residuals). The user supplies the name of the discrepancy metric calculated for the real data in the argument \code{actual}, and the corresponding discrepancy for data simulated by the model in argument \code{new}. The posterior distributions of the two parameters will be plotted in X-Y space and a Bayesian p-value calculated.
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

#Write model
#Note calculation of discrepancy stats fit and fit.new
#(sums of residuals)
writeLines("
model{

  #Likelihood
  for (i in 1:n){ 

    employed[i] ~ dnorm(mu[i], tau)     
    mu[i] <- alpha + beta*gnp[i]
    
    res[i] <- employed[i] - mu[i]   
    emp.new[i] ~ dnorm(mu[i], tau)
    res.new[i] <- emp.new[i] - mu[i]

  }
    
  #Priors
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)
  
  #Derived parameters
  fit <- sum(res[])
  fit.new <- sum(res.new[])

}
", con=modfile)

#Set parameters to monitor
params <- c('alpha','beta','sigma','fit','fit.new')

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

#Posterior predictive check plot

pp.check(out, observed = 'fit', simulated = 'fit.new')

}
