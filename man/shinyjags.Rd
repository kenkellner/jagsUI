\name{shinyjags}
% functions
\alias{shinyjags}

\title{Shiny Dashboard for jagsUI Models}

\usage{
  shinyjags(object)
}

\arguments{
  \item{object}{A jagsUI object generated using the \code{jags} or \code{autojags}
    functions}
}

\description{
  A \code{shiny} dashboard for easy examination of jagsUI output and diagnostics.
  Includes a searchable table of parameter estimates and a variety of 
  diagnostic plots. Requires packages \code{shiny}, \code{DT}, and \code{coda}. 
}

\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}

\examples{  

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

#Examine output dashboard
\dontrun{
shinyjags(out)
}

}
