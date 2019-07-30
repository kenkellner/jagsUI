\name{jags}
\docType{class}
% Classes
\alias{jagsUI-class}
% functions
\alias{jags}
\alias{jagsUI}


\title{Call JAGS from R}
\description{
  The \code{jags} function is a basic user interface for running JAGS analyses via package \code{rjags} inspired by similar packages like \code{R2WinBUGS}, \code{R2OpenBUGS}, and \code{R2jags}. The user provides a model file, data, initial values (optional), and parameters to save. The function
  compiles the information and sends it to \code{JAGS}, then consolidates and summarizes the MCMC output in an object of class \code{jagsUI}.
}

\usage{
jags(data, inits, parameters.to.save, model.file,
  n.chains, n.adapt=NULL, n.iter, n.burnin=0, n.thin=1,
  modules=c('glm'), factories=NULL, parallel=FALSE, 
  n.cores=NULL, DIC=TRUE, store.data=FALSE,
  codaOnly=FALSE,seed=NULL, bugs.format=FALSE, verbose=TRUE)}

\arguments{
  \item{data}{A named list of the data objects required by the model, or a character vector containing the names of the data objects required by the model.
              Use of a character vector will be deprecated in the next version - switch to using named lists.}
  \item{inits}{A list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is 
    \code{NULL}, \code{JAGS} will generate initial values for parameters.}
  \item{parameters.to.save}{Character vector of the names of the
    parameters in the model which should be monitored.}
  \item{model.file}{Path to file containing the model written in \code{BUGS} code}
  \item{n.chains}{Number of Markov chains to run.}
  \item{n.adapt}{Number of iterations to run in the \code{JAGS} adaptive phase. The default is \code{NULL}, which will result in the function running groups of 100 adaptation iterations (to a max of 10,000) until \code{JAGS} reports adaptation is sufficient. If you set \code{n.adapt} manually, 1000 is the recommended minimum value.}
  \item{n.iter}{Total number of iterations per chain (including burn-in).}
  \item{n.burnin}{Number of iterations at the beginning of the chain to discard (i.e., the burn-in). Does not include the adaptive phase iterations.}
  \item{n.thin}{Thinning rate. Must be a positive integer.}
  \item{modules}{List of JAGS modules to load before analysis. By default only module 'glm' is loaded (in addition to 'basemod' and 'bugs'). To force no additional modules to load, set \code{modules=NULL}.}
  \item{factories}{Optional character vector of factories to enable or disable, in the format <factory> <type> <setting>. For example, to turn \code{TemperedMix} on you would provide \code{'mix::TemperedMix sampler TRUE'} (note spaces between parts). Make sure you have the corresponding modules loaded as well.}
  \item{parallel}{If TRUE, run MCMC chains in parallel on multiple CPU cores}
  \item{n.cores}{If parallel=TRUE, specify the number of CPU cores used. Defaults to total available cores or the number of chains, whichever is smaller.}
  \item{DIC}{Option to report DIC and the estimated number of parameters (pD). Defaults to TRUE.}
  \item{store.data}{Option to store the input dataset and initial values in the output object for future use. Defaults to FALSE.}
  \item{codaOnly}{Optional character vector of parameter names for which you do NOT want to calculate detailed statistics. This may be helpful when you have many output parameters (e.g., predicted values) and you want to save time. For these parameters, only the mean value will be calculated but the mcmc output will still be found in $sims.list and $samples.}
  \item{seed}{Option to set a custom seed to initialize JAGS chains, for reproducibility. Should be an integer.
              This argument will be deprecated in the next version, but you can always set the outside the function yourself.}
  \item{bugs.format}{Option to print JAGS output in classic R2WinBUGS format. Default is FALSE.}
  \item{verbose}{If set to FALSE, all text output in the console will be suppressed as the function runs (including most warnings).}
}

\details{
Basic analysis steps:

\enumerate{
  \item Collect and package data
  \item Write a model file in BUGS language
  \item Set initial values
  \item Specify parameters to monitor
  \item Set MCMC variables and run analysis
  \item Optionally, generate more posterior samples using the \code{update} method.

}

See example below.

}

\value{

An object of class \code{jagsUI}. Notable elements in the output object include:

\item{sims.list}{A list of values sampled from the posterior distributions of each monitored parameter.}
\item{summary}{A summary of various statistics calculated based on model output, in matrix form.}
\item{samples}{The original output object from the \code{rjags} package, as class \code{mcmc.list}.}
\item{model}{The \code{rjags} model object; this will contain multiple elements if \code{parallel=TRUE}.}

}

\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}

\examples{  
#Analyze Longley economic data in JAGS
  
#Number employed as a function of GNP
  
######################################
##   1. Collect and Package Data    ##
######################################
  
#Load data (built into R)
  
data(longley)
head(longley)
  
#Separate data objects
  
gnp <- longley$GNP
employed <- longley$Employed
n <- length(employed)

#Input data objects must be numeric, and must be
#scalars, vectors, matrices, or arrays.
  
#Package together
data <- list(gnp=gnp,employed=employed,n=n)
    
######################################
##      2. Write model file         ##
######################################

#Write a model in the BUGS language

#Generate model file directly in R
#(could also read in existing model file)

#Identify filepath of model file
modfile <- tempfile()

#Write model to file
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
  
######################################
##    3. Initialize Parameters      ##
######################################
  
#Best to generate initial values using function

inits <- function(){  
  list(alpha=rnorm(1,0,1),beta=rnorm(1,0,1),sigma=runif(1,0,3))  
}
  
#In many cases, JAGS can pick initial values automatically;
#you can leave argument inits=NULL to allow this.

######################################
##  4. Set parameters to monitor    ##
######################################

#Choose parameters you want to save output for
#Only parameters in this list will appear in output object
#(deviance is added automatically if DIC=TRUE)

#List must be specified as a character vector

params <- c('alpha','beta','sigma')

######################################
##        5. Run Analysis           ##
######################################

#Call jags function; specify number of chains, number of adaptive iterations,
#the length of the burn-in period, total iterations, and the thin rate.

out <- jags(data = data,
            inits = inits,
            parameters.to.save = params,
            model.file = modfile,
            n.chains = 3,
            n.adapt = 100,
            n.iter = 1000,
            n.burnin = 500,
            n.thin = 2)

#Arguments will be passed to JAGS; you will see progress bars
#and other information

#Examine output summary

out

#Look at output object elements
names(out)

#Plot traces and posterior densities
plot(out)

#Plot traces
traceplot(out)

#Update model another 1000 iterations
out <- update(out,n.iter = 1000)

}
