\name{jags.basic}
\docType{class}
% Classes
\alias{jagsUIbasic-class}
% functions
\alias{jags.basic}


\title{Simplified function to call JAGS from R}
\description{
  The \code{jags.basic} function is a simplified version of the \code{jags} function which returns only the \code{mcmc.list}-class output from \code{rjags} rather than a more complex summary (it will also optionally return the model, in which case the output object will be class \code{jagsUIbasic}). This minimal function may be useful when the input dataset or output parameter set are very large and memory intensive.
}

\usage{
jags.basic(data, inits, parameters.to.save, model.file,
  n.chains, n.adapt=100, n.iter, n.burnin=0, n.thin=1,
  modules=c('glm'), parallel=FALSE, n.cores=NULL, DIC=TRUE,
  seed=floor(runif(1,1,10000)), save.model=FALSE, verbose=TRUE)}

\arguments{
  \item{data}{A named list of the data objects required by the model, or a character vector containing the names of the data objects required by the model.}
  \item{inits}{A list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is 
    \code{NULL}, \code{JAGS} will generate initial values for parameters.}
  \item{parameters.to.save}{Character vector of the names of the
    parameters in the model which should be monitored.}
  \item{model.file}{Path to file containing the model written in \code{BUGS} code}
  \item{n.chains}{Number of Markov chains to run.}
  \item{n.adapt}{Number of iterations to run in the JAGS adaptive phase. Sometimes JAGS chooses not to run these iterations; therefore they are separated from the burn-in in this package.}
  \item{n.iter}{Total number of iterations per chain (including burn-in).}
  \item{n.burnin}{Number of iterations at the beginning of the chain to discard (i.e., the burn-in). Does not include the adaptive phase iterations.}
  \item{n.thin}{Thinning rate. Must be a positive integer.}
  \item{modules}{List of JAGS modules to load before analysis. By default only module 'glm' is loaded (in addition to 'basemod' and 'bugs'). To force no additional modules to load, set \code{modules=NULL}.}
  \item{parallel}{If TRUE, run MCMC chains in parallel on multiple CPU cores}
  \item{n.cores}{If parallel=TRUE, specify the number of CPU cores used. Defaults to total available cores or the number of chains, whichever is smaller.}
  \item{DIC}{Option to report deviance values. Defaults to TRUE.}
  \item{seed}{Set a custom seed for the R random number generator and JAGS. The current state of the random number generator is saved in the output object.}
  \item{save.model}{Returns the JAGS model as part of the output object to allow updating the model later. If TRUE, the output object will instead be a list of class \code{jagsUIbasic}. Default is false.}
  \item{verbose}{If set to FALSE, all text output in the console will be suppressed as the function runs (including most warnings).}
}

\details{
See documentation for \code{jags} function for analysis details. The update method will only work if \code{save.model=TRUE}.

}

\value{

An object of class \code{mcmc.list}, if \code{save.model=FALSE}; if \code{save.model=TRUE}, a 2-element list of class \code{jagsUIbasic} containing the mcmc samples and the model.

}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}