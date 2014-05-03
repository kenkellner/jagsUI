\name{simplejags}
\docType{class}
% Classes
\alias{simplejags-class}
% functions
\alias{simplejags}


\title{Call JAGS from R}
\description{
  The \code{simplejags} function is a basic wrapper for running JAGS analyses in package \code{rjags}.  
  The user provides a model file, data, initial values (optional), and parameters to save. The function
  compiles the information and sends it to \code{JAGS}, then consolidates and summarizes the MCMC output.
}

\usage{
simplejags(data, inits, parameters.to.save, model.file,
  n.chains, n.adapt=100, n.iter, n.burnin=0, n.thin=1, DIC=TRUE, store.data=FALSE,
  seed=floor(runif(1,1,10000)), bugs.format=FALSE)}

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
  \item{DIC}{Option to report DIC and the estimated number of parameters (pD). Defaults to TRUE.}
  \item{store.data}{Option to store the input dataset and initial values in the model output object for future use. Defaults to FALSE.}
  \item{seed}{Set a custom seed for the R random number generator. The current state of the random number generator is saved in the model object.}
  \item{bugs.format}{Option to print JAGS output in classic R2WinBUGS format. Default is FALSE yielding the default simplejags print method.}
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}
