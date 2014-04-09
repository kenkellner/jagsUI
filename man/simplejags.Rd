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
  n.chains, n.adapt=0, n.iter, n.burnin=0, n.thin=1, store.data=TRUE)}

\arguments{
  \item{data}{A named list of the data objects required by the model.}
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is 
    \code{NULL}, \code{JAGS} will generate initial values for parameters.}
  \item{parameters.to.save}{Character vector of the names of the
    parameters to save which should be monitored.}
  \item{model.file}{Path to file containing the model written in \code{BUGS} code}
  \item{n.chains}{Number of Markov chains to run}
  \item{n.adapt}{Number of iterations to run in the JAGS adaptive phase. Sometimes JAGS chooses
  not to run these iterations; therefore they are separated from the burn-in in this package.}
  \item{n.iter}{number of total iterations per chain (including burn in).}
  \item{n.burnin}{Length of burn in, i.e. number of iterations to
    discard at the beginning of each chain.}
  \item{n.thin}{Thinning rate. Must be a positive integer}
  \item{store.data}{Option to store the input dataset and initial values in the model output object for future use. Defaults to TRUE.}
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}
