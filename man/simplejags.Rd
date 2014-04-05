\name{simplejags}
\docType{class}
% Classes
\alias{simplejags-class}
% functions
\alias{simplejags}


\title{Run JAGS within R}
\description{
  The \code{simplejags} function is a basic wrapper for running JAGS analyses in package \code{rjags}.  
  The user provides a model file, data, initial values (optional), and parameters to save. The function
  compiles the information and sends it to \code{JAGS}, then consolidates and summarizes the MCMC output.
}

\usage{
jags(data, inits, parameters.to.save, model.file,
  n.chains, n.adapt, n.iter, n.burnin=0, n.thin=1)}

\arguments{
  \item{data}{A named list of the data objects required by the model.}
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \code{BUGS} model,
    \emph{or} a function creating (possibly random) initial values. If inits is 
    \code{NULL}, \code{JAGS} will generate initial values for parameters.}
  \item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored.}
  \item{model.file}{file containing the model written in \code{BUGS} code}
  \item{n.chains}{number of Markov chains}
  \item{n.adapt}{number of iterations to run in the JAGS adaptive phase. Sometimes JAGS chooses
  not to run these iterations; therefore they are separated from the burn-in in this package.}
  \item{n.iter}{number of total iterations per chain (including burn in)}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning}
  \item{n.thin}{thinning rate. Must be a positive integer}
}
