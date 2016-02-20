\name{autojags}
\alias{autojags}


\title{Automatically run jagsUI analyses to convergence}
\description{
  The \code{autojags} function runs repeated updates of \code{jagsUI} models, until a specified convergence level (based on the statistic Rhat) or a maximum number of iterations is reached.
}

\usage{
autojags(data, inits, parameters.to.save, model.file,
  n.chains, n.adapt=100, iter.increment=1000, n.burnin=0, n.thin=1,
  save.all.iter=FALSE, modules=c('glm'), parallel=FALSE, n.cores=NULL, DIC=TRUE, 
  store.data=FALSE, codaOnly=FALSE,seed=floor(runif(1,1,10000)), 
  bugs.format=FALSE, Rhat.limit=1.1, max.iter=100000, verbose=TRUE)}

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
  \item{iter.increment}{Number of iterations per model auto-update. Set to larger values when you suspect the model will take a long time to converge.}
  \item{n.burnin}{Number of iterations at the beginning of the chain to discard (i.e., the burn-in). Does not include the adaptive phase iterations.}
  \item{n.thin}{Thinning rate. Must be a positive integer.}
  \item{save.all.iter}{Option to combine MCMC samples from all iterative updates into final posterior (by default only the final iteration is included in the posterior).}
  \item{modules}{List of JAGS modules to load before analysis. By default only module 'glm' is loaded (in addition to 'basemod' and 'bugs'). To force no additional modules to load, set \code{modules=NULL}.}
  \item{parallel}{If TRUE, run MCMC chains in parallel on multiple CPU cores}
  \item{n.cores}{If parallel=TRUE, specify the number of CPU cores used. Defaults to total available cores or the number of chains, whichever is smaller.}
  \item{DIC}{Option to report DIC and the estimated number of parameters (pD). Defaults to TRUE.}
  \item{store.data}{Option to store the input dataset and initial values in the output object for future use. Defaults to FALSE.}
  \item{codaOnly}{Optional character vector of parameter names for which you do NOT want to calculate detailed statistics. This may be helpful when you have many output parameters (e.g., predicted values) and you want to save time. For these parameters, only the mean value will be calculated but the mcmc output will still be found in $sims.list and $samples.}
  \item{seed}{Set a custom seed for the R random number generator and JAGS. The current state of the random number generator is saved in the output object.}
  \item{bugs.format}{Option to print JAGS output in classic R2WinBUGS format. Default is FALSE.}
  \item{Rhat.limit}{Set the desired cutoff point for convergence; when all Rhat values are less than this value the model assumes convergence has been reached and will stop auto-updating.}
  \item{max.iter}{Maximum number of total iterations allowed via auto-update (including burn-in).}
  \item{verbose}{If set to FALSE, all text output in the console will be suppressed as the function runs (including most warnings).}
}

\details{
Usage and output is otherwise identical to the \code{jags} function.
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}