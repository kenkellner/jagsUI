% functions
\name{update}
\alias{update}
\alias{update.jagsUI}
\alias{update,jagsUI-method}

\title{Update a JAGS model}
\description{
  This function updates a JAGS model created by created by function \code{jags} in package \code{jagsUI} for a specified number of iterations.
}

\usage{  
  \method{update}{jagsUI}(object, parameters.to.save=NULL, n.adapt=0, 
  n.iter, n.thin=NULL, modules=c('glm'), seed=as.integer(Sys.time()), 
  codaOnly=FALSE, verbose=TRUE, ...)}

\arguments{
  \item{object}{A \code{jagsUI}-class object to update.}
  \item{parameters.to.save}{Character vector of the names of the
                            parameters in the model which should be monitored. Defaults to the saved parameter set from the original model run.}
  \item{n.adapt}{Number of iterations to run in the JAGS adaptive phase.}
  \item{n.iter}{Number of iterations to update for each chain.}
  \item{n.thin}{Thinning rate. Must be a positive integer. Defaults to the thinning rate of the original model run.}
  \item{modules}{List of JAGS modules to load before analysis. By default only module 'glm' is loaded (in addition to 'basemod' and 'bugs'). To force no additional modules to load, set \code{modules=NULL}.}
  \item{seed}{Set a custom seed for the R random number generator and JAGS.}
  \item{codaOnly}{Optional character vector of parameter names for which you do NOT want to calculate detailed statistics. This may be helpful when you have many output parameters (e.g., predicted values) and you want to save time. For these parameters, only the mean value will be calculated but the mcmc output will still be found in $sims.list and $samples.}
  \item{verbose}{If set to FALSE, all text output in the console will be suppressed as the function runs (including most warnings).}
 \item{...}{Further arguments pass to or from other methods.}
}
\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}