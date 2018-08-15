% functions
\name{update}
\alias{update}
\alias{update.jagsUI}
\alias{update.jagsUIbasic}

\title{Update a JAGS model}
\description{
  This function updates a JAGS model created by created by function \code{jags} in package \code{jagsUI} for a specified number of iterations.
}

\usage{  
  \method{update}{jagsUI}(object, parameters.to.save=NULL, n.adapt=NULL, 
  n.iter, n.thin=NULL, modules=c('glm'), factories=NULL, DIC=NULL, 
  codaOnly=FALSE, verbose=TRUE, ...)}

\arguments{
  \item{object}{A \code{jagsUI} or \code{jagsUIbasic}-class object to update.}
  \item{parameters.to.save}{Character vector of the names of the
                            parameters in the model which should be monitored. Defaults to the saved parameter set from the original model run.}
  \item{n.adapt}{Number of iterations to run in the \code{JAGS} adaptive phase. The default is \code{NULL}, which will result in the function running groups of 100 adaptation iterations (to a max of 10,000) until \code{JAGS} reports adaptation is sufficient. If you set \code{n.adapt} manually, 1000 is the recommended minimum value.}
  \item{n.iter}{Number of iterations to update for each chain.}
  \item{n.thin}{Thinning rate. Must be a positive integer. Defaults to the thinning rate of the original model run.}
  \item{modules}{List of JAGS modules to load before analysis. By default only module 'glm' is loaded (in addition to 'basemod' and 'bugs'). To force no additional modules to load, set \code{modules=NULL}.}
  \item{factories}{Optional character vector of factories to enable or disable, in the format <factory> <type> <setting>. For example, to turn \code{TemperedMix} on you would provide \code{'mix::TemperedMix sampler TRUE'} (note spaces between parts). Make sure you have the corresponding modules loaded as well.}
  \item{DIC}{Option to report DIC and the estimated number of parameters (pD). Defaults to the same setting as the original model to updated.}
  \item{codaOnly}{Optional character vector of parameter names for which you do NOT want to calculate detailed statistics. This may be helpful when you have many output parameters (e.g., predicted values) and you want to save time. For these parameters, only the mean value will be calculated but the mcmc output will still be found in $sims.list and $samples.}
  \item{verbose}{If set to FALSE, all text output in the console will be suppressed as the function runs (including most warnings).}
 \item{...}{Further arguments pass to or from other methods.}
}
\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}
