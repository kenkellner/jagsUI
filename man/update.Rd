% functions
\name{update}
\alias{update}
\alias{update.jagsUI}
\alias{update.jagsUIbasic}

\title{Update a JAGS model}
\description{
  This function updates a JAGS model created by created by function \code{jags} 
  in package \code{jagsUI} for a specified number of iterations.
}

\usage{  
  \method{update}{jagsUI}(object, parameters.to.save=NULL, n.adapt=1000, 
  n.iter, n.thin=NULL, no.stats=NULL, quiet=FALSE, na.rm=NULL, ...)}

\arguments{
  \item{object}{A \code{jagsUI} or \code{jagsUIbasic}-class object to update.}
  \item{parameters.to.save}{Character vector of the names of the parameters in 
    the model which should be monitored. Defaults to the saved parameter set 
    from the original model run.}
  \item{n.adapt}{Number of iterations to run in the \code{JAGS} adaptive phase.}
  \item{n.iter}{Total number of iterations per chain (including burn-in).}
  \item{n.thin}{Thinning rate. Must be a positive integer.}
  \item{no.stats}{Optional character vector of parameters that will be saved 
    only in $samples and not otherwise processed by jagsUI.}
  \item{quiet}{If TRUE, all text output in the console will be suppressed 
    as the function runs.}
  \item{na.rm}{If TRUE, columns in the output which contain all NAs will be removed; set to FALSE if chains differ in which columns are all NA. Defaults to the same setting as the original run.}
 \item{...}{Further arguments pass to or from other methods.}
}
\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}
