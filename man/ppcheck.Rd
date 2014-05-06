\name{pp.check}
% functions
\alias{pp.check}

\title{Posterior Predictive Checks for Bayesian Analyses fit in JAGS}

\usage{
  postPredCheck(x, actual="fit", new="fit.new")}

\arguments{
  \item{x}{A simplejags object generated using the \code{jags} function}
  \item{actual}{The name of the parameter (as a string, in the JAGS model) representing the fit of the actual dataset (e.g. residuals)}
  \item{new}{The name of the corresponding parameter (as a string, in the JAGS model) representing the fit of the new 'ideal' dataset}
}

\description{
  Generates a posterior predictive check plot for a JAGS analysis fit in simplejags().
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}