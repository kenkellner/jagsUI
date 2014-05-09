\name{pp.check}
% functions
\alias{pp.check}

\title{Posterior Predictive Checks for Bayesian Analyses fit in JAGS}

\usage{
  pp.check(x, actual, new)}

\arguments{
  \item{x}{A simplejags object generated using the \code{jags} function}
  \item{actual}{The name of the parameter (as a string, in the JAGS model) representing the fit of the actual dataset (e.g. residuals)}
  \item{new}{The name of the corresponding parameter (as a string, in the JAGS model) representing the fit of the new 'ideal' dataset}
}

\description{
  A simple interface for generating a posterior predictive check plot for a JAGS analysis fit in simplejags(), based on the posterior distributions of discrepency metrics specified by the user and calculated and returned by JAGS (for example, sums of residuals). The user supplies the name of the discrepancy metric calculated for the real data in the argument \code{actual}, and the corresponding discrepancy for data simulated by the model in argument \code{new}. The posterior distributions of the two parameters will be plotted in X-Y space and a Bayesian p-value calculated.
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}