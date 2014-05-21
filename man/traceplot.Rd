\name{traceplot}
% functions
\alias{traceplot}
\alias{traceplot.default}
\alias{traceplot,jagsUI-method}

\title{Traceplots of JAGS output}

\usage{
  \S4method{traceplot}{jagsUI}(x, parameters=NULL,...)
}

\arguments{
  \item{x}{A jagsUI object}
  \item{parameters}{A vector of names (as characters) of parameters to plot. Parameter names must match parameters included in the model. Non-scalar parameters with multiple values (e.g. \code{alpha} where \code{alpha} is a vector of length 5) can be selected/subsetted (e.g. \code{alpha[1:3]}). Calling non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all values of \code{alpha}. If \code{parameters=NULL}, all nodes will be plotted.}
  \item{...}{Further arguments pass to or from other methods.}
}

\description{
  Displays a series of MCMC iteration plots for each monitored parameter in a JAGS analysis. The calculated Rhat value for each parameter is given in the plot title if there is >1 chains.
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}