\name{traceplot}
\alias{traceplot}

\title{Traceplots of JAGS output}

\usage{
  traceplot(x, parameters=NULL, Rhat_min=NULL, per_plot=9, ask=NULL)
}

\arguments{
  \item{x}{A jagsUI object}
  \item{parameters}{A vector of names (as characters) of parameters to plot. Parameter names must match parameters included in the model. Calling non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all values of \code{alpha}. If \code{parameters=NULL}, all parameters will be plotted.}
  \item{Rhat_min}{If provided, only plot parameters with Rhat values that exceed the provided value. A good min value to start with is 1.05.}
  \item{per_plot}{Maximum number of parameters to include on each plot.}
  \item{ask}{If \code{TRUE}, ask user for confirmation before generating each new plot; the default is to ask when output is going to the screen, not when it is going to a file.}
}

\description{
  Displays a series of MCMC iteration plots for monitored parameter in a JAGS analysis, along with the calculated Rhat value.
}

\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}
