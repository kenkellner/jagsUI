\name{whiskerplot}
% functions
\alias{whiskerplot}

\title{Whisker plots of parameter posterior distributions}

\usage{
  whiskerplot(x, parameters, quantiles=c(0.025,0.975), zeroline=TRUE)
}

\arguments{
  \item{x}{A simplejags object}
  \item{parameters}{A vector of names (as characters) of parameters to include in the plot. Parameter names must match parameters included in the model. For now, indexed parameters (i.e. alpha[1], alpha[2]) cannot be plotted.}
  \item{quantiles}{A vector with two values specifying the quantile values (lower and upper).}
  \item{zeroline}{If TRUE, a horizontal line at zero is drawn on the plot.}
}

\description{
  Displays whisker plots for specified parameters on the same plot, with a point at the mean value for the posterior distribution and whiskers extending to the specified quantiles of the distribution.
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}