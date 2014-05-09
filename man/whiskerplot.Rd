\name{whiskerplot}
% functions
\alias{whiskerplot}

\title{Whisker plots of parameter posterior distributions}

\usage{
  whiskerplot(x, parameters, quantiles=c(0.025,0.975), zeroline=TRUE)
}

\arguments{
  \item{x}{A simplejags object}
  \item{parameters}{A vector of names (as characters) of parameters to include in the plot. Parameter names must match parameters included in the model. Non-scalar parameters with multiple values (e.g. \code{alpha} where \code{alpha} is a vector of length 5) can be selected/subsetted (e.g. \code{alpha[1:3]}). Calling non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all values of \code{alpha}.}
  \item{quantiles}{A vector with two values specifying the quantile values (lower and upper).}
  \item{zeroline}{If TRUE, a horizontal line at zero is drawn on the plot.}
}

\description{
  Displays whisker plots for specified parameters on the same plot, with a point at the mean value for the posterior distribution and whiskers extending to the specified quantiles of the distribution.
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}