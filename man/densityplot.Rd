\name{densityplot}
\alias{densityplot}

\title{Density plots of JAGS output}

\usage{
  densityplot(x, parameters=NULL, per_plot=9, ask=TRUE)
}

\arguments{
  \item{x}{A jagsUI object}
  \item{parameters}{A vector of names (as characters) of parameters to plot. Parameter names must match parameters included in the model. Calling non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all values of \code{alpha}. If \code{parameters=NULL}, all parameters will be plotted.}
  \item{per_plot}{Maximum number of parameters to include on each plot.}
  \item{ask}{If \code{TRUE}, ask user for confirmation before generating each new plot.}
}

\description{
  Displays a series of density plots for posteriors of monitored parameters in a JAGS analysis.
}

\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}
