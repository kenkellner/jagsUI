\name{densityplot}
\alias{densityplot}

\title{Density plots of JAGS output}

\usage{
  densityplot(x, parameters=NULL, layout=NULL, ask=NULL)
}

\arguments{
  \item{x}{A jagsUI object}
  \item{parameters}{A vector of names (as characters) of parameters to plot. Parameter names must match parameters included in the model. Calling non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all values of \code{alpha}. If \code{parameters=NULL}, all parameters will be plotted.}
  \item{layout}{A length 2 vector with the number of rows and columns to display in the plot. The default is 3 x 3, or smaller if there are fewer parameters to plot.}
  \item{ask}{If \code{TRUE}, ask user for confirmation before generating each new plot; the default is to ask when output is going to the screen, not when it is going to a file.}
}

\description{
  Displays a series of density plots for posteriors of monitored parameters in a JAGS analysis.
}

\author{ 
  Ken Kellner \email{contact@kenkellner.com}.
}
