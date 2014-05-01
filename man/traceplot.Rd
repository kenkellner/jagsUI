\name{traceplot}
% functions
\alias{traceplot}

\title{Traceplots of JAGS output}

\usage{
  traceplot(x)
}

\arguments{
  \item{x}{A simplejags object}
}

\description{
  Displays a series of MCMC iteration plots for each monitored parameter in a JAGS analysis. The calculated Rhat value for each parameter is given in the plot title if there is >1 chains.
}

\author{ 
  Ken Kellner \email{ken@kenkellner.com}.
}