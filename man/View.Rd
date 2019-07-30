\name{View}
% functions
\alias{View}
\alias{View.jagsUI}
\alias{jags.View}

\title{View a jagsUI output object in a separate window}

\usage{
  
  View(x, title, ...)

  \method{View}{jagsUI}(x, title, digits=3, ...)
}

\arguments{
  \item{x}{A jagsUI object}
  \item{title}{Specify a title for the window.}
  \item{digits}{Number of digits to display after the decimal.}
  \item{...}{Further arguments pass to or from other methods.}
}

\description{
Show an R object in a separate, spreadsheet-style window. This is an S3 generic version of \code{\link[utils]{View}}.

The method will be deprecated in the next version due to constant issues with IDEs especially RStudio. 
It will be replacted with a standalone function \code{jags.View()}, which you can use now.
}

\author{ 
  Ken Kellner \email{contact@kenkellner.com} and Mike Meredith.
}
