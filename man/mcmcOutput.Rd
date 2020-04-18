\name{mcmcOutput-class}
\alias{mcmcOutput-class}
\alias{mcmcOutput}
\alias{mcmcOutput.default}
\alias{mcmcOutput.mcmc.list}
\alias{mcmcOutput.jagsUI}

\title{
Conversion to class \code{mcmcOutput}
}
\description{
Convert output containing MCMC chains to the class \code{mcmcOutput}. The function is generic, with methods for a range of input objects.

\code{print} and \code{summary} methods are available for the class.

This class is still experimental and specs may be changed in future versions.
}
\usage{
mcmcOutput(object, ...)

\method{mcmcOutput}{default}(object, ...)

\method{mcmcOutput}{mcmc.list}(object, header, ...)

\method{mcmcOutput}{jagsUI}(object, header, ...)

}

\arguments{
  \item{object}{
an object containing the MCMC chains; see Details.
}
  \item{header}{
text to use as the header by \code{print} and \code{summary} methods.
}
  \item{\dots}{
named parameters to be passed to other methods (currently not used).
}
}

\details{
\code{mcmcOutput} objects store the output from MCMC estimation runs in a compact and easily accessible format. Several customised extraction methods are available:

  \tabular{ll}{
  \code{$} \tab  : extracts arrays for individual parameters in the same way as a \code{sims.list}. \cr
  \code{[} with 1 index \tab  : returns the selected columns(s). \cr
  \code{[} with 2 indices \tab  : returns the selected row(s) and columns(s). \cr
  \code{[} with 3 indices \tab  : behaves as an iterations x chains x parameters array.}
}

\value{
An object of class \code{mcmcOutput}. This is a matrix with a column for the MCMC chain for each parameter. The first 2 attributes in the list below must be present, the rest are optional but may be used by \code{print} or \code{summary} methods:

  \tabular{ll}{
  \code{nChains} \tab  the number of chains. \cr
  \code{simsList} \tab  a list specifying which columns correspond to each parameter. \cr
  \code{header} \tab text to be displayed as the first line when the object is printed. \cr
  \code{call} \tab the original function call. \cr
  \code{timetaken} \tab an object of class \code{difftime} with the time taken for the MCMC run. }
}

\author{
Mike Meredith.
}

\keyword{methods}

