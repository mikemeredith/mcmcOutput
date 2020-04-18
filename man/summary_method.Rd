\name{summary.mcmcOutput}
\alias{summary.mcmcOutput}
\alias{print.mcmcOutput}
\title{
Print and summary methods for objects of class \code{mcmcOutput}
}
\description{
\code{summary} generates a data frame with summary and diagnostic statistics for each of the MCMC chains in the \code{mcmcOutput} object, and prints a brief overview to the Console.

\code{print} displays characteristics of the \code{mcmcOutput} object.
}
\usage{
\method{summary}{mcmcOutput}(object, digits=3, median=TRUE, CRItype=c("hdi", "symmetrical"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE, ...)

\method{print}{mcmcOutput}(x, ...)

}
\arguments{
  \item{x, object}{
an object of class \code{mcmcOutput}.
}
  \item{digits}{
the number of digits for rounding of values in the output.
}
  \item{median}{
if TRUE, the median will be included as a column in the data frame produced.
}
  \item{CRItype}{
if \code{hdi}, the credible interval will be a highest density interval; if \code{symmetrical}, a symmetrical CRI will be used generated with \code{quantile}
}
  \item{CRImass}{
the probability mass to include in the credible interval.
}
  \item{Rhat}{
if TRUE, estimates of Rhat will be included; ignored if only 1 chain or < 100 values per chain.
}
  \item{MCEpc}{
if TRUE, estimates of the Monte Carlo standard error as a percentage of the posterior SD will be included; ignored if < 100 values per chain.
}
  \item{n.eff}{
if TRUE, estimates of the effective number of draws allowing for autocorrelation will be included; ignored if < 100 values per chain.
}
  \item{\dots}{
further arguments for the print or summary function.
}
}
\details{

\code{Rhat} is the Brooks-Gelman-Rubin (BGR) (Brooks & Gelman 1998) convergence diagnostic, an 'interval' estimator of the 'potential scale reduction factor' for MCMC output. Similar to the function gelman.diag in coda, but much faster when thousands of parameters are involved and will not cause R to crash.

\code{MCEpc} is calculated using the batch method of Lunn et al (2013, p77); see also Roberts (1996).

\code{n.eff} uses the \code{effectiveSize} in package \code{coda}.
}

\value{
\code{print} returns \code{x} invisibly. \code{summary} returns a data frame with summary and diagnostic statistics for each parameter; mean, SD and a credible interval are always included, other columns if selected.
}
\author{
Mike Meredith.
}

\references{
Brooks, S.P. & Gelman, A. (1998) General methods for monitoring convergence of iterative simulations. \emph{Journal of Computational and Graphical Statistics}, 7, 434-455.

Lunn, D., Jackson, C., Best, N., Thomas, A., & Spiegelhalter, D. (2013) \emph{The BUGS book: a practical introduction to Bayesian analysis}, Chapman and Hall.

Roberts, G.O. (1996). Markov chain concepts related to sampling algorithms. In \emph{Markov Chain Monte Carlo in practice} (eds W.R. Gilks, D.J. Spiegelhalter & S. Richardson). Chapman & Hall, London.
}

\examples{
# TODO
}
\keyword{print}
\keyword{methods}
