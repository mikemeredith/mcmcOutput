\name{sumryList}
\alias{sumryList}
\title{
Summary statistics in a list-of-lists format
}
\description{
\code{sumryList} generates summary and diagnostic statistics for each of the MCMC chains in the \code{mcmcOutput} object and returns them as a list-of-lists, see Value.
}
\usage{
sumryList(object, median=TRUE, CRItype=c("hdi", "symmetrical"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE, ...)

}
\arguments{
  \item{object}{
an object of class \code{mcmcOutput}.
}
  \item{median}{
if TRUE, the median will be included as an element in the list produced.
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
further arguments for the function.
}
}
\details{

\code{Rhat} is the Brooks-Gelman-Rubin (BGR) (Brooks & Gelman 1998) convergence diagnostic, an 'interval' estimator of the 'potential scale reduction factor' for MCMC output. Similar to the function \code{coda::gelman.diag}, but much faster when thousands of parameters are involved and will not cause R to crash.

\code{MCEpc} is calculated using the batch method of Lunn et al (2013, p77); see also Roberts (1996).

\code{n.eff} uses \code{coda::effectiveSize}.
}

\value{
Returns a list with a component for each statistic; each component is itself a list with the values for each parameter. See examples.
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
data(mcmcListExample)
mco <- mcmcOutput(mcmcListExample)
mcos <- sumryList(mco)
str(mcos)
mcos$mean$p
mcos$MCEpc$z
}
\keyword{print}
\keyword{methods}
